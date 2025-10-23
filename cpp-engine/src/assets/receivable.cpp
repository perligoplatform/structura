#include "assets/receivable.h"
#include <algorithm>
#include <cmath>
#include <stdexcept>
#include <numeric>

namespace Structura {

Receivable::Receivable(const std::string& id, ReceivableType type, Amount face_value,
                      const AccountDebtor& debtor, const InvoiceDetails& invoice,
                      PaymentTerms terms, Date origination_date)
    : receivable_id_(id), receivable_type_(type), status_(Status::Current),
      face_value_(face_value), advance_amount_(0.0), reserve_amount_(0.0),
      advance_rate_(0.0), origination_date_(origination_date),
      expected_collection_date_(invoice.due_date), payment_terms_(terms),
      risk_category_(ReceivableRiskCategory::COMMERCIAL_GRADE),
      is_recourse_(true), has_credit_insurance_(false), insurance_coverage_(0.0),
      account_debtor_(debtor), invoice_details_(invoice),
      discount_rate_(0.0), total_fees_charged_(0.0), net_purchase_price_(0.0),
      partial_payments_received_(0.0), last_payment_amount_(0.0),
      is_diluted_(false), dilution_amount_(0.0), notification_required_(false),
      is_notification_sent_(false) {
    
    if (face_value <= 0) {
        throw std::invalid_argument("Face value must be positive");
    }
    
    if (invoice.invoice_amount != face_value) {
        throw std::invalid_argument("Invoice amount must match face value");
    }
    
    // Set expected collection date based on payment terms if not already set
    if (expected_collection_date_ <= origination_date_) {
        int days_to_add = paymentTermsToDays(terms);
        expected_collection_date_ = origination_date_ + days_to_add;
    }
    
    // Assess initial risk category
    risk_category_ = assessReceivableRisk(debtor, face_value, terms, has_credit_insurance_);
}

void Receivable::processPayment(Amount payment_amount, Date payment_date) {
    if (payment_amount <= 0) {
        throw std::invalid_argument("Payment amount must be positive");
    }
    
    if (status_ == Status::PaidOff || status_ == Status::Defaulted) {
        throw std::runtime_error("Cannot process payment on paid off or defaulted receivable");
    }
    
    Amount remaining_balance = face_value_ - partial_payments_received_;
    
    if (payment_amount >= remaining_balance) {
        // Full payment
        partial_payments_received_ = face_value_;
        status_ = Status::PaidOff;
        actual_collection_date_ = payment_date;
        collection_info_.status = CollectionInfo::CollectionStatus::CURRENT;
        collection_info_.recovered_amount = face_value_;
        collection_info_.recovery_date = payment_date;
        collection_info_.recovery_method = "Full Payment";
    } else {
        // Partial payment
        partial_payments_received_ += payment_amount;
        collection_info_.recovered_amount += payment_amount;
        collection_info_.status = CollectionInfo::CollectionStatus::CURRENT;
    }
    
    last_payment_date_ = payment_date;
    last_payment_amount_ = payment_amount;
    collection_info_.last_contact_date = payment_date;
    
    // Update days past due
    if (payment_date > expected_collection_date_) {
        collection_info_.days_past_due = std::max(0, 
            static_cast<int>(payment_date - expected_collection_date_));
    } else {
        collection_info_.days_past_due = 0;
    }
}

void Receivable::applyDefault(Date default_date) {
    status_ = Status::Defaulted;
    collection_info_.status = CollectionInfo::CollectionStatus::WRITTEN_OFF;
    collection_info_.days_past_due = static_cast<int>(default_date - expected_collection_date_);
    
    // Calculate recovery if any payments were made
    if (partial_payments_received_ > 0) {
        collection_info_.recovered_amount = partial_payments_received_;
        collection_info_.recovery_date = last_payment_date_;
        collection_info_.recovery_method = "Partial Recovery Before Default";
    }
}

void Receivable::applyPartialPayment(Amount amount, Date payment_date) {
    processPayment(amount, payment_date);
}

Balance Receivable::getCurrentBalance() const {
    Amount outstanding = face_value_ - partial_payments_received_;
    
    if (status_ == Status::PaidOff) {
        outstanding = 0.0;
    } else if (status_ == Status::Defaulted) {
        outstanding = face_value_ - collection_info_.recovered_amount;
    }
    
    return outstanding;
}

Amount Receivable::calculateExpectedValue() const {
    if (status_ == Status::PaidOff) {
        return face_value_;
    }
    
    if (status_ == Status::Defaulted) {
        return collection_info_.recovered_amount;
    }
    
    // Calculate expected value based on collection probability
    double collection_probability = calculateCollectionProbability();
    Amount outstanding_balance = face_value_ - partial_payments_received_;
    
    return partial_payments_received_ + (outstanding_balance * collection_probability);
}

void Receivable::resetToOriginal() {
    status_ = Status::Current;
    partial_payments_received_ = 0.0;
    last_payment_amount_ = 0.0;
    total_fees_charged_ = 0.0;
    is_diluted_ = false;
    dilution_amount_ = 0.0;
    
    // Reset collection info
    collection_info_ = CollectionInfo();
    collection_info_.status = CollectionInfo::CollectionStatus::CURRENT;
    
    // Clear actual collection date but keep expected date
    actual_collection_date_ = Date{};
}

void Receivable::purchaseReceivable(Amount purchase_price, Rate advance_rate, Date purchase_date) {
    if (advance_rate < 0.0 || advance_rate > 1.0) {
        throw std::invalid_argument("Advance rate must be between 0 and 1");
    }
    
    purchase_date_ = purchase_date;
    advance_rate_ = advance_rate;
    advance_amount_ = face_value_ * advance_rate;
    reserve_amount_ = face_value_ - advance_amount_;
    net_purchase_price_ = purchase_price;
    
    // Calculate discount rate
    if (face_value_ > 0) {
        discount_rate_ = (face_value_ - purchase_price) / face_value_;
    }
}

void Receivable::setReserveAmount(Amount reserve_amount) {
    if (reserve_amount < 0) {
        throw std::invalid_argument("Reserve amount cannot be negative");
    }
    
    reserve_amount_ = reserve_amount;
    advance_amount_ = face_value_ - reserve_amount_;
    
    if (face_value_ > 0) {
        advance_rate_ = advance_amount_ / face_value_;
    }
}

void Receivable::releaseReserve(Amount release_amount, Date release_date) {
    if (release_amount > reserve_amount_) {
        throw std::invalid_argument("Release amount cannot exceed reserve amount");
    }
    
    reserve_amount_ -= release_amount;
    // Note: Released reserve typically goes to the seller
}

void Receivable::applyDilution(Amount dilution_amount, const std::string& reason, Date dilution_date) {
    if (dilution_amount < 0) {
        throw std::invalid_argument("Dilution amount cannot be negative");
    }
    
    is_diluted_ = true;
    dilution_amount_ += dilution_amount;
    dilution_reason_ = reason;
    
    // Reduce face value by dilution amount
    face_value_ = std::max(0.0, face_value_ - dilution_amount);
}

void Receivable::updateCollectionStatus(CollectionInfo::CollectionStatus status, Date status_date) {
    collection_info_.status = status;
    collection_info_.last_contact_date = status_date;
    
    // Update days past due based on status
    switch (status) {
        case CollectionInfo::CollectionStatus::CURRENT:
            collection_info_.days_past_due = 0;
            break;
        case CollectionInfo::CollectionStatus::PAST_DUE_1_30:
            collection_info_.days_past_due = std::max(collection_info_.days_past_due, 15);
            break;
        case CollectionInfo::CollectionStatus::PAST_DUE_31_60:
            collection_info_.days_past_due = std::max(collection_info_.days_past_due, 45);
            break;
        case CollectionInfo::CollectionStatus::PAST_DUE_61_90:
            collection_info_.days_past_due = std::max(collection_info_.days_past_due, 75);
            break;
        case CollectionInfo::CollectionStatus::PAST_DUE_91_120:
            collection_info_.days_past_due = std::max(collection_info_.days_past_due, 105);
            break;
        case CollectionInfo::CollectionStatus::PAST_DUE_OVER_120:
            collection_info_.days_past_due = std::max(collection_info_.days_past_due, 150);
            break;
        default:
            break;
    }
}

void Receivable::recordCollectionActivity(const std::string& activity, Date activity_date) {
    collection_info_.last_contact_date = activity_date;
    // In a full implementation, this would add to a collection activity log
}

void Receivable::sendToCollection(const std::string& collection_agency, Date referral_date) {
    collection_info_.collection_agency = collection_agency;
    collection_info_.status = CollectionInfo::CollectionStatus::IN_COLLECTION;
    collection_info_.last_contact_date = referral_date;
}

void Receivable::initiateLegalAction(Date action_date, const std::string& legal_firm) {
    collection_info_.status = CollectionInfo::CollectionStatus::LEGAL_ACTION;
    collection_info_.last_contact_date = action_date;
    // In a full implementation, this would record legal firm details
}

void Receivable::addFee(const ReceivableFeeStructure& fee) {
    fee_structure_.push_back(fee);
}

Amount Receivable::calculateTotalFees() const {
    Amount total_fees = 0.0;
    
    for (const auto& fee : fee_structure_) {
        if (fee.is_percentage) {
            total_fees += face_value_ * fee.fee_rate;
        } else {
            total_fees += fee.fee_amount;
        }
    }
    
    return total_fees;
}

void Receivable::chargeFee(ReceivableFeeStructure::FeeType fee_type, Amount amount, Date charge_date) {
    total_fees_charged_ += amount;
}

double Receivable::calculateCollectionProbability() const {
    if (status_ == Status::PaidOff) {
        return 1.0;
    }
    
    if (status_ == Status::Defaulted) {
        return 0.0;
    }
    
    // Base probability on risk category
    double base_probability = 0.95; // Default high probability
    
    switch (risk_category_) {
        case ReceivableRiskCategory::INVESTMENT_GRADE:
        case ReceivableRiskCategory::GOVERNMENT_BACKED:
            base_probability = 0.98;
            break;
        case ReceivableRiskCategory::COMMERCIAL_GRADE:
            base_probability = 0.92;
            break;
        case ReceivableRiskCategory::SUB_INVESTMENT_GRADE:
            base_probability = 0.85;
            break;
        case ReceivableRiskCategory::DISTRESSED:
            base_probability = 0.60;
            break;
        case ReceivableRiskCategory::INSURED:
        case ReceivableRiskCategory::GUARANTEED:
            base_probability = 0.96;
            break;
        case ReceivableRiskCategory::SECURED:
            base_probability = 0.90;
            break;
        case ReceivableRiskCategory::UNSECURED:
            base_probability = 0.88;
            break;
    }
    
    // Adjust for days past due
    if (collection_info_.days_past_due > 0) {
        double aging_factor = std::exp(-collection_info_.days_past_due / 90.0);
        base_probability *= aging_factor;
    }
    
    // Adjust for debtor payment history
    base_probability *= account_debtor_.payment_reliability_score;
    
    // Adjust for disputes
    if (invoice_details_.is_disputed) {
        base_probability *= 0.7; // Reduced probability for disputed invoices
    }
    
    return std::max(0.0, std::min(1.0, base_probability));
}

int Receivable::calculateDaysToCollection() const {
    if (status_ == Status::PaidOff && actual_collection_date_ > Date{}) {
        return static_cast<int>(actual_collection_date_ - origination_date_);
    }
    
    // Estimate based on payment terms and debtor history
    int base_days = paymentTermsToDays(payment_terms_);
    int debtor_average_days = account_debtor_.average_days_to_pay;
    
    // Weight between standard terms and debtor history
    return static_cast<int>(0.3 * base_days + 0.7 * debtor_average_days);
}

double Receivable::assessDilutionRisk() const {
    double dilution_risk = 0.02; // Base 2% dilution risk
    
    // Adjust based on receivable type
    switch (receivable_type_) {
        case ReceivableType::TRADE_RECEIVABLE:
            dilution_risk = 0.03;
            break;
        case ReceivableType::RETAIL_RECEIVABLES:
            dilution_risk = 0.05; // Higher returns/exchanges
            break;
        case ReceivableType::CONSTRUCTION_RECEIVABLES:
            dilution_risk = 0.04; // Change orders, disputes
            break;
        case ReceivableType::GOVERNMENT_RECEIVABLES:
            dilution_risk = 0.01; // Lower dilution risk
            break;
        default:
            dilution_risk = 0.02;
            break;
    }
    
    // Adjust for debtor dispute history
    if (account_debtor_.disputed_invoices_count > 0) {
        dilution_risk *= (1.0 + account_debtor_.disputed_invoices_count * 0.1);
    }
    
    return std::min(0.15, dilution_risk); // Cap at 15%
}

Amount Receivable::calculateExpectedLoss() const {
    double default_probability = 1.0 - calculateCollectionProbability();
    double loss_given_default = 0.6; // Assume 60% loss given default
    
    // Adjust loss given default for security/insurance
    if (has_credit_insurance_) {
        loss_given_default *= (1.0 - std::min(1.0, insurance_coverage_ / face_value_));
    }
    
    if (risk_category_ == ReceivableRiskCategory::SECURED) {
        loss_given_default *= 0.4; // Lower loss for secured receivables
    }
    
    Amount outstanding_balance = face_value_ - partial_payments_received_;
    Amount dilution_loss = outstanding_balance * assessDilutionRisk();
    Amount credit_loss = outstanding_balance * default_probability * loss_given_default;
    
    return dilution_loss + credit_loss;
}

ReceivableRiskCategory Receivable::reassessRiskCategory() const {
    return assessReceivableRisk(account_debtor_, face_value_, payment_terms_, has_credit_insurance_);
}

bool Receivable::verifyInvoice(const std::string& verification_method, Date verification_date) {
    // In a real implementation, this would perform actual verification
    invoice_details_.is_verified = true;
    invoice_details_.verification_date = verification_date;
    invoice_details_.verification_method = verification_method;
    
    return true;
}

void Receivable::flagDispute(Amount disputed_amount, const std::string& reason, Date dispute_date) {
    invoice_details_.is_disputed = true;
    invoice_details_.disputed_amount = disputed_amount;
    invoice_details_.dispute_reason = reason;
    invoice_details_.dispute_date = dispute_date;
}

void Receivable::resolveDispute(Amount resolution_amount, Date resolution_date) {
    invoice_details_.is_disputed = false;
    
    if (resolution_amount < invoice_details_.disputed_amount) {
        // Apply dilution for the difference
        Amount dilution = invoice_details_.disputed_amount - resolution_amount;
        applyDilution(dilution, "Dispute Resolution", resolution_date);
    }
    
    invoice_details_.disputed_amount = 0.0;
    invoice_details_.dispute_reason.clear();
}

void Receivable::sendDebtorNotification(Date notification_date) {
    is_notification_sent_ = true;
    notification_date_ = notification_date;
}

void Receivable::recordLegalCompliance(const std::string& compliance_type, bool is_compliant) {
    // In a full implementation, this would maintain a compliance record
}

bool Receivable::isEligibleForFinancing() const {
    // Check basic eligibility criteria
    if (face_value_ <= 0) return false;
    if (status_ == Status::Defaulted || status_ == Status::PaidOff) return false;
    if (invoice_details_.is_disputed) return false;
    if (!invoice_details_.is_verified) return false;
    
    // Check concentration limits (simplified)
    if (face_value_ > account_debtor_.credit_limit * 0.8) return false;
    
    return true;
}

double Receivable::calculateYieldToMaturity() const {
    if (net_purchase_price_ <= 0 || face_value_ <= 0) return 0.0;
    
    int days_to_maturity = calculateDaysToCollection();
    if (days_to_maturity <= 0) return 0.0;
    
    double return_multiple = face_value_ / net_purchase_price_;
    double annualized_yield = std::pow(return_multiple, 365.0 / days_to_maturity) - 1.0;
    
    return annualized_yield;
}

int Receivable::getDaysOutstanding() const {
    Date current_date = QuantLib::Date::todaysDate();
    return static_cast<int>(current_date - origination_date_);
}

double Receivable::getCollectionEfficiencyRatio() const {
    if (face_value_ <= 0) return 0.0;
    
    return partial_payments_received_ / face_value_;
}

Amount Receivable::calculateNetRealizedValue() const {
    Amount gross_collections = partial_payments_received_;
    Amount total_costs = total_fees_charged_ + collection_info_.collection_costs;
    
    return std::max(0.0, gross_collections - total_costs);
}

double Receivable::calculateDebtorConcentration(const std::vector<Receivable>& portfolio) const {
    Amount total_portfolio_value = 0.0;
    Amount debtor_exposure = 0.0;
    
    for (const auto& receivable : portfolio) {
        total_portfolio_value += receivable.face_value_;
        if (receivable.account_debtor_.debtor_id == account_debtor_.debtor_id) {
            debtor_exposure += receivable.face_value_;
        }
    }
    
    return (total_portfolio_value > 0) ? (debtor_exposure / total_portfolio_value) : 0.0;
}

bool Receivable::exceedsConcentrationLimit(Amount concentration_limit) const {
    return face_value_ > concentration_limit;
}

void Receivable::setCreditInsurance(bool has_insurance, Amount coverage) {
    has_credit_insurance_ = has_insurance;
    insurance_coverage_ = coverage;
    
    // Reassess risk category with insurance
    risk_category_ = reassessRiskCategory();
}

// Utility functions implementation

std::string receivableTypeToString(ReceivableType type) {
    switch (type) {
        case ReceivableType::TRADE_RECEIVABLE: return "Trade Receivable";
        case ReceivableType::INVOICE_FACTORING: return "Invoice Factoring";
        case ReceivableType::ACCOUNTS_RECEIVABLE: return "Accounts Receivable";
        case ReceivableType::SUPPLY_CHAIN_FINANCE: return "Supply Chain Finance";
        case ReceivableType::EXPORT_RECEIVABLES: return "Export Receivables";
        case ReceivableType::GOVERNMENT_RECEIVABLES: return "Government Receivables";
        case ReceivableType::MEDICAL_RECEIVABLES: return "Medical Receivables";
        case ReceivableType::LEGAL_RECEIVABLES: return "Legal Receivables";
        case ReceivableType::CONSTRUCTION_RECEIVABLES: return "Construction Receivables";
        case ReceivableType::FREIGHT_RECEIVABLES: return "Freight Receivables";
        case ReceivableType::ENERGY_RECEIVABLES: return "Energy Receivables";
        case ReceivableType::RETAIL_RECEIVABLES: return "Retail Receivables";
        case ReceivableType::ROYALTY_RECEIVABLES: return "Royalty Receivables";
        case ReceivableType::OTHER_RECEIVABLES: return "Other Receivables";
        default: return "Unknown";
    }
}

std::string paymentTermsToString(PaymentTerms terms) {
    switch (terms) {
        case PaymentTerms::NET_10: return "Net 10";
        case PaymentTerms::NET_15: return "Net 15";
        case PaymentTerms::NET_30: return "Net 30";
        case PaymentTerms::NET_45: return "Net 45";
        case PaymentTerms::NET_60: return "Net 60";
        case PaymentTerms::NET_90: return "Net 90";
        case PaymentTerms::NET_120: return "Net 120";
        case PaymentTerms::CASH_ON_DELIVERY: return "Cash on Delivery";
        case PaymentTerms::PAYMENT_IN_ADVANCE: return "Payment in Advance";
        case PaymentTerms::PROGRESS_BILLING: return "Progress Billing";
        case PaymentTerms::SEASONAL_TERMS: return "Seasonal Terms";
        case PaymentTerms::CUSTOM_TERMS: return "Custom Terms";
        default: return "Unknown";
    }
}

std::string receivableRiskCategoryToString(ReceivableRiskCategory category) {
    switch (category) {
        case ReceivableRiskCategory::INVESTMENT_GRADE: return "Investment Grade";
        case ReceivableRiskCategory::COMMERCIAL_GRADE: return "Commercial Grade";
        case ReceivableRiskCategory::SUB_INVESTMENT_GRADE: return "Sub-Investment Grade";
        case ReceivableRiskCategory::DISTRESSED: return "Distressed";
        case ReceivableRiskCategory::GOVERNMENT_BACKED: return "Government Backed";
        case ReceivableRiskCategory::INSURED: return "Insured";
        case ReceivableRiskCategory::GUARANTEED: return "Guaranteed";
        case ReceivableRiskCategory::SECURED: return "Secured";
        case ReceivableRiskCategory::UNSECURED: return "Unsecured";
        default: return "Unknown";
    }
}

std::string collectionStatusToString(CollectionInfo::CollectionStatus status) {
    switch (status) {
        case CollectionInfo::CollectionStatus::CURRENT: return "Current";
        case CollectionInfo::CollectionStatus::PAST_DUE_1_30: return "Past Due 1-30";
        case CollectionInfo::CollectionStatus::PAST_DUE_31_60: return "Past Due 31-60";
        case CollectionInfo::CollectionStatus::PAST_DUE_61_90: return "Past Due 61-90";
        case CollectionInfo::CollectionStatus::PAST_DUE_91_120: return "Past Due 91-120";
        case CollectionInfo::CollectionStatus::PAST_DUE_OVER_120: return "Past Due Over 120";
        case CollectionInfo::CollectionStatus::IN_COLLECTION: return "In Collection";
        case CollectionInfo::CollectionStatus::LEGAL_ACTION: return "Legal Action";
        case CollectionInfo::CollectionStatus::WRITTEN_OFF: return "Written Off";
        case CollectionInfo::CollectionStatus::RECOVERED: return "Recovered";
        default: return "Unknown";
    }
}

std::string feeTypeToString(ReceivableFeeStructure::FeeType fee_type) {
    switch (fee_type) {
        case ReceivableFeeStructure::FeeType::FACTORING_FEE: return "Factoring Fee";
        case ReceivableFeeStructure::FeeType::DISCOUNT_FEE: return "Discount Fee";
        case ReceivableFeeStructure::FeeType::SERVICING_FEE: return "Servicing Fee";
        case ReceivableFeeStructure::FeeType::COLLECTION_FEE: return "Collection Fee";
        case ReceivableFeeStructure::FeeType::DUE_DILIGENCE_FEE: return "Due Diligence Fee";
        case ReceivableFeeStructure::FeeType::WIRE_TRANSFER_FEE: return "Wire Transfer Fee";
        case ReceivableFeeStructure::FeeType::LEGAL_FEE: return "Legal Fee";
        case ReceivableFeeStructure::FeeType::INSURANCE_FEE: return "Insurance Fee";
        case ReceivableFeeStructure::FeeType::RESERVE_FEE: return "Reserve Fee";
        default: return "Unknown";
    }
}

int paymentTermsToDays(PaymentTerms terms) {
    switch (terms) {
        case PaymentTerms::NET_10: return 10;
        case PaymentTerms::NET_15: return 15;
        case PaymentTerms::NET_30: return 30;
        case PaymentTerms::NET_45: return 45;
        case PaymentTerms::NET_60: return 60;
        case PaymentTerms::NET_90: return 90;
        case PaymentTerms::NET_120: return 120;
        case PaymentTerms::CASH_ON_DELIVERY: return 0;
        case PaymentTerms::PAYMENT_IN_ADVANCE: return -30;
        case PaymentTerms::PROGRESS_BILLING: return 45;
        case PaymentTerms::SEASONAL_TERMS: return 90;
        case PaymentTerms::CUSTOM_TERMS: return 30;
        default: return 30;
    }
}

PaymentTerms daysToPaymentTerms(int days) {
    if (days <= 10) return PaymentTerms::NET_10;
    if (days <= 15) return PaymentTerms::NET_15;
    if (days <= 30) return PaymentTerms::NET_30;
    if (days <= 45) return PaymentTerms::NET_45;
    if (days <= 60) return PaymentTerms::NET_60;
    if (days <= 90) return PaymentTerms::NET_90;
    if (days <= 120) return PaymentTerms::NET_120;
    return PaymentTerms::CUSTOM_TERMS;
}

ReceivableRiskCategory assessReceivableRisk(const AccountDebtor& debtor, Amount invoice_amount,
                                          PaymentTerms terms, bool has_insurance) {
    if (has_insurance) {
        return ReceivableRiskCategory::INSURED;
    }
    
    // Check for government debtor
    if (debtor.industry == "Government" || debtor.industry == "Public Sector") {
        return ReceivableRiskCategory::GOVERNMENT_BACKED;
    }
    
    // Assess based on internal score and payment history
    if (debtor.internal_score >= 90 && debtor.payment_reliability_score >= 0.95) {
        return ReceivableRiskCategory::INVESTMENT_GRADE;
    } else if (debtor.internal_score >= 75 && debtor.payment_reliability_score >= 0.85) {
        return ReceivableRiskCategory::COMMERCIAL_GRADE;
    } else if (debtor.internal_score >= 60 && debtor.payment_reliability_score >= 0.70) {
        return ReceivableRiskCategory::SUB_INVESTMENT_GRADE;
    } else {
        return ReceivableRiskCategory::DISTRESSED;
    }
}

double calculateDefaultProbability(ReceivableRiskCategory risk_category, int days_outstanding) {
    double base_probability = 0.05; // 5% base default rate
    
    switch (risk_category) {
        case ReceivableRiskCategory::INVESTMENT_GRADE:
        case ReceivableRiskCategory::GOVERNMENT_BACKED:
            base_probability = 0.01;
            break;
        case ReceivableRiskCategory::COMMERCIAL_GRADE:
            base_probability = 0.03;
            break;
        case ReceivableRiskCategory::SUB_INVESTMENT_GRADE:
            base_probability = 0.08;
            break;
        case ReceivableRiskCategory::DISTRESSED:
            base_probability = 0.25;
            break;
        case ReceivableRiskCategory::INSURED:
        case ReceivableRiskCategory::GUARANTEED:
            base_probability = 0.005;
            break;
        case ReceivableRiskCategory::SECURED:
            base_probability = 0.02;
            break;
        case ReceivableRiskCategory::UNSECURED:
            base_probability = 0.06;
            break;
    }
    
    // Adjust for aging
    if (days_outstanding > 90) {
        base_probability *= (1.0 + (days_outstanding - 90) / 180.0);
    }
    
    return std::min(0.99, base_probability);
}

Amount calculatePortfolioConcentration(const std::vector<Receivable>& portfolio, 
                                     const std::string& debtor_id) {
    Amount total_value = 0.0;
    Amount debtor_exposure = 0.0;
    
    for (const auto& receivable : portfolio) {
        total_value += receivable.getFaceValue();
        if (receivable.getAccountDebtor().debtor_id == debtor_id) {
            debtor_exposure += receivable.getFaceValue();
        }
    }
    
    return debtor_exposure;
}

double calculatePortfolioWeightedAverageLife(const std::vector<Receivable>& portfolio) {
    Amount total_value = 0.0;
    double weighted_days = 0.0;
    
    for (const auto& receivable : portfolio) {
        Amount face_value = receivable.getFaceValue();
        int days_to_collection = receivable.calculateDaysToCollection();
        
        total_value += face_value;
        weighted_days += face_value * days_to_collection;
    }
    
    return (total_value > 0) ? (weighted_days / total_value / 365.0) : 0.0;
}

Amount calculatePortfolioExpectedLoss(const std::vector<Receivable>& portfolio) {
    Amount total_expected_loss = 0.0;
    
    for (const auto& receivable : portfolio) {
        total_expected_loss += receivable.calculateExpectedLoss();
    }
    
    return total_expected_loss;
}

} // namespace Structura