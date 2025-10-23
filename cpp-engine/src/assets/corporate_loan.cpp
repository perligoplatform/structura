#include "corporate_loan.h"
#include <algorithm>
#include <cmath>
#include <stdexcept>
#include <sstream>
#include <numeric>

namespace Structura {

// Constructor
CorporateLoan::CorporateLoan(const std::string& id, CorporateLoanType type,
                            const CorporateBorrower& borrower, Amount facility_size,
                            const BenchmarkRate& benchmark, Date agreement_date, Date maturity)
    : loan_id_(id), facility_type_(type), borrower_(borrower), status_(Status::Current),
      facility_size_(facility_size), outstanding_balance_(0.0), 
      available_amount_(facility_size), benchmark_(benchmark),
      facility_agreement_date_(agreement_date), maturity_date_(maturity),
      commitment_expiry_date_(maturity), total_commitments_(facility_size),
      is_syndicated_(false), is_covenant_compliant_(true),
      unused_fee_rate_(0.0015), utilization_fee_rate_(0.0025), // Default 15 bps unused, 25 bps utilization
      arrangement_fee_(facility_size * 0.005), commitment_fee_(0.0), // Default 50 bps arrangement fee
      security_type_(SecurityType::SENIOR_SECURED), days_past_due_(0),
      accrued_interest_(0.0), accrued_fees_(0.0), current_risk_rating_(5.0) {
    
    // Set commitment expiry for revolving facilities
    if (type == CorporateLoanType::REVOLVING_CREDIT ||
        type == CorporateLoanType::WORKING_CAPITAL ||
        type == CorporateLoanType::SWING_LINE) {
        // Revolving facilities typically have shorter commitment periods
        commitment_expiry_date_ = Date(agreement_date.year() + 3, 
                                      agreement_date.month(), 
                                      agreement_date.day());
    }
    
    // Initialize with borrower as sole lender if not syndicated
    if (!is_syndicated_) {
        administrative_agent_ = "Self";
    }
}

// Core asset interface implementations
void CorporateLoan::makePayment(Balance payment_amount, Date payment_date) {
    if (payment_amount <= 0.0) {
        throw std::invalid_argument("Payment amount must be positive");
    }
    
    Balance remaining_payment = payment_amount;
    
    // Apply to accrued fees first
    if (accrued_fees_ > 0.0 && remaining_payment > 0.0) {
        Balance fee_payment = std::min(remaining_payment, accrued_fees_);
        accrued_fees_ -= fee_payment;
        remaining_payment -= fee_payment;
    }
    
    // Apply to accrued interest
    if (accrued_interest_ > 0.0 && remaining_payment > 0.0) {
        Balance interest_payment = std::min(remaining_payment, accrued_interest_);
        accrued_interest_ -= interest_payment;
        remaining_payment -= interest_payment;
    }
    
    // Apply to principal
    if (remaining_payment > 0.0) {
        Balance principal_payment = std::min(remaining_payment, outstanding_balance_);
        outstanding_balance_ -= principal_payment;
        
        // For revolving facilities, restore availability
        if (facility_type_ == CorporateLoanType::REVOLVING_CREDIT ||
            facility_type_ == CorporateLoanType::WORKING_CAPITAL ||
            facility_type_ == CorporateLoanType::SWING_LINE) {
            available_amount_ += principal_payment;
        }
    }
    
    // Record payment
    RepaymentRecord record;
    record.repayment_date = payment_date;
    record.repayment_amount = payment_amount;
    record.is_mandatory = false; // Assume voluntary unless specified
    record.prepayment_penalty = 0.0; // Corporate loans typically no prepayment penalty
    repayment_history_.push_back(record);
    
    last_payment_date_ = payment_date;
    
    // Reset delinquency
    if (days_past_due_ > 0) {
        days_past_due_ = 0;
    }
    
    // Check if loan is paid off
    if (outstanding_balance_ <= 0.01) {
        outstanding_balance_ = 0.0;
        status_ = Status::PaidOff;
    }
}

void CorporateLoan::applyInterestAccrual(Date accrual_date) {
    if (outstanding_balance_ <= 0.0 || status_ == Status::PaidOff) {
        return;
    }
    
    // Calculate daily interest
    Rate all_in_rate = calculateAllInRate();
    double daily_rate = all_in_rate / 365.25;
    Balance daily_interest = outstanding_balance_ * daily_rate;
    
    accrued_interest_ += daily_interest;
    
    // Accrue fees on unused commitment
    if (available_amount_ > 0.0) {
        double daily_unused_fee_rate = unused_fee_rate_ / 365.25;
        Balance daily_unused_fee = available_amount_ * daily_unused_fee_rate;
        accrued_fees_ += daily_unused_fee;
    }
    
    // Accrue utilization fee if applicable
    double utilization_rate = getUtilizationRate();
    if (utilization_rate > 0.33) { // Typical utilization fee threshold
        double daily_util_fee_rate = utilization_fee_rate_ / 365.25;
        Balance daily_util_fee = outstanding_balance_ * daily_util_fee_rate;
        accrued_fees_ += daily_util_fee;
    }
}

void CorporateLoan::applyDefault(Date default_date) {
    status_ = Status::Defaulted;
    days_past_due_ = 90; // Typically 90+ days for corporate loans
    
    // Mark all covenants as potentially breached
    is_covenant_compliant_ = false;
    
    // Accelerate maturity if not already accelerated
    if (maturity_date_ > default_date) {
        maturity_date_ = default_date;
    }
    
    // Cease availability for revolving facilities
    available_amount_ = 0.0;
}

void CorporateLoan::applyPrepayment(Balance amount, Date payment_date) {
    if (amount <= 0.0) {
        throw std::invalid_argument("Prepayment amount must be positive");
    }
    
    // Corporate loans typically allow prepayment without penalty
    Balance prepayment_amount = std::min(amount, outstanding_balance_);
    outstanding_balance_ -= prepayment_amount;
    
    // For revolving facilities, restore availability
    if (facility_type_ == CorporateLoanType::REVOLVING_CREDIT ||
        facility_type_ == CorporateLoanType::WORKING_CAPITAL ||
        facility_type_ == CorporateLoanType::SWING_LINE) {
        available_amount_ += prepayment_amount;
    }
    
    // Record as voluntary prepayment
    RepaymentRecord record;
    record.repayment_date = payment_date;
    record.repayment_amount = prepayment_amount;
    record.is_mandatory = false;
    record.prepayment_penalty = 0.0;
    repayment_history_.push_back(record);
    
    if (outstanding_balance_ <= 0.01) {
        outstanding_balance_ = 0.0;
        status_ = Status::PaidOff;
    }
}

Balance CorporateLoan::getCurrentBalance() const {
    return outstanding_balance_ + accrued_interest_ + accrued_fees_;
}

Rate CorporateLoan::getCurrentRate() const {
    return calculateAllInRate();
}

void CorporateLoan::resetToOriginal() {
    outstanding_balance_ = 0.0;
    available_amount_ = facility_size_;
    accrued_interest_ = 0.0;
    accrued_fees_ = 0.0;
    status_ = Status::Current;
    days_past_due_ = 0;
    is_covenant_compliant_ = true;
    drawdown_history_.clear();
    repayment_history_.clear();
    fee_payments_.clear();
}

// Corporate loan specific operations
void CorporateLoan::drawdown(Amount amount, Date drawdown_date, const std::string& purpose) {
    if (amount <= 0.0) {
        throw std::invalid_argument("Drawdown amount must be positive");
    }
    
    if (amount > available_amount_) {
        throw std::runtime_error("Drawdown amount exceeds available commitment");
    }
    
    if (drawdown_date > commitment_expiry_date_) {
        throw std::runtime_error("Cannot drawdown after commitment expiry");
    }
    
    // Process drawdown
    outstanding_balance_ += amount;
    available_amount_ -= amount;
    
    // Record drawdown
    DrawdownRecord record;
    record.drawdown_date = drawdown_date;
    record.drawdown_amount = amount;
    record.purpose = purpose;
    record.applicable_margin = benchmark_.margin;
    drawdown_history_.push_back(record);
    
    // Allocate to syndicate members if syndicated
    if (is_syndicated_) {
        for (auto& member : syndicate_) {
            Amount member_allocation = calculateMemberAllocation(member.institution_id, amount);
            member.funded_amount += member_allocation;
        }
    }
}

void CorporateLoan::repay(Amount amount, Date repayment_date, bool is_mandatory) {
    if (amount <= 0.0) {
        throw std::invalid_argument("Repayment amount must be positive");
    }
    
    Amount actual_repayment = std::min(amount, outstanding_balance_);
    outstanding_balance_ -= actual_repayment;
    
    // For revolving facilities, restore availability
    if (facility_type_ == CorporateLoanType::REVOLVING_CREDIT ||
        facility_type_ == CorporateLoanType::WORKING_CAPITAL) {
        available_amount_ += actual_repayment;
    }
    
    // Record repayment
    RepaymentRecord record;
    record.repayment_date = repayment_date;
    record.repayment_amount = actual_repayment;
    record.is_mandatory = is_mandatory;
    record.prepayment_penalty = 0.0;
    repayment_history_.push_back(record);
    
    // Update syndicate funding
    if (is_syndicated_) {
        for (auto& member : syndicate_) {
            Amount member_allocation = calculateMemberAllocation(member.institution_id, actual_repayment);
            member.funded_amount -= member_allocation;
        }
    }
}

void CorporateLoan::revolve(Amount repay_amount, Amount redraw_amount, Date transaction_date) {
    if (facility_type_ != CorporateLoanType::REVOLVING_CREDIT &&
        facility_type_ != CorporateLoanType::WORKING_CAPITAL) {
        throw std::runtime_error("Revolve operation only available for revolving facilities");
    }
    
    // Process repayment first
    if (repay_amount > 0.0) {
        repay(repay_amount, transaction_date, false);
    }
    
    // Process redraw
    if (redraw_amount > 0.0) {
        drawdown(redraw_amount, transaction_date, "Revolving redraw");
    }
}

// Syndication management
void CorporateLoan::addSyndicateMember(const SyndicateMember& member) {
    syndicate_.push_back(member);
    total_commitments_ += member.commitment_amount;
    is_syndicated_ = true;
    
    // Set administrative agent if this is the first lead role
    if ((member.role == SyndicationRole::ADMINISTRATIVE_AGENT ||
         member.role == SyndicationRole::LEAD_ARRANGER) &&
        administrative_agent_ == "Self") {
        administrative_agent_ = member.institution_name;
    }
}

void CorporateLoan::removeSyndicateMember(const std::string& institution_id) {
    auto it = std::find_if(syndicate_.begin(), syndicate_.end(),
        [&institution_id](const SyndicateMember& member) {
            return member.institution_id == institution_id;
        });
    
    if (it != syndicate_.end()) {
        total_commitments_ -= it->commitment_amount;
        syndicate_.erase(it);
        
        if (syndicate_.empty()) {
            is_syndicated_ = false;
            administrative_agent_ = "Self";
        }
    }
}

Amount CorporateLoan::calculateMemberAllocation(const std::string& institution_id, Amount transaction_amount) const {
    auto it = std::find_if(syndicate_.begin(), syndicate_.end(),
        [&institution_id](const SyndicateMember& member) {
            return member.institution_id == institution_id;
        });
    
    if (it != syndicate_.end() && total_commitments_ > 0.0) {
        double allocation_percentage = it->commitment_amount / total_commitments_;
        return transaction_amount * allocation_percentage;
    }
    
    return 0.0;
}

// Covenant tracking and testing
void CorporateLoan::addFinancialCovenant(const FinancialCovenant& covenant) {
    financial_covenants_.push_back(covenant);
}

void CorporateLoan::updateCovenantValue(FinancialCovenant::CovenantType type, double new_value, Date test_date) {
    auto it = std::find_if(financial_covenants_.begin(), financial_covenants_.end(),
        [type](const FinancialCovenant& covenant) {
            return covenant.type == type;
        });
    
    if (it != financial_covenants_.end()) {
        it->current_value = new_value;
        it->test_date = test_date;
        
        // Test covenant compliance
        bool in_compliance = false;
        switch (type) {
            case FinancialCovenant::CovenantType::DEBT_TO_EBITDA:
            case FinancialCovenant::CovenantType::DEBT_TO_EQUITY:
            case FinancialCovenant::CovenantType::LOAN_TO_VALUE:
                in_compliance = (new_value <= it->threshold_value);
                it->cushion = it->threshold_value - new_value;
                break;
            case FinancialCovenant::CovenantType::EBITDA_TO_INTEREST:
            case FinancialCovenant::CovenantType::FIXED_CHARGE_COVERAGE:
            case FinancialCovenant::CovenantType::CURRENT_RATIO:
            case FinancialCovenant::CovenantType::DEBT_SERVICE_COVERAGE:
            case FinancialCovenant::CovenantType::CASH_FLOW_COVERAGE:
                in_compliance = (new_value >= it->threshold_value);
                it->cushion = new_value - it->threshold_value;
                break;
            case FinancialCovenant::CovenantType::TANGIBLE_NET_WORTH:
                in_compliance = (new_value >= it->threshold_value);
                it->cushion = new_value - it->threshold_value;
                break;
            default:
                in_compliance = true; // Default to compliant for other types
        }
        
        it->is_in_compliance = in_compliance;
        last_covenant_test_date_ = test_date;
    }
}

bool CorporateLoan::testAllCovenants(Date test_date) {
    bool all_compliant = true;
    
    for (auto& covenant : financial_covenants_) {
        // Test based on maintenance vs incurrence covenant
        if (covenant.is_maintenance) {
            // Maintenance covenants tested quarterly
            if (covenant.current_value != 0.0) { // Has been updated
                if (!covenant.is_in_compliance) {
                    all_compliant = false;
                }
            }
        }
    }
    
    is_covenant_compliant_ = all_compliant;
    return all_compliant;
}

std::vector<FinancialCovenant> CorporateLoan::getBreachedCovenants() const {
    std::vector<FinancialCovenant> breached;
    
    for (const auto& covenant : financial_covenants_) {
        if (!covenant.is_in_compliance) {
            breached.push_back(covenant);
        }
    }
    
    return breached;
}

// Interest rate management
void CorporateLoan::resetInterestRate(Date reset_date) {
    benchmark_.reset_date = reset_date;
    // In practice, would fetch new benchmark rate from market data
    // For now, keeping current rate
}

Rate CorporateLoan::calculateAllInRate() const {
    Rate base_rate = benchmark_.current_rate + benchmark_.margin;
    
    // Apply floor and cap
    base_rate = std::max(base_rate, benchmark_.floor_rate);
    base_rate = std::min(base_rate, benchmark_.cap_rate);
    
    return base_rate;
}

// Fee calculations
Amount CorporateLoan::calculateUnusedFee(Date calculation_date) const {
    if (available_amount_ <= 0.0) {
        return 0.0;
    }
    
    // Unused fee typically calculated on available commitment
    return available_amount_ * unused_fee_rate_;
}

Amount CorporateLoan::calculateUtilizationFee(Date calculation_date) const {
    double utilization_rate = getUtilizationRate();
    
    // Utilization fee kicks in above certain threshold (typically 33% or 50%)
    if (utilization_rate > 0.33) {
        return outstanding_balance_ * utilization_fee_rate_;
    }
    
    return 0.0;
}

// Financial reporting and analytics
double CorporateLoan::getUtilizationRate() const {
    if (facility_size_ <= 0.0) {
        return 0.0;
    }
    
    return outstanding_balance_ / facility_size_;
}

Amount CorporateLoan::getAvailableCommitment() const {
    return available_amount_;
}

double CorporateLoan::getCurrentMargin() const {
    return benchmark_.margin;
}

Amount CorporateLoan::calculatePresentValue(Rate discount_rate) const {
    // Simplified PV calculation - would be more sophisticated in practice
    if (discount_rate <= 0.0) {
        return getCurrentBalance();
    }
    
    // Estimate remaining term
    double years_to_maturity = 1.0; // Simplified
    
    return getCurrentBalance() / std::pow(1.0 + discount_rate, years_to_maturity);
}

// Risk management
double CorporateLoan::calculateExpectedLoss() const {
    // EL = PD × EAD × LGD
    double pd = borrower_.probability_of_default;
    Amount ead = borrower_.exposure_at_default > 0 ? borrower_.exposure_at_default : getCurrentBalance();
    double lgd = borrower_.loss_given_default;
    
    return pd * ead * lgd;
}

bool CorporateLoan::isWatchListCredit() const {
    return current_risk_rating_ >= 6.0 || // Internal rating threshold
           !is_covenant_compliant_ ||
           days_past_due_ > 30;
}

bool CorporateLoan::isNonPerformingLoan() const {
    return status_ == Status::Defaulted ||
           days_past_due_ >= 90;
}

// Utility function implementations
std::string corporateLoanTypeToString(CorporateLoanType type) {
    switch (type) {
        case CorporateLoanType::TERM_LOAN_A: return "Term Loan A";
        case CorporateLoanType::TERM_LOAN_B: return "Term Loan B";
        case CorporateLoanType::REVOLVING_CREDIT: return "Revolving Credit Facility";
        case CorporateLoanType::BRIDGE_LOAN: return "Bridge Loan";
        case CorporateLoanType::ACQUISITION_FINANCING: return "Acquisition Financing";
        case CorporateLoanType::WORKING_CAPITAL: return "Working Capital Facility";
        case CorporateLoanType::CAPITAL_EXPENDITURE: return "Capital Expenditure Facility";
        case CorporateLoanType::REAL_ESTATE_LOAN: return "Commercial Real Estate Loan";
        case CorporateLoanType::EQUIPMENT_FINANCING: return "Equipment Financing";
        case CorporateLoanType::CONSTRUCTION_LOAN: return "Construction Loan";
        case CorporateLoanType::LETTER_OF_CREDIT: return "Letter of Credit";
        case CorporateLoanType::SWING_LINE: return "Swing Line Facility";
        case CorporateLoanType::DELAYED_DRAW: return "Delayed Draw Term Loan";
        case CorporateLoanType::MULTICURRENCY: return "Multi-Currency Facility";
        default: return "Unknown";
    }
}

std::string syndicationRoleToString(SyndicationRole role) {
    switch (role) {
        case SyndicationRole::ADMINISTRATIVE_AGENT: return "Administrative Agent";
        case SyndicationRole::COLLATERAL_AGENT: return "Collateral Agent";
        case SyndicationRole::LEAD_ARRANGER: return "Lead Arranger";
        case SyndicationRole::CO_AGENT: return "Co-Administrative Agent";
        case SyndicationRole::DOCUMENTATION_AGENT: return "Documentation Agent";
        case SyndicationRole::SYNDICATION_AGENT: return "Syndication Agent";
        case SyndicationRole::LENDER: return "Lender";
        case SyndicationRole::PARTICIPANT: return "Participant";
        default: return "Unknown";
    }
}

std::string industryClassificationToString(IndustryClassification industry) {
    switch (industry) {
        case IndustryClassification::AEROSPACE_DEFENSE: return "Aerospace & Defense";
        case IndustryClassification::AUTOMOTIVE: return "Automotive & Transportation";
        case IndustryClassification::BUSINESS_SERVICES: return "Business Services";
        case IndustryClassification::CHEMICALS: return "Chemicals, Plastics & Rubber";
        case IndustryClassification::CONSUMER_GOODS: return "Consumer Goods";
        case IndustryClassification::CONSTRUCTION: return "Construction & Building";
        case IndustryClassification::ENERGY: return "Energy: Oil & Gas";
        case IndustryClassification::ENVIRONMENTAL: return "Environmental Industries";
        case IndustryClassification::FINANCIAL_SERVICES: return "Financial Services";
        case IndustryClassification::FOOD_BEVERAGE: return "Food, Beverage & Tobacco";
        case IndustryClassification::FORESTRY_PAPER: return "Forestry & Paper";
        case IndustryClassification::HEALTHCARE: return "Healthcare & Pharmaceuticals";
        case IndustryClassification::HIGH_TECH: return "High Tech Industries";
        case IndustryClassification::HOTEL_GAMING: return "Hotel, Gaming & Leisure";
        case IndustryClassification::MANUFACTURING: return "Manufacturing";
        case IndustryClassification::MEDIA_TELECOM: return "Media & Telecommunications";
        case IndustryClassification::METALS_MINING: return "Metals & Mining";
        case IndustryClassification::REAL_ESTATE: return "Real Estate";
        case IndustryClassification::RETAIL: return "Retail";
        case IndustryClassification::SERVICES: return "Services";
        case IndustryClassification::UTILITIES: return "Utilities";
        case IndustryClassification::OTHER: return "Other";
        default: return "Unknown";
    }
}

// Covenant calculation helpers
double calculateDebtToEBITDA(Amount total_debt, Amount ebitda) {
    if (ebitda <= 0.0) {
        return 999.0; // Return very high ratio if EBITDA is zero or negative
    }
    return total_debt / ebitda;
}

double calculateInterestCoverage(Amount ebitda, Amount interest_expense) {
    if (interest_expense <= 0.0) {
        return 999.0; // Perfect coverage if no interest expense
    }
    return ebitda / interest_expense;
}

double calculateFixedChargeCoverage(Amount ebitda, Amount fixed_charges) {
    if (fixed_charges <= 0.0) {
        return 999.0; // Perfect coverage if no fixed charges
    }
    return ebitda / fixed_charges;
}

} // namespace Structura