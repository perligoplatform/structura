#include "consumer_installment.h"
#include <algorithm>
#include <cmath>
#include <stdexcept>
#include <sstream>

namespace Structura {

// Constructor
ConsumerInstallment::ConsumerInstallment(const std::string& id, ConsumerInstallmentType type,
                                        LoanPurpose purpose, const ConsumerBorrower& borrower,
                                        Amount principal, Rate interest_rate, int term_months,
                                        Date origination_date)
    : loan_id_(id), loan_type_(type), loan_purpose_(purpose), borrower_(borrower),
      status_(Status::Current), original_principal_(principal), current_principal_(principal),
      accrued_interest_(0.0), original_interest_rate_(interest_rate),
      current_interest_rate_(interest_rate), original_term_months_(term_months),
      remaining_term_months_(term_months), origination_date_(origination_date),
      is_in_promotional_period_(false), payments_made_(0), consecutive_on_time_payments_(0),
      missed_payments_(0), total_payments_made_(0.0), days_delinquent_(0),
      late_fees_assessed_(0.0), collection_costs_(0.0), is_in_hardship_program_(false),
      underwriting_decision_(UnderwritingDecision::DecisionType::APPROVED, "Standard Approval"),
      debt_to_income_ratio_(0.0), payment_to_income_ratio_(0.0),
      monthly_disposable_income_(0.0), servicer_name_("Default Servicer"),
      is_refinanced_(false), is_modified_(false), down_payment_(0.0) {
    
    // Calculate scheduled payment
    scheduled_payment_ = calculateMonthlyPayment();
    
    // Set payment dates
    first_payment_date_ = Date(origination_date.dayOfMonth(), 
                              static_cast<QuantLib::Month>(origination_date.month() + 1), 
                              origination_date.year());
    next_payment_date_ = first_payment_date_;
    maturity_date_ = Date(origination_date.dayOfMonth(), 
                         static_cast<QuantLib::Month>(origination_date.month() + term_months), 
                         origination_date.year());
    
    // Calculate DTI ratios
    if (borrower_.monthly_income > 0.0) {
        debt_to_income_ratio_ = (borrower_.monthly_debt_payments + scheduled_payment_) / borrower_.monthly_income;
        payment_to_income_ratio_ = scheduled_payment_ / borrower_.monthly_income;
        monthly_disposable_income_ = borrower_.monthly_income - borrower_.monthly_debt_payments - 
                                   borrower_.monthly_housing_payment - scheduled_payment_;
    }
    
    // Set underwriting decision details
    underwriting_decision_.approved_amount = principal;
    underwriting_decision_.approved_rate = interest_rate;
    underwriting_decision_.approved_term = term_months;
    underwriting_decision_.risk_score = calculateRiskScore();
}

// Core asset interface implementations
void ConsumerInstallment::makePayment(Balance payment_amount, Date payment_date) {
    if (payment_amount <= 0.0) {
        throw std::invalid_argument("Payment amount must be positive");
    }
    
    processRegularPayment(payment_amount, payment_date);
}

void ConsumerInstallment::applyInterestAccrual(Date accrual_date) {
    if (current_principal_ <= 0.0 || status_ == Status::PaidOff) {
        return;
    }
    
    // Handle promotional period
    Rate applicable_rate = current_interest_rate_;
    if (is_in_promotional_period_ && promotional_info_.has_value()) {
        applicable_rate = promotional_info_->promotional_rate;
    }
    
    // Calculate daily interest
    double daily_rate = applicable_rate / 365.25;
    Balance daily_interest = current_principal_ * daily_rate;
    
    accrued_interest_ += daily_interest;
}

void ConsumerInstallment::applyDefault(Date default_date) {
    status_ = Status::Defaulted;
    first_delinquent_date_ = default_date;
    days_delinquent_ = 120; // Typically 120+ days for consumer loans
    
    // Add collection costs (typical percentage for consumer loans)
    collection_costs_ = current_principal_ * 0.15; // 15% collection costs
    
    // Exit any hardship programs
    if (is_in_hardship_program_) {
        is_in_hardship_program_ = false;
    }
}

void ConsumerInstallment::applyPrepayment(Balance amount, Date payment_date) {
    if (amount <= 0.0) {
        throw std::invalid_argument("Prepayment amount must be positive");
    }
    
    Balance remaining_amount = amount;
    
    // Pay accrued interest first
    if (accrued_interest_ > 0.0 && remaining_amount > 0.0) {
        Balance interest_payment = std::min(remaining_amount, accrued_interest_);
        accrued_interest_ -= interest_payment;
        remaining_amount -= interest_payment;
    }
    
    // Pay principal
    if (remaining_amount > 0.0) {
        Balance principal_payment = std::min(remaining_amount, current_principal_);
        current_principal_ -= principal_payment;
        
        // Recalculate remaining terms
        if (current_principal_ > 0.01) {
            // Approximate remaining terms based on remaining balance
            remaining_term_months_ = static_cast<int>(current_principal_ / scheduled_payment_);
        } else {
            current_principal_ = 0.0;
            remaining_term_months_ = 0;
            status_ = Status::PaidOff;
        }
    }
    
    last_payment_date_ = payment_date;
    last_payment_amount_ = amount;
    total_payments_made_ += amount;
    
    // Reset delinquency
    if (days_delinquent_ > 0) {
        days_delinquent_ = 0;
    }
}

Balance ConsumerInstallment::getCurrentBalance() const {
    return current_principal_ + accrued_interest_;
}

Rate ConsumerInstallment::getCurrentRate() const {
    if (is_in_promotional_period_ && promotional_info_.has_value()) {
        return promotional_info_->promotional_rate;
    }
    return current_interest_rate_;
}

void ConsumerInstallment::resetToOriginal() {
    current_principal_ = original_principal_;
    accrued_interest_ = 0.0;
    current_interest_rate_ = original_interest_rate_;
    remaining_term_months_ = original_term_months_;
    status_ = Status::Current;
    payments_made_ = 0;
    consecutive_on_time_payments_ = 0;
    missed_payments_ = 0;
    total_payments_made_ = 0.0;
    days_delinquent_ = 0;
    late_fees_assessed_ = 0.0;
    collection_costs_ = 0.0;
    is_in_hardship_program_ = false;
    is_in_promotional_period_ = false;
    scheduled_payment_ = calculateMonthlyPayment();
}

// Consumer installment specific operations
void ConsumerInstallment::processRegularPayment(Amount payment_amount, Date payment_date) {
    if (status_ == Status::PaidOff) {
        return;
    }
    
    Balance remaining_payment = payment_amount;
    
    // Apply to late fees first
    if (late_fees_assessed_ > 0.0 && remaining_payment > 0.0) {
        Balance fee_payment = std::min(remaining_payment, late_fees_assessed_);
        late_fees_assessed_ -= fee_payment;
        remaining_payment -= fee_payment;
    }
    
    // Apply to collection costs
    if (collection_costs_ > 0.0 && remaining_payment > 0.0) {
        Balance collection_payment = std::min(remaining_payment, collection_costs_);
        collection_costs_ -= collection_payment;
        remaining_payment -= collection_payment;
    }
    
    // Apply to accrued interest
    if (accrued_interest_ > 0.0 && remaining_payment > 0.0) {
        Balance interest_payment = std::min(remaining_payment, accrued_interest_);
        accrued_interest_ -= interest_payment;
        remaining_payment -= interest_payment;
    }
    
    // Apply to principal
    if (remaining_payment > 0.0) {
        Balance principal_payment = std::min(remaining_payment, current_principal_);
        current_principal_ -= principal_payment;
        remaining_term_months_--;
    }
    
    // Update payment tracking
    payments_made_++;
    total_payments_made_ += payment_amount;
    last_payment_date_ = payment_date;
    last_payment_amount_ = payment_amount;
    
    // Check if payment is on time (within grace period)
    if (payment_date <= next_payment_date_) {
        consecutive_on_time_payments_++;
        
        // Reset delinquency
        if (days_delinquent_ > 0) {
            days_delinquent_ = 0;
        }
    } else {
        consecutive_on_time_payments_ = 0;
        missed_payments_++;
    }
    
    // Update next payment date
    next_payment_date_ = Date(next_payment_date_.dayOfMonth(), 
                             static_cast<QuantLib::Month>(next_payment_date_.month() + 1), 
                             next_payment_date_.year());
    
    // Check if loan is paid off
    if (current_principal_ <= 0.01) {
        current_principal_ = 0.0;
        accrued_interest_ = 0.0;
        status_ = Status::PaidOff;
    }
}

void ConsumerInstallment::processPartialPayment(Amount payment_amount, Date payment_date) {
    if (payment_amount < scheduled_payment_ * 0.1) { // Less than 10% of scheduled payment
        throw std::invalid_argument("Payment too small to process");
    }
    
    // Process as regular payment but don't advance payment schedule
    Balance remaining_payment = payment_amount;
    
    // Apply to fees and interest first, then principal
    if (late_fees_assessed_ > 0.0) {
        Balance fee_payment = std::min(remaining_payment, late_fees_assessed_);
        late_fees_assessed_ -= fee_payment;
        remaining_payment -= fee_payment;
    }
    
    if (accrued_interest_ > 0.0) {
        Balance interest_payment = std::min(remaining_payment, accrued_interest_);
        accrued_interest_ -= interest_payment;
        remaining_payment -= interest_payment;
    }
    
    if (remaining_payment > 0.0) {
        Balance principal_payment = std::min(remaining_payment, current_principal_);
        current_principal_ -= principal_payment;
    }
    
    total_payments_made_ += payment_amount;
    last_payment_date_ = payment_date;
    last_payment_amount_ = payment_amount;
}

void ConsumerInstallment::applyLateFee(Amount fee_amount, Date fee_date) {
    if (fee_amount <= 0.0) {
        return;
    }
    
    late_fees_assessed_ += fee_amount;
    
    // Cap late fees at reasonable percentage of payment
    Amount max_late_fee = scheduled_payment_ * 0.05; // 5% of payment
    if (late_fees_assessed_ > max_late_fee) {
        late_fees_assessed_ = max_late_fee;
    }
}

void ConsumerInstallment::enterHardshipProgram(Date start_date, const std::string& program_type) {
    is_in_hardship_program_ = true;
    
    // Common hardship modifications
    if (program_type == "Payment Reduction") {
        scheduled_payment_ *= 0.75; // Reduce payment by 25%
    } else if (program_type == "Interest Rate Reduction") {
        current_interest_rate_ *= 0.75; // Reduce rate by 25%
        scheduled_payment_ = calculateMonthlyPayment();
    } else if (program_type == "Payment Deferral") {
        // Extend term by 6 months
        remaining_term_months_ += 6;
        maturity_date_ = Date(maturity_date_.dayOfMonth(), 
                             static_cast<QuantLib::Month>(maturity_date_.month() + 6), 
                             maturity_date_.year());
    }
}

void ConsumerInstallment::exitHardshipProgram(Date exit_date) {
    if (!is_in_hardship_program_) {
        return;
    }
    
    is_in_hardship_program_ = false;
    
    // Restore original terms (simplified - in practice would be more complex)
    current_interest_rate_ = original_interest_rate_;
    scheduled_payment_ = calculateMonthlyPayment();
}

// Promotional rate management
void ConsumerInstallment::setPromotionalRate(const PromotionalRateInfo& promo_info) {
    promotional_info_ = promo_info;
    is_in_promotional_period_ = true;
    
    // Recalculate payment if necessary
    if (promo_info.promotional_rate != current_interest_rate_) {
        // For 0% promotions, payment might be interest-only during promo period
        if (promo_info.promotional_rate == 0.0) {
            scheduled_payment_ = current_principal_ / promo_info.promotional_term_months;
        }
    }
}

void ConsumerInstallment::endPromotionalPeriod(Date end_date) {
    if (!is_in_promotional_period_ || !promotional_info_.has_value()) {
        return;
    }
    
    is_in_promotional_period_ = false;
    
    // If deferred interest promotion, add all deferred interest to principal
    if (promotional_info_->is_deferred_interest) {
        Amount deferred_interest = calculateDeferredInterest();
        current_principal_ += deferred_interest;
    }
    
    // Switch to regular rate
    current_interest_rate_ = promotional_info_->regular_rate;
    scheduled_payment_ = calculateMonthlyPayment();
}

Amount ConsumerInstallment::calculateDeferredInterest() const {
    if (!promotional_info_.has_value() || !promotional_info_->is_deferred_interest) {
        return 0.0;
    }
    
    // Calculate interest that would have accrued during promotional period
    Rate regular_rate = promotional_info_->regular_rate;
    int promo_months = promotional_info_->promotional_term_months;
    
    // Simplified calculation - in practice would be more precise
    return current_principal_ * regular_rate * (promo_months / 12.0);
}

// Risk assessment and scoring
double ConsumerInstallment::calculateRiskScore() const {
    double risk_score = 0.0;
    
    // Credit score component (40% weight)
    double credit_component = 0.0;
    if (borrower_.credit_score >= 750) credit_component = 1.0;
    else if (borrower_.credit_score >= 700) credit_component = 0.8;
    else if (borrower_.credit_score >= 650) credit_component = 0.6;
    else if (borrower_.credit_score >= 600) credit_component = 0.4;
    else credit_component = 0.2;
    
    risk_score += credit_component * 40.0;
    
    // DTI ratio component (30% weight)
    double dti_component = 0.0;
    if (debt_to_income_ratio_ <= 0.30) dti_component = 1.0;
    else if (debt_to_income_ratio_ <= 0.40) dti_component = 0.8;
    else if (debt_to_income_ratio_ <= 0.50) dti_component = 0.6;
    else dti_component = 0.3;
    
    risk_score += dti_component * 30.0;
    
    // Income stability (20% weight)
    double income_component = 0.8; // Default moderate score
    if (borrower_.employment_verification == EmploymentVerification::FULL_VERIFICATION) {
        income_component = 1.0;
    } else if (borrower_.employment_verification == EmploymentVerification::UNEMPLOYED) {
        income_component = 0.2;
    }
    
    risk_score += income_component * 20.0;
    
    // Loan characteristics (10% weight)
    double loan_component = 0.8;
    if (original_term_months_ <= 36) loan_component = 1.0;
    else if (original_term_months_ <= 60) loan_component = 0.8;
    else loan_component = 0.6;
    
    risk_score += loan_component * 10.0;
    
    return risk_score;
}

double ConsumerInstallment::calculateProbabilityOfDefault() const {
    // Simplified PD model based on credit score and DTI
    double base_pd = 0.0;
    
    // Credit score impact
    if (borrower_.credit_score >= 750) base_pd = 0.01;
    else if (borrower_.credit_score >= 700) base_pd = 0.02;
    else if (borrower_.credit_score >= 650) base_pd = 0.05;
    else if (borrower_.credit_score >= 600) base_pd = 0.10;
    else base_pd = 0.20;
    
    // DTI adjustment
    if (debt_to_income_ratio_ > 0.50) base_pd *= 1.5;
    else if (debt_to_income_ratio_ > 0.40) base_pd *= 1.2;
    
    // Term adjustment
    if (original_term_months_ > 60) base_pd *= 1.1;
    
    return std::min(base_pd, 0.50); // Cap at 50%
}

bool ConsumerInstallment::isHighRiskLoan() const {
    return calculateRiskScore() < 50.0 || 
           borrower_.credit_score < 600 ||
           debt_to_income_ratio_ > 0.50 ||
           days_delinquent_ > 30;
}

// Payment calculations
Amount ConsumerInstallment::calculateMonthlyPayment() const {
    if (remaining_term_months_ <= 0) {
        return current_principal_; // Lump sum if no remaining terms
    }
    
    Rate applicable_rate = current_interest_rate_;
    if (is_in_promotional_period_ && promotional_info_.has_value()) {
        applicable_rate = promotional_info_->promotional_rate;
    }
    
    if (applicable_rate == 0.0) {
        return current_principal_ / remaining_term_months_;
    }
    
    double monthly_rate = applicable_rate / 12.0;
    double factor = std::pow(1.0 + monthly_rate, remaining_term_months_);
    
    return current_principal_ * (monthly_rate * factor) / (factor - 1.0);
}

Amount ConsumerInstallment::calculatePayoffAmount(Date payoff_date) const {
    // Add any accrued interest up to payoff date
    // Simplified - would calculate precise daily interest in practice
    return current_principal_ + accrued_interest_;
}

Date ConsumerInstallment::calculatePayoffDate() const {
    if (scheduled_payment_ <= 0.0) {
        return maturity_date_;
    }
    
    // Estimate based on current payment schedule
    int estimated_months = static_cast<int>(getCurrentBalance() / scheduled_payment_);
    return Date(origination_date_.dayOfMonth(),
               static_cast<QuantLib::Month>(origination_date_.month() + payments_made_ + estimated_months),
               origination_date_.year());
}

// Utility function implementations
std::string consumerInstallmentTypeToString(ConsumerInstallmentType type) {
    switch (type) {
        case ConsumerInstallmentType::PERSONAL_LOAN: return "Personal Loan";
        case ConsumerInstallmentType::DEBT_CONSOLIDATION: return "Debt Consolidation";
        case ConsumerInstallmentType::HOME_IMPROVEMENT: return "Home Improvement";
        case ConsumerInstallmentType::MEDICAL_FINANCING: return "Medical Financing";
        case ConsumerInstallmentType::WEDDING_LOAN: return "Wedding Loan";
        case ConsumerInstallmentType::VACATION_LOAN: return "Vacation Loan";
        case ConsumerInstallmentType::MAJOR_PURCHASE: return "Major Purchase";
        case ConsumerInstallmentType::EDUCATIONAL_EXPENSES: return "Educational Expenses";
        case ConsumerInstallmentType::APPLIANCE_FINANCING: return "Appliance Financing";
        case ConsumerInstallmentType::FURNITURE_FINANCING: return "Furniture Financing";
        case ConsumerInstallmentType::PAYDAY_LOAN: return "Payday Loan";
        case ConsumerInstallmentType::TITLE_LOAN: return "Title Loan";
        case ConsumerInstallmentType::PEER_TO_PEER: return "Peer-to-Peer Loan";
        case ConsumerInstallmentType::POINT_OF_SALE: return "Point-of-Sale Financing";
        case ConsumerInstallmentType::CRYPTOCURRENCY_BACKED: return "Cryptocurrency-Backed Loan";
        default: return "Unknown";
    }
}

std::string creditScoreRangeToString(CreditScoreRange range) {
    switch (range) {
        case CreditScoreRange::SUPER_PRIME: return "Super Prime (781-850)";
        case CreditScoreRange::PRIME: return "Prime (661-780)";
        case CreditScoreRange::NEAR_PRIME: return "Near Prime (601-660)";
        case CreditScoreRange::SUBPRIME: return "Subprime (501-600)";
        case CreditScoreRange::DEEP_SUBPRIME: return "Deep Subprime (300-500)";
        case CreditScoreRange::NO_SCORE: return "No Score";
        case CreditScoreRange::THIN_FILE: return "Thin File";
        default: return "Unknown";
    }
}

std::string loanPurposeToString(LoanPurpose purpose) {
    switch (purpose) {
        case LoanPurpose::DEBT_CONSOLIDATION_PURPOSE: return "Debt Consolidation";
        case LoanPurpose::HOME_IMPROVEMENT_PURPOSE: return "Home Improvement";
        case LoanPurpose::MAJOR_PURCHASE_PURPOSE: return "Major Purchase";
        case LoanPurpose::EMERGENCY_EXPENSES: return "Emergency Expenses";
        case LoanPurpose::BUSINESS_EXPENSES: return "Business Expenses";
        case LoanPurpose::INVESTMENT: return "Investment";
        case LoanPurpose::EDUCATION: return "Education";
        case LoanPurpose::MEDICAL_EXPENSES: return "Medical Expenses";
        case LoanPurpose::VACATION_TRAVEL: return "Vacation/Travel";
        case LoanPurpose::WEDDING_EXPENSES: return "Wedding Expenses";
        case LoanPurpose::MOVING_RELOCATION: return "Moving/Relocation";
        case LoanPurpose::OTHER_PURPOSE: return "Other";
        default: return "Unknown";
    }
}

// Credit scoring functions
CreditScoreRange categorizeScore(int credit_score) {
    if (credit_score >= 781) return CreditScoreRange::SUPER_PRIME;
    if (credit_score >= 661) return CreditScoreRange::PRIME;
    if (credit_score >= 601) return CreditScoreRange::NEAR_PRIME;
    if (credit_score >= 501) return CreditScoreRange::SUBPRIME;
    if (credit_score >= 300) return CreditScoreRange::DEEP_SUBPRIME;
    return CreditScoreRange::NO_SCORE;
}

double calculateBasePricingRate(CreditScoreRange score_range, ConsumerInstallmentType loan_type) {
    double base_rate = 0.05; // 5% base rate
    
    // Credit score adjustment
    switch (score_range) {
        case CreditScoreRange::SUPER_PRIME: base_rate += 0.02; break; // 7%
        case CreditScoreRange::PRIME: base_rate += 0.05; break;       // 10%
        case CreditScoreRange::NEAR_PRIME: base_rate += 0.10; break;  // 15%
        case CreditScoreRange::SUBPRIME: base_rate += 0.20; break;    // 25%
        case CreditScoreRange::DEEP_SUBPRIME: base_rate += 0.30; break; // 35%
        default: base_rate += 0.25; break; // 30%
    }
    
    // Loan type adjustment
    if (loan_type == ConsumerInstallmentType::PAYDAY_LOAN) {
        base_rate += 0.20; // Payday loans much higher
    } else if (loan_type == ConsumerInstallmentType::TITLE_LOAN) {
        base_rate += 0.15; // Title loans higher
    } else if (loan_type == ConsumerInstallmentType::POINT_OF_SALE) {
        base_rate -= 0.05; // POS financing often lower
    }
    
    return std::min(base_rate, 0.36); // Cap at 36% (typical state limits)
}

} // namespace Structura