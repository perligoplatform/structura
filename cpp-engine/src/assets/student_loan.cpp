#include "student_loan.h"
#include <algorithm>
#include <cmath>
#include <stdexcept>
#include <sstream>

namespace Structura {

// Constructor
StudentLoan::StudentLoan(const std::string& id, StudentLoanProgram program,
                        const StudentBorrower& borrower, const EducationalInstitution& institution,
                        const LoanServicer& servicer, Amount principal, Rate interest_rate,
                        Date disbursement_date)
    : loan_id_(id), program_type_(program), borrower_(borrower), 
      institution_(institution), servicer_(servicer), status_(StudentLoanStatus::IN_SCHOOL),
      repayment_plan_(RepaymentPlan::STANDARD), original_principal_(principal),
      current_principal_(principal), accrued_interest_(0.0), capitalized_interest_(0.0),
      current_interest_rate_(interest_rate), original_interest_rate_(interest_rate),
      first_disbursement_date_(disbursement_date), payments_made_(0),
      consecutive_on_time_payments_(0), total_payments_made_(0.0),
      total_deferment_months_(0), total_forbearance_months_(0),
      interest_accrues_during_deferment_(true), is_eligible_for_forgiveness_(false),
      qualifying_payments_for_forgiveness_(0), days_delinquent_(0),
      collection_costs_(0.0), is_in_rehabilitation_(false),
      rehabilitation_payments_made_(0), is_pslf_eligible_(false),
      pslf_qualifying_payments_(0) {
    
    // Set subsidy status based on program type
    is_subsidized_ = (program == StudentLoanProgram::FEDERAL_DIRECT_SUBSIDIZED);
    
    // Calculate origination fee
    origination_fee_ = calculateOriginationFee(program, principal, 2024); // Current academic year
    
    // Set derived dates
    grace_period_end_ = Date(disbursement_date.dayOfMonth(), 
                            static_cast<QuantLib::Month>(disbursement_date.month() + 6), 
                            disbursement_date.year());    // Set initial repayment begin date (after grace period)
    repayment_begin_date_ = grace_period_end_;
    
    // Calculate initial scheduled payment for standard 10-year term
    original_repayment_term_months_ = 120; // 10 years
    remaining_term_months_ = original_repayment_term_months_;
    scheduled_monthly_payment_ = calculateMonthlyPayment(RepaymentPlan::STANDARD);
    
    // Calculate scheduled payoff date
    scheduled_payoff_date_ = Date(repayment_begin_date_.dayOfMonth(), 
                                 repayment_begin_date_.month(), 
                                 repayment_begin_date_.year() + 10);
}

// Core asset interface implementations
void StudentLoan::makePayment(Balance payment_amount, Date payment_date) {
    if (payment_amount <= 0.0) {
        throw std::invalid_argument("Payment amount must be positive");
    }
    
    processStandardPayment(payment_amount, payment_date);
}

void StudentLoan::applyInterestAccrual(Date accrual_date) {
    if (status_ == StudentLoanStatus::PAID_OFF || 
        status_ == StudentLoanStatus::DISCHARGED ||
        status_ == StudentLoanStatus::FORGIVEN) {
        return; // No interest accrual on paid off/discharged loans
    }
    
    // Check if interest should accrue during deferment
    if (isInDefermentOrForbearance() && !interest_accrues_during_deferment_ && is_subsidized_) {
        return; // Government pays interest during deferment for subsidized loans
    }
    
    // Calculate daily interest rate
    double daily_rate = current_interest_rate_ / 365.25;
    Balance daily_interest = current_principal_ * daily_rate;
    
    accrued_interest_ += daily_interest;
}

void StudentLoan::applyDefault(Date default_date) {
    status_ = StudentLoanStatus::DEFAULT;
    first_delinquent_date_ = default_date;
    days_delinquent_ = 270; // Federal loans default after 270+ days delinquency
    
    // Capitalize all accrued interest upon default
    capitalizeAccruedInterest(default_date);
    
    // Add collection costs (typical percentage)
    collection_costs_ = current_principal_ * 0.18; // Up to 18.5% for federal loans
}

void StudentLoan::applyPrepayment(Balance amount, Date payment_date) {
    if (amount <= 0.0) {
        throw std::invalid_argument("Prepayment amount must be positive");
    }
    
    // Apply prepayment (no prepayment penalty for student loans)
    Balance remaining_amount = amount;
    
    // First pay any accrued interest
    if (accrued_interest_ > 0.0 && remaining_amount > 0.0) {
        Balance interest_payment = std::min(remaining_amount, accrued_interest_);
        accrued_interest_ -= interest_payment;
        remaining_amount -= interest_payment;
    }
    
    // Then pay principal
    if (remaining_amount > 0.0) {
        Balance principal_payment = std::min(remaining_amount, current_principal_);
        current_principal_ -= principal_payment;
        
        if (current_principal_ <= 0.01) { // Essentially paid off
            current_principal_ = 0.0;
            accrued_interest_ = 0.0;
            status_ = StudentLoanStatus::PAID_OFF;
        }
    }
    
    last_payment_date_ = payment_date;
    last_payment_amount_ = amount;
    total_payments_made_ += amount;
}

Balance StudentLoan::getCurrentBalance() const {
    return current_principal_ + accrued_interest_;
}

Rate StudentLoan::getCurrentRate() const {
    return current_interest_rate_;
}

void StudentLoan::resetToOriginal() {
    current_principal_ = original_principal_;
    accrued_interest_ = 0.0;
    capitalized_interest_ = 0.0;
    current_interest_rate_ = original_interest_rate_;
    status_ = StudentLoanStatus::IN_SCHOOL;
    repayment_plan_ = RepaymentPlan::STANDARD;
    payments_made_ = 0;
    consecutive_on_time_payments_ = 0;
    total_payments_made_ = 0.0;
    days_delinquent_ = 0;
    collection_costs_ = 0.0;
    is_in_rehabilitation_ = false;
    rehabilitation_payments_made_ = 0;
    pslf_qualifying_payments_ = 0;
}

// Student loan specific payment processing
void StudentLoan::processStandardPayment(Amount payment_amount, Date payment_date) {
    if (status_ == StudentLoanStatus::PAID_OFF) {
        return;
    }
    
    Balance remaining_payment = payment_amount;
    
    // Apply to collection costs first (if any)
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
    }
    
    // Update payment tracking
    payments_made_++;
    total_payments_made_ += payment_amount;
    last_payment_date_ = payment_date;
    last_payment_amount_ = payment_amount;
    
    // Check if loan is paid off
    if (current_principal_ <= 0.01 && accrued_interest_ <= 0.01) {
        current_principal_ = 0.0;
        accrued_interest_ = 0.0;
        status_ = StudentLoanStatus::PAID_OFF;
    }
    
    // Update consecutive on-time payments and PSLF tracking
    if (status_ == StudentLoanStatus::REPAYMENT) {
        consecutive_on_time_payments_++;
        
        // Count qualifying payments for PSLF if eligible
        if (is_pslf_eligible_ && repayment_plan_ != RepaymentPlan::STANDARD) {
            pslf_qualifying_payments_++;
        }
    }
    
    // Reset delinquency if applicable
    if (days_delinquent_ > 0) {
        days_delinquent_ = 0;
    }
}

void StudentLoan::processIDRPayment(Date payment_date) {
    if (!idr_calculation_.has_value()) {
        throw std::runtime_error("IDR calculation not available");
    }
    
    Amount payment_amount = idr_calculation_->monthly_payment;
    processStandardPayment(payment_amount, payment_date);
}

void StudentLoan::capitalizeAccruedInterest(Date capitalization_date) {
    if (accrued_interest_ > 0.0) {
        current_principal_ += accrued_interest_;
        capitalized_interest_ += accrued_interest_;
        accrued_interest_ = 0.0;
    }
}

// Deferment and forbearance methods
void StudentLoan::enterDeferment(DefermentReason reason, Date start_date, Date end_date) {
    if (!canEnterDeferment(reason)) {
        throw std::runtime_error("Not eligible for this deferment type");
    }
    
    current_deferment_reason_ = reason;
    deferment_forbearance_start_ = start_date;
    deferment_forbearance_end_ = end_date;
    status_ = StudentLoanStatus::DEFERMENT;
    
    // Determine if interest accrues based on loan type and deferment reason
    if (is_subsidized_ && (reason == DefermentReason::IN_SCHOOL_DEFERMENT ||
                          reason == DefermentReason::UNEMPLOYMENT_DEFERMENT ||
                          reason == DefermentReason::ECONOMIC_HARDSHIP_DEFERMENT)) {
        interest_accrues_during_deferment_ = false;
    } else {
        interest_accrues_during_deferment_ = true;
    }
}

void StudentLoan::enterForbearance(DefermentReason reason, Date start_date, Date end_date) {
    current_deferment_reason_ = reason;
    deferment_forbearance_start_ = start_date;
    deferment_forbearance_end_ = end_date;
    status_ = StudentLoanStatus::FORBEARANCE;
    interest_accrues_during_deferment_ = true; // Interest always accrues during forbearance
}

void StudentLoan::exitDefermentForbearance(Date exit_date) {
    if (isInDefermentOrForbearance()) {
        // Calculate total months in deferment/forbearance
        int months_in_status = 1; // Simplified calculation
        
        if (status_ == StudentLoanStatus::DEFERMENT) {
            total_deferment_months_ += months_in_status;
        } else {
            total_forbearance_months_ += months_in_status;
        }
        
        // Capitalize interest if it accrued during the period
        if (interest_accrues_during_deferment_) {
            capitalizeAccruedInterest(exit_date);
        }
        
        status_ = StudentLoanStatus::REPAYMENT;
        current_deferment_reason_.reset();
    }
}

bool StudentLoan::isInDefermentOrForbearance() const {
    return status_ == StudentLoanStatus::DEFERMENT || 
           status_ == StudentLoanStatus::FORBEARANCE;
}

bool StudentLoan::canEnterDeferment(DefermentReason reason) const {
    // Simplified eligibility check - in practice this would be more complex
    switch (reason) {
        case DefermentReason::IN_SCHOOL_DEFERMENT:
            return true; // Always eligible if enrolled
        case DefermentReason::UNEMPLOYMENT_DEFERMENT:
        case DefermentReason::ECONOMIC_HARDSHIP_DEFERMENT:
            return program_type_ != StudentLoanProgram::PRIVATE_FIXED_RATE &&
                   program_type_ != StudentLoanProgram::PRIVATE_VARIABLE_RATE;
        default:
            return true;
    }
}

// Repayment plan management
void StudentLoan::changeRepaymentPlan(RepaymentPlan new_plan, Date effective_date) {
    repayment_plan_ = new_plan;
    scheduled_monthly_payment_ = calculateMonthlyPayment(new_plan);
    
    // Recalculate IDR payment if applicable
    if (new_plan == RepaymentPlan::INCOME_DRIVEN_IBR ||
        new_plan == RepaymentPlan::INCOME_DRIVEN_PAYE ||
        new_plan == RepaymentPlan::INCOME_DRIVEN_REPAYE ||
        new_plan == RepaymentPlan::INCOME_DRIVEN_ICR) {
        idr_calculation_ = calculateIDRPayment();
    }
}

Amount StudentLoan::calculateMonthlyPayment(RepaymentPlan plan) const {
    switch (plan) {
        case RepaymentPlan::STANDARD: {
            // Standard 10-year fixed payment
            double monthly_rate = current_interest_rate_ / 12;
            int num_payments = 120; // 10 years
            if (monthly_rate == 0) return current_principal_ / num_payments;
            return current_principal_ * (monthly_rate * std::pow(1 + monthly_rate, num_payments)) /
                   (std::pow(1 + monthly_rate, num_payments) - 1);
        }
        case RepaymentPlan::EXTENDED: {
            // Extended 25-year payment
            double monthly_rate = current_interest_rate_ / 12;
            int num_payments = 300; // 25 years
            if (monthly_rate == 0) return current_principal_ / num_payments;
            return current_principal_ * (monthly_rate * std::pow(1 + monthly_rate, num_payments)) /
                   (std::pow(1 + monthly_rate, num_payments) - 1);
        }
        case RepaymentPlan::GRADUATED: {
            // Start at 50% of standard payment, increase every 2 years
            Amount standard_payment = calculateMonthlyPayment(RepaymentPlan::STANDARD);
            return standard_payment * 0.5; // Initial payment
        }
        case RepaymentPlan::INCOME_DRIVEN_IBR:
        case RepaymentPlan::INCOME_DRIVEN_PAYE:
        case RepaymentPlan::INCOME_DRIVEN_REPAYE:
        case RepaymentPlan::INCOME_DRIVEN_ICR: {
            if (idr_calculation_.has_value()) {
                return idr_calculation_->monthly_payment;
            }
            return 0.0; // Requires income certification
        }
        case RepaymentPlan::INTEREST_ONLY: {
            return current_principal_ * (current_interest_rate_ / 12);
        }
        default:
            return calculateMonthlyPayment(RepaymentPlan::STANDARD);
    }
}

IDRCalculation StudentLoan::calculateIDRPayment() const {
    IDRCalculation calc;
    
    Amount discretionary_income = borrower_.discretionary_income;
    if (discretionary_income <= 0) {
        calc.monthly_payment = 0.0;
        return calc;
    }
    
    // Calculate based on repayment plan type
    double percentage = 0.0;
    switch (repayment_plan_) {
        case RepaymentPlan::INCOME_DRIVEN_IBR:
            percentage = 0.15; // 15% of discretionary income
            break;
        case RepaymentPlan::INCOME_DRIVEN_PAYE:
            percentage = 0.10; // 10% of discretionary income
            break;
        case RepaymentPlan::INCOME_DRIVEN_REPAYE:
            percentage = 0.10; // 10% of discretionary income
            break;
        case RepaymentPlan::INCOME_DRIVEN_ICR:
            percentage = 0.20; // 20% of discretionary income
            break;
        default:
            percentage = 0.10;
    }
    
    calc.monthly_payment = (discretionary_income * percentage) / 12;
    calc.payment_cap_percentage = percentage;
    calc.annual_payment_cap = discretionary_income * percentage;
    
    // Next recertification is typically annual
        Date recert_due = Date(last_income_certification_.dayOfMonth(),
                                   last_income_certification_.month(),
                                   last_income_certification_.year() + 1);
    
    return calc;
}

void StudentLoan::recertifyIncome(Amount new_annual_income, int new_family_size, Date cert_date) {
    // Update borrower information
    const_cast<StudentBorrower&>(borrower_).annual_income = new_annual_income;
    const_cast<StudentBorrower&>(borrower_).family_size = new_family_size;
    const_cast<StudentBorrower&>(borrower_).income_certification_date = cert_date;
    last_income_certification_ = cert_date;
    
    // Recalculate discretionary income (simplified calculation)
    Amount poverty_guideline = 12760 + (4480 * (new_family_size - 1)); // 2024 federal poverty guideline
    Amount discretionary = std::max(0.0, new_annual_income - (poverty_guideline * 1.5));
    const_cast<StudentBorrower&>(borrower_).discretionary_income = discretionary;
    
    // Recalculate IDR payment if on IDR plan
    if (repayment_plan_ == RepaymentPlan::INCOME_DRIVEN_IBR ||
        repayment_plan_ == RepaymentPlan::INCOME_DRIVEN_PAYE ||
        repayment_plan_ == RepaymentPlan::INCOME_DRIVEN_REPAYE ||
        repayment_plan_ == RepaymentPlan::INCOME_DRIVEN_ICR) {
        idr_calculation_ = calculateIDRPayment();
        scheduled_monthly_payment_ = idr_calculation_->monthly_payment;
    }
}

// Financial calculations
Amount StudentLoan::calculateTotalCostOfLoan() const {
    // Simplified calculation - would be more complex in practice
    Amount remaining_payments = scheduled_monthly_payment_ * remaining_term_months_;
    return total_payments_made_ + remaining_payments;
}

Date StudentLoan::calculatePayoffDate() const {
    if (scheduled_monthly_payment_ <= 0) {
        return Date(); // Cannot calculate without payment amount
    }
    
    // Simplified calculation
    int months_remaining = static_cast<int>(getCurrentBalance() / scheduled_monthly_payment_);
    return Date(repayment_begin_date_.dayOfMonth(), 
               static_cast<QuantLib::Month>(repayment_begin_date_.month() + months_remaining), 
               repayment_begin_date_.year());
}

Amount StudentLoan::calculatePayoffAmount(Date payoff_date) const {
    // Calculate accrued interest up to payoff date
    // This is simplified - actual calculation would be more precise
    return current_principal_ + accrued_interest_;
}

// Utility function implementations
std::string studentLoanProgramToString(StudentLoanProgram program) {
    switch (program) {
        case StudentLoanProgram::FEDERAL_DIRECT_SUBSIDIZED: return "Federal Direct Subsidized";
        case StudentLoanProgram::FEDERAL_DIRECT_UNSUBSIDIZED: return "Federal Direct Unsubsidized";
        case StudentLoanProgram::FEDERAL_DIRECT_PLUS_PARENT: return "Federal Direct PLUS (Parent)";
        case StudentLoanProgram::FEDERAL_DIRECT_PLUS_GRAD: return "Federal Direct PLUS (Graduate)";
        case StudentLoanProgram::FEDERAL_PERKINS: return "Federal Perkins";
        case StudentLoanProgram::FEDERAL_FFEL: return "Federal FFEL";
        case StudentLoanProgram::PRIVATE_FIXED_RATE: return "Private Fixed Rate";
        case StudentLoanProgram::PRIVATE_VARIABLE_RATE: return "Private Variable Rate";
        case StudentLoanProgram::STATE_SPONSORED: return "State Sponsored";
        case StudentLoanProgram::INSTITUTIONAL: return "Institutional";
        default: return "Unknown";
    }
}

std::string repaymentPlanToString(RepaymentPlan plan) {
    switch (plan) {
        case RepaymentPlan::STANDARD: return "Standard";
        case RepaymentPlan::EXTENDED: return "Extended";
        case RepaymentPlan::GRADUATED: return "Graduated";
        case RepaymentPlan::INCOME_DRIVEN_IBR: return "Income-Based Repayment (IBR)";
        case RepaymentPlan::INCOME_DRIVEN_PAYE: return "Pay As You Earn (PAYE)";
        case RepaymentPlan::INCOME_DRIVEN_REPAYE: return "Revised Pay As You Earn (REPAYE)";
        case RepaymentPlan::INCOME_DRIVEN_ICR: return "Income-Contingent Repayment (ICR)";
        case RepaymentPlan::INTEREST_ONLY: return "Interest Only";
        case RepaymentPlan::CUSTOM: return "Custom";
        default: return "Unknown";
    }
}

std::string studentLoanStatusToString(StudentLoanStatus status) {
    switch (status) {
        case StudentLoanStatus::IN_SCHOOL: return "In School";
        case StudentLoanStatus::GRACE_PERIOD: return "Grace Period";
        case StudentLoanStatus::REPAYMENT: return "Repayment";
        case StudentLoanStatus::DEFERMENT: return "Deferment";
        case StudentLoanStatus::FORBEARANCE: return "Forbearance";
        case StudentLoanStatus::DEFAULT: return "Default";
        case StudentLoanStatus::PAID_OFF: return "Paid Off";
        case StudentLoanStatus::DISCHARGED: return "Discharged";
        case StudentLoanStatus::FORGIVEN: return "Forgiven";
        default: return "Unknown";
    }
}

// Federal loan utility functions
Amount getFederalLoanLimit(StudentLoanProgram program, int academic_year, 
                          bool is_dependent, int grade_level) {
    // 2024-2025 academic year limits (simplified)
    switch (program) {
        case StudentLoanProgram::FEDERAL_DIRECT_SUBSIDIZED:
            if (grade_level == 1) return is_dependent ? 3500 : 3500;
            if (grade_level == 2) return is_dependent ? 4500 : 4500;
            return is_dependent ? 5500 : 5500; // Junior/Senior
        case StudentLoanProgram::FEDERAL_DIRECT_UNSUBSIDIZED:
            if (grade_level == 1) return is_dependent ? 2000 : 6000;
            if (grade_level == 2) return is_dependent ? 2000 : 6000;
            return is_dependent ? 2000 : 7000; // Junior/Senior
        case StudentLoanProgram::FEDERAL_DIRECT_PLUS_PARENT:
        case StudentLoanProgram::FEDERAL_DIRECT_PLUS_GRAD:
            return 999999; // Up to cost of attendance minus other aid
        default:
            return 0;
    }
}

Rate getCurrentFederalInterestRate(StudentLoanProgram program, int academic_year) {
    // 2024-2025 academic year rates (example)
    switch (program) {
        case StudentLoanProgram::FEDERAL_DIRECT_SUBSIDIZED:
        case StudentLoanProgram::FEDERAL_DIRECT_UNSUBSIDIZED:
            return 0.0668; // 6.68%
        case StudentLoanProgram::FEDERAL_DIRECT_PLUS_PARENT:
        case StudentLoanProgram::FEDERAL_DIRECT_PLUS_GRAD:
            return 0.0723; // 7.23%
        default:
            return 0.05; // Default rate
    }
}

Amount calculateOriginationFee(StudentLoanProgram program, Amount loan_amount, int academic_year) {
    // 2024-2025 academic year fees
    switch (program) {
        case StudentLoanProgram::FEDERAL_DIRECT_SUBSIDIZED:
        case StudentLoanProgram::FEDERAL_DIRECT_UNSUBSIDIZED:
            return loan_amount * 0.01057; // 1.057%
        case StudentLoanProgram::FEDERAL_DIRECT_PLUS_PARENT:
        case StudentLoanProgram::FEDERAL_DIRECT_PLUS_GRAD:
            return loan_amount * 0.04228; // 4.228%
        default:
            return 0.0; // No fee for private loans
    }
}

} // namespace Structura