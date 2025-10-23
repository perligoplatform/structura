#include "auto_loan.h"
#include <algorithm>
#include <cmath>
#include <stdexcept>

namespace Structura {

// Vehicle implementation
Amount Vehicle::calculateStraightLineDepreciation(int months_elapsed) const {
    if (months_elapsed <= 0) return 0.0;
    
    // Assume 15-year useful life for vehicles
    const int useful_life_months = 180;
    const Amount residual_value = original_msrp * 0.1; // 10% residual
    
    double depreciation_per_month = (purchase_price - residual_value) / useful_life_months;
    return std::min(depreciation_per_month * months_elapsed, purchase_price - residual_value);
}

Amount Vehicle::calculateAcceleratedDepreciation(int months_elapsed, double factor) const {
    if (months_elapsed <= 0) return 0.0;
    
    // Double declining balance method
    double annual_rate = factor / 15.0; // 15-year life
    double monthly_rate = annual_rate / 12.0;
    
    Amount book_value = purchase_price;
    for (int i = 0; i < months_elapsed; ++i) {
        Amount depreciation = book_value * monthly_rate;
        book_value = std::max(book_value - depreciation, purchase_price * 0.1); // Floor at 10%
        if (book_value <= purchase_price * 0.1) break;
    }
    
    return purchase_price - book_value;
}

void Vehicle::updateMileage(int new_mileage, Date update_date) {
    if (new_mileage >= mileage) {
        mileage = new_mileage;
        last_valuation_date = update_date;
        
        // Update value based on mileage
        current_value = estimateCurrentValue(update_date);
    }
}

Amount Vehicle::estimateCurrentValue(Date valuation_date) const {
    // Simplified valuation model
    int age_months = valuation_date - Date(); // Simplified date math
    
    // Base depreciation by age
    Amount depreciated_value = purchase_price;
    if (age_months > 0) {
        double annual_depreciation_rate = 0.15; // 15% per year
        double total_depreciation = 1.0 - std::pow(1.0 - annual_depreciation_rate, age_months / 12.0);
        depreciated_value = purchase_price * (1.0 - total_depreciation);
    }
    
    // Adjust for mileage (typical 12,000 miles/year)
    int expected_mileage = age_months * 1000; // ~12k miles/year
    if (mileage > expected_mileage) {
        double excess_mileage_penalty = (mileage - expected_mileage) * 0.10; // $0.10 per excess mile
        depreciated_value -= excess_mileage_penalty;
    }
    
    // Floor at 10% of original purchase price
    return std::max(depreciated_value, purchase_price * 0.1);
}

double Vehicle::getDepreciationRate() const {
    if (purchase_price <= 0) return 0.0;
    return (purchase_price - current_value) / purchase_price;
}

// AutoLoanType conversion
std::string autoLoanTypeToString(AutoLoanType type) {
    switch (type) {
        case AutoLoanType::NEW_VEHICLE: return "New Vehicle";
        case AutoLoanType::USED_VEHICLE: return "Used Vehicle";
        case AutoLoanType::REFINANCE: return "Refinance";
        case AutoLoanType::LEASE_BUYOUT: return "Lease Buyout";
        case AutoLoanType::PRIVATE_PARTY: return "Private Party";
        default: return "Unknown";
    }
}

// LTV Category calculation
LTVCategory calculateLTVCategory(Amount loan_amount, Amount vehicle_value) {
    if (vehicle_value <= 0) return LTVCategory::HIGH_LTV;
    
    double ltv_ratio = loan_amount / vehicle_value;
    
    if (ltv_ratio <= 0.70) return LTVCategory::SUPER_PRIME;
    if (ltv_ratio <= 0.80) return LTVCategory::PRIME;
    if (ltv_ratio <= 0.90) return LTVCategory::NEAR_PRIME;
    if (ltv_ratio <= 1.00) return LTVCategory::SUB_PRIME;
    return LTVCategory::HIGH_LTV;
}

// AutoLoan implementation
AutoLoan::AutoLoan(const std::string& id, AutoLoanType type, const Vehicle& vehicle,
                   const Obligor& borrower, Amount original_balance, Rate rate,
                   int term_months, Date origination_date)
    : loan_id_(id), loan_type_(type), vehicle_(vehicle), borrower_(borrower),
      status_(Status::Current), original_balance_(original_balance),
      current_balance_(original_balance), current_rate_(rate),
      original_term_months_(term_months), remaining_terms_(term_months),
      origination_date_(origination_date) {
    
    // Calculate maturity date
    maturity_date_ = origination_date + (term_months * 30); // Approximate
    next_payment_date_ = origination_date + 30; // First payment due in 30 days
    
    // Calculate monthly payment
    monthly_payment_ = calculateMonthlyPayment();
    
    // Initialize LTV
    loan_to_value_ratio_ = original_balance / vehicle.current_value;
    ltv_category_ = calculateLTVCategory(original_balance, vehicle.current_value);
    
    // Initialize payment tracking
    payments_made_ = 0;
    payments_missed_ = 0;
    total_interest_paid_ = 0.0;
    total_principal_paid_ = 0.0;
    is_in_default_ = false;
    
    // Initialize credit enhancement
    down_payment_ = vehicle.purchase_price - original_balance;
    has_gap_insurance_ = false;
    has_extended_warranty_ = false;
    has_credit_insurance_ = false;
}

void AutoLoan::makePayment(Balance principalPayment, Balance interestPayment, Date paymentDate) {
    if (principalPayment < 0 || interestPayment < 0 || current_balance_ <= 0) return;
    
    // Apply payment directly using provided split
    total_interest_paid_ += interestPayment;
    total_principal_paid_ += principalPayment;
    current_balance_ = std::max(0.0, current_balance_ - principalPayment);
    
    // Update tracking
    payments_made_++;
    remaining_terms_ = std::max(0, remaining_terms_ - 1);
    last_payment_date_ = paymentDate;
    
    // Update status if loan is paid off
    if (current_balance_ <= 0.01) { // Allow for small rounding errors
        status_ = Status::PaidOff;
        remaining_terms_ = 0;
    }
}

void AutoLoan::applyDefault(Date defaultDate) {
    status_ = Status::Defaulted;
    is_in_default_ = true;
    // Would trigger collection procedures
}

void AutoLoan::applyPrepayment(Balance amount, Date paymentDate) {
    Amount payoff_amount = current_balance_;
    if (amount >= payoff_amount) {
        processPayoff(amount, paymentDate);
    } else {
        // Partial prepayment - apply to principal
        current_balance_ = std::max(0.0, current_balance_ - amount);
        total_principal_paid_ += amount;
        
        // Recalculate remaining terms
        if (current_balance_ > 0) {
            // Simplified recalculation
            remaining_terms_ = static_cast<int>(current_balance_ / (monthly_payment_ * 0.7)); // Rough estimate
        } else {
            remaining_terms_ = 0;
            status_ = Status::PaidOff;
        }
    }
}

Balance AutoLoan::getCurrentBalance() const {
    return current_balance_;
}

Rate AutoLoan::getCurrentRate() const {
    return current_rate_;
}

void AutoLoan::resetToOriginal() {
    current_balance_ = original_balance_;
    remaining_terms_ = original_term_months_;
    status_ = Status::Current;
    payments_made_ = 0;
    payments_missed_ = 0;
    total_interest_paid_ = 0.0;
    total_principal_paid_ = 0.0;
    is_in_default_ = false;
}

void AutoLoan::updateVehicleValue(Amount new_value, Date valuation_date) {
    vehicle_.current_value = new_value;
    vehicle_.last_valuation_date = valuation_date;
    
    // Recalculate LTV
    if (new_value > 0) {
        loan_to_value_ratio_ = current_balance_ / new_value;
        ltv_category_ = calculateLTVCategory(current_balance_, new_value);
    }
}

void AutoLoan::updateMileage(int new_mileage, Date update_date) {
    vehicle_.updateMileage(new_mileage, update_date);
    
    // Update LTV based on new vehicle value
    updateVehicleValue(vehicle_.current_value, update_date);
}

double AutoLoan::getCurrentLTV() const {
    return loan_to_value_ratio_;
}

Amount AutoLoan::calculateEquityPosition() const {
    return std::max(0.0, vehicle_.current_value - current_balance_);
}

void AutoLoan::processRegularPayment(Amount payment_amount, Date payment_date) {
    if (payment_amount <= 0 || current_balance_ <= 0) return;
    
    // Calculate interest portion
    double daily_rate = current_rate_ / 365.0;
    int days_since_last_payment = (last_payment_date_ == Date()) ? 30 : (payment_date - last_payment_date_);
    Amount interest_due = current_balance_ * daily_rate * days_since_last_payment;
    
    // Split payment
    Amount interest_payment = std::min(payment_amount, interest_due);
    Amount principal_payment = payment_amount - interest_payment;
    
    // Apply payment
    total_interest_paid_ += interest_payment;
    total_principal_paid_ += principal_payment;
    current_balance_ = std::max(0.0, current_balance_ - principal_payment);
    
    // Update tracking
    payments_made_++;
    remaining_terms_ = std::max(0, remaining_terms_ - 1);
    last_payment_date_ = payment_date;
    next_payment_date_ = payment_date + 30; // Next month
    
    // Check if paid off
    if (current_balance_ <= 0.01) {
        status_ = Status::PaidOff;
        remaining_terms_ = 0;
    }
}

void AutoLoan::processPartialPayment(Amount payment_amount, Date payment_date) {
    if (payment_amount < monthly_payment_) {
        // Track as missed payment but apply what was received
        payments_missed_++;
        processRegularPayment(payment_amount, payment_date);
    } else {
        processRegularPayment(payment_amount, payment_date);
    }
}

void AutoLoan::applyLateFee(Amount fee_amount, Date fee_date) {
    // Add late fee to current balance
    current_balance_ += fee_amount;
}

void AutoLoan::processPayoff(Amount payoff_amount, Date payoff_date) {
    total_principal_paid_ += current_balance_;
    current_balance_ = 0.0;
    remaining_terms_ = 0;
    status_ = Status::PaidOff;
    last_payment_date_ = payoff_date;
}

void AutoLoan::initiateRepossession(Date repo_date) {
    status_ = Status::Defaulted;
    is_in_default_ = true;
    // Would trigger repossession procedures
}

void AutoLoan::processLiquidation(Amount recovery_amount, Date liquidation_date) {
    // Apply recovery to outstanding balance
    Amount net_recovery = std::max(0.0, recovery_amount - 500.0); // Subtract repo costs
    current_balance_ = std::max(0.0, current_balance_ - net_recovery);
    
    if (current_balance_ <= 0.01) {
        status_ = Status::PaidOff;
    }
}

Amount AutoLoan::calculateDeficiencyBalance() const {
    if (status_ != Status::Defaulted) return 0.0;
    return std::max(0.0, current_balance_ - vehicle_.current_value);
}

void AutoLoan::processInsuranceClaim(Amount claim_amount, Date claim_date) {
    // Process insurance claim (gap insurance, comprehensive, etc.)
    Amount net_claim = std::min(claim_amount, current_balance_);
    current_balance_ = std::max(0.0, current_balance_ - net_claim);
    
    if (current_balance_ <= 0.01) {
        status_ = Status::PaidOff;
    }
}

void AutoLoan::updateVehicleCondition(const std::string& condition_report) {
    // Would store condition information and potentially adjust value
    // Implementation would depend on condition scoring system
}

Amount AutoLoan::estimateRecoveryValue() const {
    // Estimate recovery value in case of default
    Amount vehicle_value = vehicle_.estimateCurrentValue(Date());
    Amount recovery_rate = 0.70; // Typical 70% recovery for auto loans
    
    return vehicle_value * recovery_rate;
}

double AutoLoan::calculateProbabilityOfDefault() const {
    // Simplified PD calculation based on LTV, seasoning, and payment history
    double base_pd = 0.02; // 2% base probability
    
    // Adjust for LTV
    if (loan_to_value_ratio_ > 1.0) base_pd *= 2.0;
    else if (loan_to_value_ratio_ > 0.9) base_pd *= 1.5;
    
    // Adjust for payment history
    if (payments_missed_ > 0) {
        base_pd *= (1.0 + payments_missed_ * 0.5);
    }
    
    // Adjust for seasoning (newer loans are riskier)
    double seasoning = getSeasoningMonths();
    if (seasoning < 12) base_pd *= 1.5;
    
    return std::min(base_pd, 1.0);
}

Amount AutoLoan::calculateExpectedLoss() const {
    double pd = calculateProbabilityOfDefault();
    Amount exposure = current_balance_;
    Amount recovery = estimateRecoveryValue();
    double lgd = std::max(0.0, (exposure - recovery) / exposure); // Loss Given Default
    
    return exposure * pd * lgd;
}

double AutoLoan::getRiskScore() const {
    // Risk score from 0-1000, higher = riskier
    double score = 500.0; // Base score
    
    // Adjust for LTV
    score += (loan_to_value_ratio_ - 0.8) * 200.0;
    
    // Adjust for payment history
    score += payments_missed_ * 50.0;
    
    // Adjust for vehicle age
    int vehicle_age = Date() - Date(); // Simplified
    score += vehicle_age * 5.0;
    
    return std::max(0.0, std::min(score, 1000.0));
}

bool AutoLoan::isHighRisk() const {
    return getRiskScore() > 700.0 || payments_missed_ > 2 || loan_to_value_ratio_ > 1.1;
}

Amount AutoLoan::calculateMonthlyPayment() const {
    if (current_rate_ <= 0 || remaining_terms_ <= 0) return 0.0;
    
    double monthly_rate = current_rate_ / 12.0;
    double factor = std::pow(1.0 + monthly_rate, remaining_terms_);
    
    return current_balance_ * (monthly_rate * factor) / (factor - 1.0);
}

Amount AutoLoan::calculateRemainingInterest() const {
    return (monthly_payment_ * remaining_terms_) - current_balance_;
}

std::vector<Date> AutoLoan::getPaymentSchedule(int num_payments) const {
    std::vector<Date> schedule;
    Date current_date = next_payment_date_;
    
    for (int i = 0; i < std::min(num_payments, remaining_terms_); ++i) {
        schedule.push_back(current_date);
        current_date = current_date + 30; // Monthly
    }
    
    return schedule;
}

Amount AutoLoan::getNextPaymentAmount() const {
    return monthly_payment_;
}

Date AutoLoan::getNextPaymentDate() const {
    return next_payment_date_;
}

double AutoLoan::getPaymentToIncomeRatio() const {
    // Would need borrower income information
    // Simplified implementation
    return 0.15; // Assume 15% payment-to-income ratio
}

int AutoLoan::getDaysDelinquent() const {
    if (status_ != Status::Current && status_ != Status::Defaulted) return 0;
    
    Date current_date = Date(); // Would use actual current date
    if (current_date > next_payment_date_) {
        return current_date - next_payment_date_;
    }
    return 0;
}

bool AutoLoan::isNearMaturity(int months_threshold) const {
    return remaining_terms_ <= months_threshold;
}

double AutoLoan::getSeasoningMonths() const {
    Date current_date = Date(); // Would use actual current date
    return current_date - origination_date_;
}

} // namespace Structura