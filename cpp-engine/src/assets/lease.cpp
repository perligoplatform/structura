#include "lease.h"
#include <algorithm>
#include <cmath>
#include <stdexcept>

namespace Structura {

// LeaseType conversion
std::string leaseTypeToString(LeaseType type) {
    switch (type) {
        case LeaseType::EQUIPMENT: return "Equipment";
        case LeaseType::VEHICLE: return "Vehicle";
        case LeaseType::AIRCRAFT: return "Aircraft";
        case LeaseType::REAL_ESTATE: return "Real Estate";
        case LeaseType::TECHNOLOGY: return "Technology";
        case LeaseType::MEDICAL: return "Medical";
        case LeaseType::OTHER: return "Other";
        default: return "Unknown";
    }
}

// LeaseStructure conversion
std::string leaseStructureToString(LeaseStructure structure) {
    switch (structure) {
        case LeaseStructure::CAPITAL: return "Capital";
        case LeaseStructure::OPERATING: return "Operating";
        case LeaseStructure::SYNTHETIC: return "Synthetic";
        case LeaseStructure::LEVERAGED: return "Leveraged";
        default: return "Unknown";
    }
}

// Equipment implementation
Equipment::Equipment(const std::string& id, const std::string& mfg, const std::string& model_name,
                     Amount cost, Amount residual, int life_months)
    : equipment_id(id), manufacturer(mfg), model(model_name), 
      original_cost(cost), estimated_residual_value(residual), useful_life_months(life_months) {
    
    current_value = original_cost;
    manufacture_date = Date(); // Would be set properly in practice
    depreciation_rate = life_months > 0 ? 1.0 / life_months : 0.0;
}

Amount Equipment::calculateStraightLineDepreciation(int months_elapsed) const {
    if (useful_life_months <= 0) return 0.0;
    
    double depreciation_per_month = (original_cost - estimated_residual_value) / useful_life_months;
    return std::min(depreciation_per_month * months_elapsed, 
                    original_cost - estimated_residual_value);
}

Amount Equipment::calculateAcceleratedDepreciation(int months_elapsed, double factor) const {
    if (useful_life_months <= 0) return 0.0;
    
    // Double declining balance method
    double monthly_rate = factor / useful_life_months;
    Amount book_value = original_cost;
    
    for (int i = 0; i < months_elapsed; ++i) {
        Amount depreciation = book_value * monthly_rate;
        book_value = std::max(book_value - depreciation, estimated_residual_value);
    }
    
    return original_cost - book_value;
}

Amount Equipment::getCurrentBookValue(int months_elapsed) const {
    Amount depreciation = calculateStraightLineDepreciation(months_elapsed);
    return original_cost - depreciation;
}

Amount Equipment::getEstimatedRemainingValue(int months_remaining) const {
    int total_months = useful_life_months;
    int months_elapsed = total_months - months_remaining;
    return getCurrentBookValue(months_elapsed);
}

// Lessee implementation
Lessee::Lessee(const std::string& id, const std::string& name, const std::string& industry_sector)
    : lessee_id(id), company_name(name), industry(industry_sector),
      credit_score(700.0), annual_revenue(1000000.0), years_in_business(5) {
    credit_rating = "BBB";
}

double Lessee::getCreditRiskScore() const {
    // Simplified credit risk scoring
    double score = 0.0;
    
    // Credit score component (40% weight)
    score += (credit_score / 850.0) * 0.4;
    
    // Revenue stability (30% weight)
    if (annual_revenue > 10000000) score += 0.3;
    else if (annual_revenue > 1000000) score += 0.2;
    else score += 0.1;
    
    // Business maturity (30% weight)
    if (years_in_business > 10) score += 0.3;
    else if (years_in_business > 5) score += 0.2;
    else score += 0.1;
    
    return std::min(score, 1.0);
}

bool Lessee::isInvestmentGrade() const {
    return credit_rating <= "BBB-" && credit_score >= 650;
}

// LeasePaymentSchedule implementation
void LeasePaymentSchedule::generateSchedule(Date start_date, Date end_date, Amount monthly_payment, 
                                           PaymentFrequency frequency) {
    payment_dates.clear();
    payment_amounts.clear();
    
    int increment_days = 30;  // Default monthly
    switch (frequency) {
        case PaymentFrequency::Monthly: increment_days = 30; break;
        case PaymentFrequency::Quarterly: increment_days = 90; break;
        case PaymentFrequency::SemiAnnually: increment_days = 180; break;
        case PaymentFrequency::Annually: increment_days = 365; break;
        default: increment_days = 30; break;
    }
    
    Date current_date = start_date;
    while (current_date <= end_date) {
        payment_dates.push_back(current_date);
        payment_amounts.push_back(monthly_payment);
        current_date = current_date + increment_days;
    }
}

Amount LeasePaymentSchedule::getTotalPayments() const {
    Amount total = 0.0;
    for (Amount payment : payment_amounts) {
        total += payment;
    }
    return total + security_deposit + end_of_term_payment;
}

Amount LeasePaymentSchedule::getOutstandingPayments(Date as_of_date) const {
    Amount outstanding = 0.0;
    for (size_t i = 0; i < payment_dates.size(); ++i) {
        if (payment_dates[i] > as_of_date) {
            outstanding += payment_amounts[i];
        }
    }
    return outstanding;
}

// Lease implementation
Lease::Lease(const std::string& id, LeaseType type, LeaseStructure structure,
             const Equipment& equipment, const Lessee& lessee,
             Date start_date, int term_months, Amount monthly_payment)
    : lease_id_(id), lease_type_(type), lease_structure_(structure), 
      equipment_(equipment), lessee_(lessee), status_(Status::Current),
      lease_start_date_(start_date), lease_term_months_(term_months), 
      monthly_payment_(monthly_payment) {
    
    // Calculate lease end date
    lease_end_date_ = lease_start_date_ + (term_months * 30); // Approximate
    
    // Initialize financial tracking
    total_payments_received_ = 0.0;
    outstanding_lease_payments_ = monthly_payment * term_months;
    payments_missed_ = 0;
    is_in_default_ = false;
    
    // Initialize residual values
    estimated_residual_value_ = equipment.estimated_residual_value;
    guaranteed_residual_value_ = estimated_residual_value_ * 0.8; // 80% guarantee typical
    residual_value_guarantee_percentage_ = 0.8;
    
    // Initialize options
    has_purchase_option_ = false;
    has_renewal_option_ = false;
    purchase_option_price_ = 0.0;
    renewal_term_months_ = 0;
    security_deposit_ = monthly_payment; // Typical 1 month deposit
    
    // Generate payment schedule
    payment_schedule_.generateSchedule(lease_start_date_, lease_end_date_, 
                                     monthly_payment, PaymentFrequency::Monthly);
}

void Lease::makePayment(Balance principalPayment, Balance interestPayment, Date paymentDate) {
    // For leases, treat the total as lease payment
    Amount total_payment = principalPayment + interestPayment;
    processLeasePayment(total_payment, paymentDate);
}

void Lease::applyDefault(Date defaultDate) {
    is_in_default_ = true;
    setStatus(Status::Defaulted);
    last_payment_date_ = defaultDate;
}

void Lease::applyPrepayment(Balance amount, Date paymentDate) {
    // Early termination with payment
    processEarlyTermination(paymentDate, amount);
}

Balance Lease::getCurrentBalance() const {
    return outstanding_lease_payments_;
}

Rate Lease::getCurrentRate() const {
    // Lease doesn't have traditional interest rate
    // Return implied rate based on payments vs equipment value
    if (equipment_.original_cost > 0) {
        Amount total_lease_payments = monthly_payment_ * lease_term_months_;
        return (total_lease_payments - equipment_.original_cost) / equipment_.original_cost;
    }
    return 0.0;
}

void Lease::resetToOriginal() {
    total_payments_received_ = 0.0;
    outstanding_lease_payments_ = monthly_payment_ * lease_term_months_;
    payments_missed_ = 0;
    is_in_default_ = false;
    setStatus(Status::Current);
    last_payment_date_ = Date();
}

void Lease::processLeasePayment(Amount payment_amount, Date payment_date) {
    if (payment_amount <= 0) return;
    
    total_payments_received_ += payment_amount;
    outstanding_lease_payments_ = std::max(0.0, outstanding_lease_payments_ - payment_amount);
    last_payment_date_ = payment_date;
    
    // Reset missed payments if catching up
    if (payment_amount >= monthly_payment_) {
        payments_missed_ = std::max(0, payments_missed_ - 1);
        if (payments_missed_ == 0 && is_in_default_) {
            is_in_default_ = false;
            setStatus(Status::Current);
        }
    }
}

void Lease::processEarlyTermination(Date termination_date, Amount termination_payment) {
    // Calculate remaining payments
    int months_remaining = std::max(0, (int)((lease_end_date_ - termination_date) / 30));
    Amount remaining_payments = monthly_payment_ * months_remaining;
    
    // Apply termination payment
    total_payments_received_ += termination_payment;
    outstanding_lease_payments_ = std::max(0.0, remaining_payments - termination_payment);
    
    // Update lease end date
    lease_end_date_ = termination_date;
    setStatus(Status::PaidOff);
}

void Lease::exercisePurchaseOption(Date exercise_date) {
    if (!has_purchase_option_) {
        throw std::runtime_error("No purchase option available");
    }
    
    // Process purchase
    total_payments_received_ += purchase_option_price_;
    outstanding_lease_payments_ = 0.0;
    lease_end_date_ = exercise_date;
    setStatus(Status::PaidOff);
}

void Lease::processRenewal(int additional_months, Amount new_monthly_payment) {
    if (!has_renewal_option_) {
        throw std::runtime_error("No renewal option available");
    }
    
    // Extend lease
    lease_term_months_ += additional_months;
    lease_end_date_ = lease_end_date_ + (additional_months * 30);
    monthly_payment_ = new_monthly_payment;
    outstanding_lease_payments_ += new_monthly_payment * additional_months;
    
    // Regenerate payment schedule
    payment_schedule_.generateSchedule(lease_start_date_, lease_end_date_, 
                                     monthly_payment_, PaymentFrequency::Monthly);
}

void Lease::updateEquipmentValue(Amount new_value, Date valuation_date) {
    equipment_.current_value = new_value;
    
    // Update estimated residual if significant change
    double value_change_ratio = new_value / equipment_.original_cost;
    if (value_change_ratio < 0.7 || value_change_ratio > 1.3) {
        estimated_residual_value_ = equipment_.estimated_residual_value * value_change_ratio;
    }
}

void Lease::updateResidualValue(Amount new_residual_estimate) {
    estimated_residual_value_ = new_residual_estimate;
}

Amount Lease::calculateCurrentEquipmentValue() const {
    // Calculate based on age and depreciation
    Date current_date = Date(); // Would use actual current date
    int months_elapsed = std::max(0, (int)((current_date - lease_start_date_) / 30));
    
    return equipment_.getCurrentBookValue(months_elapsed);
}

Amount Lease::calculateExpectedResidualValue() const {
    int months_remaining = std::max(0, (int)((lease_end_date_ - Date()) / 30));
    return equipment_.getEstimatedRemainingValue(months_remaining);
}

std::vector<Date> Lease::getPaymentDates(int numPayments) const {
    std::vector<Date> dates;
    Date current_date = lease_start_date_;
    
    for (int i = 0; i < numPayments && current_date <= lease_end_date_; ++i) {
        dates.push_back(current_date);
        current_date = current_date + 30; // Monthly
    }
    
    return dates;
}

Amount Lease::getNextPaymentAmount() const {
    return monthly_payment_;
}

Date Lease::getNextPaymentDate() const {
    Date current_date = Date(); // Would use actual current date
    
    for (const Date& payment_date : payment_schedule_.payment_dates) {
        if (payment_date > current_date) {
            return payment_date;
        }
    }
    
    return lease_end_date_;
}

Amount Lease::getOutstandingPayments() const {
    return outstanding_lease_payments_;
}

double Lease::getPaymentToValueRatio() const {
    if (equipment_.current_value > 0) {
        return monthly_payment_ / equipment_.current_value;
    }
    return 0.0;
}

double Lease::getResidualRiskRatio() const {
    if (equipment_.original_cost > 0) {
        return estimated_residual_value_ / equipment_.original_cost;
    }
    return 0.0;
}

int Lease::getDaysDelinquent() const {
    if (!is_in_default_ || payments_missed_ == 0) return 0;
    
    Date current_date = Date(); // Would use actual current date
    Date expected_payment_date = getNextPaymentDate() - 30; // Previous payment
    
    return std::max(0, (int)(current_date - expected_payment_date));
}

bool Lease::isNearMaturity(int months_threshold) const {
    Date current_date = Date(); // Would use actual current date
    int months_to_maturity = (int)((lease_end_date_ - current_date) / 30);
    
    return months_to_maturity <= months_threshold;
}

} // namespace Structura