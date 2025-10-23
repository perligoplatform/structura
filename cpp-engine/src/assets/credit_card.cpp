#include "credit_card.h"
#include <stdexcept>
#include <sstream>
#include <algorithm>
#include <cmath>

namespace Structura {

// CreditCardAccount Implementation
CreditCardAccount::CreditCardAccount(const std::string& account_num, const std::string& product,
                                   Amount limit, Rate annual_rate, int grace_days,
                                   Amount min_pmt_pct, Amount min_pmt_floor)
    : account_number(account_num), product_type(product), credit_limit(limit),
      available_credit(limit), cash_advance_limit(limit * 0.5), // Typically 50% of credit limit
      apr(annual_rate), cash_advance_apr(annual_rate + 0.05), // Typically 5% higher
      penalty_apr(annual_rate + 0.10), // Typically 10% higher for penalties
      grace_period_days(grace_days), minimum_payment_percent(min_pmt_pct),
      minimum_payment_floor(min_pmt_floor) {
    
    if (limit <= 0) {
        throw std::invalid_argument("Credit limit must be positive");
    }
    if (annual_rate < 0) {
        throw std::invalid_argument("APR cannot be negative");
    }
    if (grace_days < 0) {
        throw std::invalid_argument("Grace period cannot be negative");
    }
}

// CreditCard Implementation
CreditCard::CreditCard(const std::string& account_number, const std::string& product_type,
                      Amount credit_limit, Rate apr, Date origination_date,
                      const std::string& obligor_id, double initial_fico)
    : account(std::make_unique<CreditCardAccount>(account_number, product_type, 
                                                credit_limit, apr)),
      current_balance(0.0), available_credit(credit_limit), days_past_due(0),
      last_payment_date(origination_date), last_statement_date(origination_date),
      last_minimum_payment(0.0), is_overlimit(false), consecutive_minimum_payments(0),
      originationDate(origination_date), status(Status::Current), obligor(obligor_id) {
    
    // Set up obligor FICO score
    obligor.setField("fico_score", initial_fico);
}

Amount CreditCard::getCurrentBalance() const {
    return current_balance;
}

Amount CreditCard::getOriginalBalance() const {
    // For credit cards, original balance is the credit limit
    return account->credit_limit;
}

Rate CreditCard::getCurrentRate() const {
    // Return penalty APR if account is delinquent
    if (days_past_due > 60) {
        return account->penalty_apr;
    }
    return account->apr;
}

Date CreditCard::getOriginationDate() const {
    return originationDate;
}

Date CreditCard::getMaturityDate() const {
    // Credit cards don't have fixed maturity dates
    return Date(31, QuantLib::December, 2099);
}

Status CreditCard::getStatus() const {
    if (days_past_due > 180) {
        return Status::Defaulted;
    } else if (days_past_due > 30) {
        return Status::Defaulted;  // Use Defaulted since Delinquent doesn't exist
    }
    return Status::Current;
}

Amount CreditCard::getAvailableCredit() const {
    return available_credit;
}

Amount CreditCard::getCreditLimit() const {
    return account->credit_limit;
}

Rate CreditCard::getAPR() const {
    return getCurrentRate();
}

UtilizationCategory CreditCard::getUtilizationCategory() const {
    double utilization = getUtilizationRatio();
    
    if (utilization <= 0.30) {
        return UtilizationCategory::LOW_UTILIZATION;
    } else if (utilization <= 0.60) {
        return UtilizationCategory::MODERATE_UTILIZATION;
    } else if (utilization <= 0.80) {
        return UtilizationCategory::HIGH_UTILIZATION;
    } else {
        return UtilizationCategory::MAXED_OUT;
    }
}

Amount CreditCard::getMinimumPayment() const {
    return calculateMinimumPayment();
}

void CreditCard::makePayment(Amount payment_amount, Date payment_date) {
    if (payment_amount <= 0) {
        throw std::invalid_argument("Payment amount must be positive");
    }
    
    // Apply payment allocation (typically: fees, interest, then principal)
    applyPaymentAllocation(payment_amount);
    
    // Update available credit
    updateAvailableCredit();
    
    // Update payment history
    last_payment_date = payment_date;
    
    // Check if this was a minimum payment
    Amount minimum_due = calculateMinimumPayment();
    if (payment_amount >= minimum_due) {
        consecutive_minimum_payments++;
        // Reset days past due if paying at least minimum
        if (days_past_due > 0) {
            days_past_due = 0;
            status = Status::Current;
        }
    }
    
    // Check overlimit status
    checkOverlimitStatus();
}

void CreditCard::makeMinimumPayment(Date payment_date) {
    Amount minimum_due = calculateMinimumPayment();
    makePayment(minimum_due, payment_date);
}

void CreditCard::processTransaction(Amount transaction_amount, Date transaction_date, 
                                  const std::string& description) {
    if (transaction_amount <= 0) {
        throw std::invalid_argument("Transaction amount must be positive");
    }
    
    // Check if transaction would exceed credit limit
    if (current_balance + transaction_amount > account->credit_limit * 1.1) { // Allow 10% overlimit
        throw std::runtime_error("Transaction would exceed credit limit");
    }
    
    // Add transaction to balance
    current_balance += transaction_amount;
    
    // Update available credit
    updateAvailableCredit();
    
    // Check overlimit status
    checkOverlimitStatus();
    
    // Apply interest if not in grace period
    if (!isInGracePeriod(transaction_date)) {
        applyInterestCharges(transaction_date);
    }
}

void CreditCard::processCashAdvance(Amount advance_amount, Date advance_date) {
    if (advance_amount <= 0) {
        throw std::invalid_argument("Cash advance amount must be positive");
    }
    
    if (advance_amount > account->cash_advance_limit) {
        throw std::runtime_error("Cash advance exceeds limit");
    }
    
    // Cash advances typically have fees and higher APR
    Amount cash_advance_fee = advance_amount * 0.03; // 3% fee
    processTransaction(advance_amount + cash_advance_fee, advance_date, "Cash Advance");
}

void CreditCard::applyInterestCharges(Date calculation_date) {
    if (current_balance <= 0) {
        return;
    }
    
    // Calculate daily interest charge
    Rate daily_rate = getCurrentRate() / 365.0;
    Amount interest_charge = current_balance * daily_rate;
    
    current_balance += interest_charge;
    updateAvailableCredit();
}

void CreditCard::applyLateFees(Date fee_date) {
    Amount late_fee = std::min(35.0, current_balance * 0.05); // Max $35 or 5% of balance
    current_balance += late_fee;
    updateAvailableCredit();
    
    // Increase days past due
    days_past_due += 30; // Assume monthly billing cycle
    
    if (days_past_due > 30) {
        status = Status::Defaulted;  // Use Defaulted since Delinquent doesn't exist
    }
}

void CreditCard::updateCreditLimit(Amount new_limit, Date effective_date) {
    if (new_limit <= 0) {
        throw std::invalid_argument("Credit limit must be positive");
    }
    
    Amount old_limit = account->credit_limit;
    account->credit_limit = new_limit;
    
    // Update cash advance limit proportionally
    account->cash_advance_limit = new_limit * 0.5;
    
    // Update available credit
    updateAvailableCredit();
    
    // Check overlimit status
    checkOverlimitStatus();
}

void CreditCard::generateStatement(Date statement_date) {
    last_statement_date = statement_date;
    last_minimum_payment = calculateMinimumPayment();
    
    // Apply interest charges for the statement period
    applyInterestCharges(statement_date);
}

double CreditCard::calculateProbabilityOfDefault() const {
    double base_pd = 0.02; // 2% base default rate for credit cards
    double fico_adjustment = 1.0;
    double utilization_adjustment = 1.0;
    double delinquency_adjustment = 1.0;
    
    // FICO adjustment
    auto fico_score = obligor.getDoubleField("fico_score").value_or(720.0);
    if (fico_score < 620) {
        fico_adjustment = 3.0;
    } else if (fico_score < 680) {
        fico_adjustment = 2.0;
    } else if (fico_score > 750) {
        fico_adjustment = 0.5;
    }
    
    // Utilization adjustment
    double utilization = getUtilizationRatio();
    if (utilization > 0.90) {
        utilization_adjustment = 2.5;
    } else if (utilization > 0.70) {
        utilization_adjustment = 1.8;
    } else if (utilization < 0.30) {
        utilization_adjustment = 0.7;
    }
    
    // Delinquency adjustment
    if (days_past_due > 90) {
        delinquency_adjustment = 5.0;
    } else if (days_past_due > 30) {
        delinquency_adjustment = 2.5;
    }
    
    return std::min(0.95, base_pd * fico_adjustment * utilization_adjustment * delinquency_adjustment);
}

double CreditCard::calculateLossGivenDefault() const {
    // Credit cards are unsecured, so LGD is typically high
    double base_lgd = 0.80; // 80% base loss rate
    
    // Adjust based on utilization (higher utilization = higher loss)
    double utilization_adjustment = 1.0 + (getUtilizationRatio() * 0.20);
    
    return std::min(0.95, base_lgd * utilization_adjustment);
}

double CreditCard::calculateExpectedLoss() const {
    return calculateProbabilityOfDefault() * calculateLossGivenDefault() * current_balance;
}

double CreditCard::getUtilizationRatio() const {
    if (account->credit_limit <= 0) {
        return 0.0;
    }
    return current_balance / account->credit_limit;
}

bool CreditCard::isHighRiskAccount() const {
    auto fico_score = obligor.getDoubleField("fico_score").value_or(720.0);
    return (getUtilizationRatio() > 0.80) || 
           (days_past_due > 60) || 
           (fico_score < 620) ||
           (calculateProbabilityOfDefault() > 0.10);
}

void CreditCard::applyDefault(Date default_date) {
    status = Status::Defaulted;
    days_past_due = 180; // Mark as 180+ days past due
    
    // Typically, credit cards are charged off at default
    processChargeOff(current_balance, default_date);
}

void CreditCard::processChargeOff(Amount charge_off_amount, Date charge_off_date) {
    // Charge-off writes off the debt but doesn't eliminate it
    // The balance remains for collection purposes
    status = Status::Defaulted;
    
    // In practice, the balance might be sold to collectors at a discount
    // For modeling purposes, we keep the full balance
}

void CreditCard::processRecovery(Amount recovery_amount, Date recovery_date) {
    if (recovery_amount <= 0) {
        throw std::invalid_argument("Recovery amount must be positive");
    }
    
    // Apply recovery against outstanding balance
    current_balance = std::max(0.0, current_balance - recovery_amount);
    updateAvailableCredit();
}

void CreditCard::freezeAccount(Date freeze_date, const std::string& reason) {
    // Freeze prevents new transactions but allows payments
    // Using Current status since Frozen doesn't exist in the enum
    status = Status::Current;
}

void CreditCard::closeAccount(Date close_date, const std::string& reason) {
    status = Status::PaidOff;  // Use PaidOff for closed accounts
    available_credit = 0.0;
}

std::string CreditCard::getAccountSummary() const {
    std::ostringstream summary;
    summary << "Credit Card Account: " << account->account_number << "\n"
            << "Product Type: " << account->product_type << "\n"
            << "Credit Limit: $" << account->credit_limit << "\n"
            << "Current Balance: $" << current_balance << "\n"
            << "Available Credit: $" << available_credit << "\n"
            << "Utilization: " << (getUtilizationRatio() * 100) << "%\n"
            << "APR: " << (getCurrentRate() * 100) << "%\n"
            << "Status: " << static_cast<int>(getStatus()) << "\n"
            << "Days Past Due: " << days_past_due << "\n"
            << "Minimum Payment: $" << calculateMinimumPayment() << "\n";
    return summary.str();
}

double CreditCard::getAverageUtilization(int months) const {
    // For simplicity, return current utilization
    // In practice, this would track historical utilization
    return getUtilizationRatio();
}

// Private helper methods
Amount CreditCard::calculateInterestCharges(Date from_date, Date to_date) const {
    if (current_balance <= 0) {
        return 0.0;
    }
    
    // Calculate daily interest for the period
    int days = to_date - from_date;  // QuantLib dates subtract to give days directly
    Rate daily_rate = getCurrentRate() / 365.0;
    
    return current_balance * daily_rate * days;
}

Amount CreditCard::calculateMinimumPayment() const {
    if (current_balance <= 0) {
        return 0.0;
    }
    
    // Minimum payment is typically the greater of:
    // 1. A percentage of the balance (e.g., 2%)
    // 2. A minimum floor amount (e.g., $25)
    Amount percentage_payment = current_balance * account->minimum_payment_percent;
    
    return std::max(percentage_payment, account->minimum_payment_floor);
}

void CreditCard::updateAvailableCredit() {
    available_credit = std::max(0.0, account->credit_limit - current_balance);
}

void CreditCard::checkOverlimitStatus() {
    is_overlimit = (current_balance > account->credit_limit);
    
    if (is_overlimit) {
        // Apply overlimit fee
        Amount overlimit_fee = 25.0; // Typical overlimit fee
        current_balance += overlimit_fee;
    }
}

bool CreditCard::isInGracePeriod(Date transaction_date) const {
    // Grace period applies if previous balance was paid in full
    // For simplicity, assume grace period if payment was made recently
    int days_since_payment = transaction_date - last_payment_date;  // QuantLib dates subtract to give days directly
    return (days_since_payment <= account->grace_period_days) && (current_balance == 0);
}

void CreditCard::applyPaymentAllocation(Amount payment_amount) {
    // Typical payment allocation hierarchy:
    // 1. Fees and penalties
    // 2. Interest charges
    // 3. Principal balance
    
    // For simplicity, apply directly to current balance
    current_balance = std::max(0.0, current_balance - payment_amount);
}

} // namespace Structura