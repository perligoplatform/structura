#include "assets/mortgage.h"
#include "core/types.h"
#include <cmath>
#include <algorithm>
#include <stdexcept>
#include <ql/time/period.hpp>

namespace Structura {

// Mortgage Implementation
void Mortgage::makePayment(Balance principalPayment, Balance interestPayment, Date paymentDate) {
    if (principalPayment < 0 || interestPayment < 0) {
        throw std::invalid_argument("Payment amounts cannot be negative");
    }
    
    if (principalPayment > currentBalance_) {
        principalPayment = currentBalance_;
    }
    
    Balance oldBalance = currentBalance_;
    currentBalance_ -= principalPayment;
    remainingTerms_ = std::max(0, remainingTerms_ - 1);
    
    // Update borrower number based on balance reduction
    updateBorrowerNumber(oldBalance, currentBalance_);
}

void Mortgage::applyDefault(Date defaultDate) {
    status_ = Status::Defaulted;
    // In a default, the remaining balance becomes the loss
    // In practice, this would trigger recovery processes
}

void Mortgage::applyPrepayment(Balance amount, Date paymentDate) {
    if (amount <= 0) return;
    
    Balance oldBalance = currentBalance_;
    Balance prepayAmount = std::min(amount, currentBalance_);
    currentBalance_ -= prepayAmount;
    
    // Update borrower number based on balance reduction
    updateBorrowerNumber(oldBalance, currentBalance_);
    
    // If fully prepaid, set remaining terms to 0
    if (currentBalance_ <= 0.01) { // Small tolerance for floating point
        remainingTerms_ = 0;
        currentBalance_ = 0.0;
    }
}

std::pair<Balance, Balance> Mortgage::calculateNextPayment() const {
    if (remainingTerms_ <= 0 || currentBalance_ <= 0) {
        return {0.0, 0.0};
    }
    
    // Convert annual rate to periodic rate based on payment frequency
    Rate periodicRate = currentRate_;
    switch (getPeriod()) {
        case Period::Monthly:
            periodicRate = currentRate_ / 12.0;
            break;
        case Period::Quarterly:
            periodicRate = currentRate_ / 4.0;
            break;
        case Period::SemiAnnually:
            periodicRate = currentRate_ / 2.0;
            break;
        case Period::Annually:
            // Keep annual rate as is
            break;
        default:
            // For other periods, assume annual (safe default)
            break;
    }
    
    // Use the asset base calculation with current mortgage parameters
    std::pair<Balance, int> amortInfo = {currentBalance_, remainingTerms_};
    
    return calculatePrincipalInterest(
        getAmortPlan(), 
        currentBalance_, 
        periodicRate, 
        getOriginTerms(), 
        remainingTerms_, 
        amortInfo
    );
}

Mortgage Mortgage::resetToOriginal() const {
    return Mortgage(
        originalInfo_,
        getOriginBalance(),
        getOriginRate(),
        getOriginTerms(),
        borrowerNumber_.value_or(1),
        Status::Current
    );
}

std::vector<Date> Mortgage::getPaymentDates(int extraPeriods) const {
    std::vector<Date> dates;
    Date currentDate = getOriginDate();
    int totalPeriods = getOriginTerms() + extraPeriods;
    
    for (int i = 0; i < totalPeriods; ++i) {
        // Add period to current date based on payment frequency
        switch (getPeriod()) {
            case Period::Monthly:
                currentDate = currentDate + QuantLib::Period(1, QuantLib::Months);
                break;
            case Period::Quarterly:
                currentDate = currentDate + QuantLib::Period(3, QuantLib::Months);
                break;
            case Period::SemiAnnually:
                currentDate = currentDate + QuantLib::Period(6, QuantLib::Months);
                break;
            case Period::Annually:
                currentDate = currentDate + QuantLib::Period(1, QuantLib::Years);
                break;
        }
        dates.push_back(currentDate);
    }
    
    return dates;
}

void Mortgage::updateBorrowerNumber(Balance oldBalance, Balance newBalance) {
    if (!borrowerNumber_.has_value() || oldBalance <= 0) return;
    
    // Proportionally reduce borrower number based on balance reduction
    double reductionRatio = static_cast<double>(newBalance) / static_cast<double>(oldBalance);
    int newBorrowerNum = static_cast<int>(std::ceil(borrowerNumber_.value() * reductionRatio));
    borrowerNumber_ = std::max(0, newBorrowerNum);
}

// AdjustableRateMortgage Implementation
bool AdjustableRateMortgage::isInInitialPeriod() const {
    int periodsPassed = getOriginTerms() - getRemainingTerms();
    return periodsPassed < armInfo_.getInitPeriod();
}

Rate AdjustableRateMortgage::calculateAdjustedRate(Rate indexRate, Date adjustmentDate) const {
    Rate newRate = indexRate; // This would include spread calculation in full implementation
    
    // Apply caps and floors
    if (!isInInitialPeriod()) {
        // Apply periodic cap
        Rate maxIncrease = getCurrentRate() + armInfo_.getPeriodicCap();
        Rate maxDecrease = getCurrentRate() - armInfo_.getPeriodicCap();
        newRate = std::min(maxIncrease, std::max(maxDecrease, newRate));
    } else {
        // Apply initial cap
        Rate maxInitialRate = getOriginRate() + armInfo_.getInitCap();
        newRate = std::min(maxInitialRate, newRate);
    }
    
    // Apply lifetime caps
    newRate = std::min(armInfo_.getLifeCap(), std::max(armInfo_.getLifeFloor(), newRate));
    
    return newRate;
}

std::vector<Date> AdjustableRateMortgage::getRateAdjustmentDates() const {
    std::vector<Date> adjustmentDates;
    std::vector<Date> paymentDates = getPaymentDates();
    
    // First adjustment after initial period
    int initPeriod = armInfo_.getInitPeriod();
    if (initPeriod < static_cast<int>(paymentDates.size())) {
        adjustmentDates.push_back(paymentDates[initPeriod]);
        
        // Subsequent adjustments (typically annual for most ARMs)
        for (size_t i = initPeriod + 12; i < paymentDates.size(); i += 12) {
            adjustmentDates.push_back(paymentDates[i]);
        }
    }
    
    return adjustmentDates;
}

// MortgageUtils Implementation
namespace MortgageUtils {

Amount calculateMortgagePayment(Balance balance, Rate rate, int periods) {
    return calculatePayment(balance, rate, periods);
}

std::pair<Balance, Balance> calculatePaymentBreakdown(
    const Mortgage& mortgage, Rate currentRate) {
    
    std::pair<Balance, int> amortInfo = {mortgage.getCurrentBalance(), mortgage.getRemainingTerms()};
    
    return calculatePrincipalInterest(
        mortgage.getAmortPlan(),
        mortgage.getCurrentBalance(),
        currentRate,
        mortgage.getOriginTerms(),
        mortgage.getRemainingTerms(),
        amortInfo
    );
}

std::vector<AmortizationRow> generateAmortizationSchedule(
    const Mortgage& mortgage, int periods) {
    
    std::vector<AmortizationRow> schedule;
    
    if (periods == 0) {
        periods = mortgage.getRemainingTerms();
    }
    
    Balance balance = mortgage.getCurrentBalance();
    Rate rate = mortgage.getCurrentRate();
    
    // Convert annual rate to periodic rate based on payment frequency
    switch (mortgage.getPeriod()) {
        case Period::Monthly:
            rate = rate / 12.0;
            break;
        case Period::Quarterly:
            rate = rate / 4.0;
            break;
        case Period::SemiAnnually:
            rate = rate / 2.0;
            break;
        case Period::Annually:
            // Keep annual rate as is
            break;
        default:
            // For other periods, assume annual (safe default)
            break;
    }
    
    std::vector<Date> paymentDates = mortgage.getPaymentDates(periods);
    
    for (int i = 0; i < periods && balance > 0.01; ++i) {
        Balance beginningBalance = balance;
        
        // Calculate payment breakdown using mortgage's remaining terms, not schedule periods
        int remainingMortgageTerms = mortgage.getRemainingTerms() - i;
        std::pair<Balance, int> amortInfo = {balance, remainingMortgageTerms};
        auto [interestPayment, principalPayment] = calculatePrincipalInterest(
            mortgage.getAmortPlan(), balance, rate, 
            mortgage.getOriginTerms(), remainingMortgageTerms, amortInfo
        );
        
        Balance totalPayment = interestPayment + principalPayment;
        balance -= principalPayment;
        
        // Ensure balance doesn't go negative
        if (balance < 0) {
            principalPayment += balance;
            balance = 0;
        }
        
        Date paymentDate = (i < static_cast<int>(paymentDates.size())) ? 
                          paymentDates[i] : 
                          DateUtils::makeDate(2024, 1, 1); // Fallback date
        
        schedule.push_back({
            paymentDate,
            beginningBalance,
            totalPayment,
            principalPayment,
            interestPayment,
            balance
        });
    }
    
    return schedule;
}

} // namespace MortgageUtils

} // namespace Structura