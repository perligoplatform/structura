#pragma once

#include "assets/asset_base.h"
#include <memory>

namespace Structura {

// ARM (Adjustable Rate Mortgage) parameters
class ARMInfo {
private:
    int initPeriod_;
    Rate initCap_;
    Rate periodicCap_;
    Rate lifeCap_;
    Rate lifeFloor_;

public:
    ARMInfo(int initPeriod, Rate initCap, Rate periodicCap, Rate lifeCap, Rate lifeFloor)
        : initPeriod_(initPeriod), initCap_(initCap), periodicCap_(periodicCap)
        , lifeCap_(lifeCap), lifeFloor_(lifeFloor) {}
        
    int getInitPeriod() const { return initPeriod_; }
    Rate getInitCap() const { return initCap_; }
    Rate getPeriodicCap() const { return periodicCap_; }
    Rate getLifeCap() const { return lifeCap_; }
    Rate getLifeFloor() const { return lifeFloor_; }
};

// Base Mortgage class - first concrete asset type
class Mortgage {
private:
    OriginalInfo originalInfo_;
    Balance currentBalance_;
    Rate currentRate_;
    int remainingTerms_;
    std::optional<int> borrowerNumber_;
    Status status_;

public:
    // Standard mortgage constructor
    Mortgage(const OriginalInfo& originalInfo, Balance currentBalance, 
             Rate currentRate, int remainingTerms, Status status = Status::Current)
        : originalInfo_(originalInfo), currentBalance_(currentBalance)
        , currentRate_(currentRate), remainingTerms_(remainingTerms)
        , status_(status) {}
        
    // Constructor with borrower number
    Mortgage(const OriginalInfo& originalInfo, Balance currentBalance, 
             Rate currentRate, int remainingTerms, int borrowerNumber, 
             Status status = Status::Current)
        : originalInfo_(originalInfo), currentBalance_(currentBalance)
        , currentRate_(currentRate), remainingTerms_(remainingTerms)
        , borrowerNumber_(borrowerNumber), status_(status) {}

    // Getters
    const OriginalInfo& getOriginalInfo() const { return originalInfo_; }
    Balance getCurrentBalance() const { return currentBalance_; }
    Balance getOriginBalance() const { return originalInfo_.getOriginBalance(); }
    Rate getCurrentRate() const { return currentRate_; }
    Rate getOriginRate() const { return originalInfo_.getOriginRate(); }
    int getRemainingTerms() const { return remainingTerms_; }
    int getOriginTerms() const { return originalInfo_.getOriginTerm(); }
    Date getOriginDate() const { return originalInfo_.getStartDate(); }
    Period getPeriod() const { return originalInfo_.getPeriod(); }
    AmortPlan getAmortPlan() const { return originalInfo_.getPrinType(); }
    Status getStatus() const { return status_; }
    
    std::optional<int> getBorrowerNumber() const { return borrowerNumber_; }
    int getBorrowerCount() const { return borrowerNumber_.value_or(1); }
    
    // Status queries
    bool isDefaulted() const { return status_ == Status::Defaulted; }
    bool isCurrent() const { return status_ == Status::Current; }
    
    // Balance operations
    void makePayment(Balance principalPayment, Balance interestPayment, Date paymentDate);
    void applyDefault(Date defaultDate);
    void applyPrepayment(Balance amount, Date paymentDate);
    
    // Payment calculation - calculates next payment amounts
    std::pair<Balance, Balance> calculateNextPayment() const;
    
    // Reset to original terms (for analysis)
    Mortgage resetToOriginal() const;
    
    // Generate payment dates
    std::vector<Date> getPaymentDates(int extraPeriods = 0) const;
    
    // Update operations
    void updateRate(Rate newRate) { currentRate_ = newRate; }
    void updateBalance(Balance newBalance) { currentBalance_ = newBalance; }
    void updateStatus(Status newStatus) { status_ = newStatus; }
    void setBorrowerNumber(int borrowerNum) { borrowerNumber_ = borrowerNum; }
    
private:
    // Helper function to decrease borrower count based on balance reduction
    void updateBorrowerNumber(Balance oldBalance, Balance newBalance);
};

// Adjustable Rate Mortgage - extends basic mortgage with ARM features
class AdjustableRateMortgage : public Mortgage {
private:
    ARMInfo armInfo_;

public:
    AdjustableRateMortgage(const OriginalInfo& originalInfo, const ARMInfo& armInfo,
                          Balance currentBalance, Rate currentRate, int remainingTerms,
                          Status status = Status::Current)
        : Mortgage(originalInfo, currentBalance, currentRate, remainingTerms, status)
        , armInfo_(armInfo) {}
        
    AdjustableRateMortgage(const OriginalInfo& originalInfo, const ARMInfo& armInfo,
                          Balance currentBalance, Rate currentRate, int remainingTerms,
                          int borrowerNumber, Status status = Status::Current)
        : Mortgage(originalInfo, currentBalance, currentRate, remainingTerms, borrowerNumber, status)
        , armInfo_(armInfo) {}

    const ARMInfo& getARMInfo() const { return armInfo_; }
    
    // ARM-specific operations
    bool isInInitialPeriod() const;
    Rate calculateAdjustedRate(Rate indexRate, Date adjustmentDate) const;
    std::vector<Date> getRateAdjustmentDates() const;
};

// Utility functions for mortgage operations
namespace MortgageUtils {
    // Calculate mortgage payment for given parameters
    Amount calculateMortgagePayment(Balance balance, Rate rate, int periods);
    
    // Calculate principal and interest breakdown
    std::pair<Balance, Balance> calculatePaymentBreakdown(
        const Mortgage& mortgage, Rate currentRate);
        
    // Generate amortization schedule
    struct AmortizationRow {
        Date paymentDate;
        Balance beginningBalance;
        Balance payment;
        Balance principalPayment;
        Balance interestPayment;
        Balance endingBalance;
    };
    
    std::vector<AmortizationRow> generateAmortizationSchedule(
        const Mortgage& mortgage, int periods = 0);
}

} // namespace Structura