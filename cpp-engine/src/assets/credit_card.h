#pragma once

#include "asset_base.h"
#include <string>
#include <vector>
#include <memory>

namespace Structura {

// Credit utilization categories for risk assessment
enum class UtilizationCategory {
    LOW_UTILIZATION = 0,    // 0-30%
    MODERATE_UTILIZATION,   // 30-60%
    HIGH_UTILIZATION,       // 60-80%
    MAXED_OUT              // 80%+
};

// Credit card account information
struct CreditCardAccount {
    std::string account_number;        // Unique account identifier
    std::string product_type;          // Rewards, cashback, premium, etc.
    Amount credit_limit;              // Maximum credit line
    Amount available_credit;          // Remaining available credit
    Amount cash_advance_limit;        // Cash advance limit
    Rate apr;                        // Annual percentage rate
    Rate cash_advance_apr;           // Cash advance APR
    Rate penalty_apr;                // Penalty APR for late payments
    int grace_period_days;           // Interest-free period
    Amount minimum_payment_percent;   // Minimum payment as % of balance
    Amount minimum_payment_floor;     // Minimum payment floor amount
    
    CreditCardAccount(const std::string& account_num, const std::string& product,
                     Amount limit, Rate annual_rate, int grace_days = 25,
                     Amount min_pmt_pct = 0.02, Amount min_pmt_floor = 25.0);
};

class CreditCard {
private:
    std::unique_ptr<CreditCardAccount> account;
    Amount current_balance;           // Outstanding balance
    Amount available_credit;          // Available credit remaining
    int days_past_due;               // Days past due for payments
    Date last_payment_date;          // Last payment received date
    Date last_statement_date;        // Last statement generation date
    Amount last_minimum_payment;     // Last calculated minimum payment
    bool is_overlimit;               // Whether account is over limit
    int consecutive_minimum_payments; // Consecutive minimum payments made
    
    // Basic asset fields
    Date originationDate;
    Status status;
    Obligor obligor;
    
public:
    // Constructor
    CreditCard(const std::string& account_number, const std::string& product_type,
               Amount credit_limit, Rate apr, Date origination_date,
               const std::string& obligor_id, double initial_fico = 720.0);
    
    // Destructor
    virtual ~CreditCard() = default;
    
    // Basic asset interface methods
    Amount getCurrentBalance() const;
    Amount getOriginalBalance() const;
    Rate getCurrentRate() const;
    Date getOriginationDate() const;
    Date getMaturityDate() const;
    Status getStatus() const;
    
    // Credit card specific methods
    Amount getAvailableCredit() const;
    Amount getCreditLimit() const;
    Rate getAPR() const;
    UtilizationCategory getUtilizationCategory() const;
    Amount getMinimumPayment() const;
    
    // Payment processing
    void makePayment(Amount payment_amount, Date payment_date);
    void makeMinimumPayment(Date payment_date);
    
    // Transaction processing
    void processTransaction(Amount transaction_amount, Date transaction_date, 
                          const std::string& description = "Purchase");
    void processCashAdvance(Amount advance_amount, Date advance_date);
    
    // Account management
    void applyInterestCharges(Date calculation_date);
    void applyLateFees(Date fee_date);
    void updateCreditLimit(Amount new_limit, Date effective_date);
    void generateStatement(Date statement_date);
    
    // Risk assessment
    double calculateProbabilityOfDefault() const;
    double calculateLossGivenDefault() const;
    double calculateExpectedLoss() const;
    
    // Credit utilization analysis
    double getUtilizationRatio() const;
    bool isHighRiskAccount() const;
    
    // Delinquency management
    void applyDefault(Date default_date);
    void processChargeOff(Amount charge_off_amount, Date charge_off_date);
    void processRecovery(Amount recovery_amount, Date recovery_date);
    
    // Account status management
    void freezeAccount(Date freeze_date, const std::string& reason);
    void closeAccount(Date close_date, const std::string& reason);
    
    // Reporting
    std::string getAccountSummary() const;
    double getAverageUtilization(int months = 12) const;
    
private:
    // Helper methods
    Amount calculateInterestCharges(Date from_date, Date to_date) const;
    Amount calculateMinimumPayment() const;
    void updateAvailableCredit();
    void checkOverlimitStatus();
    bool isInGracePeriod(Date transaction_date) const;
    void applyPaymentAllocation(Amount payment_amount);
};

} // namespace Structura