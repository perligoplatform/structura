#pragma once

#include "assets/asset_base.h"
#include <memory>
#include <optional>

namespace Structura {

/**
 * Vehicle information for auto loan collateral
 */
struct Vehicle {
    std::string vin;                    // Vehicle Identification Number
    std::string make;                   // Manufacturer (Toyota, Ford, etc.)
    std::string model;                  // Model name
    int year;                          // Model year
    std::string body_style;            // Sedan, SUV, Truck, etc.
    std::string fuel_type;             // Gas, Hybrid, Electric, Diesel
    int mileage;                       // Current odometer reading
    Amount original_msrp;              // Manufacturer's Suggested Retail Price
    Amount purchase_price;             // Actual purchase price
    Amount current_value;              // Current estimated value
    Date last_valuation_date;          // Last appraisal date
    
    Vehicle(const std::string& vin, const std::string& make, const std::string& model,
            int year, Amount msrp, Amount purchase_price)
        : vin(vin), make(make), model(model), year(year), 
          original_msrp(msrp), purchase_price(purchase_price),
          current_value(purchase_price), mileage(0) {
        last_valuation_date = Date(); // Would be set to current date
    }
    
    // Depreciation calculations
    Amount calculateStraightLineDepreciation(int months_elapsed) const;
    Amount calculateAcceleratedDepreciation(int months_elapsed, double factor = 2.0) const;
    void updateMileage(int new_mileage, Date update_date);
    Amount estimateCurrentValue(Date valuation_date) const;
    double getDepreciationRate() const;
};

/**
 * Auto loan types for different financing structures
 */
enum class AutoLoanType {
    NEW_VEHICLE,        // New car financing
    USED_VEHICLE,       // Used car financing
    REFINANCE,          // Refinanced existing loan
    LEASE_BUYOUT,       // Purchase at end of lease
    PRIVATE_PARTY       // Private party sale financing
};

std::string autoLoanTypeToString(AutoLoanType type);

/**
 * Loan-to-Value ratio categories for risk assessment
 */
enum class LTVCategory {
    PRIME,              // LTV <= 80%
    NEAR_PRIME,         // 80% < LTV <= 90%
    SUB_PRIME,          // 90% < LTV <= 100%
    SUPER_PRIME,        // LTV <= 70% with excellent credit
    HIGH_LTV            // LTV > 100%
};

LTVCategory calculateLTVCategory(Amount loan_amount, Amount vehicle_value);

/**
 * Auto loan asset implementation
 */
class AutoLoan {
private:
    std::string loan_id_;
    AutoLoanType loan_type_;
    Vehicle vehicle_;
    Obligor borrower_;
    Status status_;
    
    // Loan terms
    Amount original_balance_;
    Balance current_balance_;
    Rate current_rate_;
    int original_term_months_;
    int remaining_terms_;
    Amount monthly_payment_;
    Date origination_date_;
    Date maturity_date_;
    Date next_payment_date_;
    
    // Collateral tracking
    Amount loan_to_value_ratio_;
    LTVCategory ltv_category_;
    bool has_gap_insurance_;
    bool has_extended_warranty_;
    
    // Payment history
    int payments_made_;
    int payments_missed_;
    Amount total_interest_paid_;
    Amount total_principal_paid_;
    Date last_payment_date_;
    bool is_in_default_;
    
    // Credit enhancement
    Amount down_payment_;
    std::optional<std::string> co_borrower_id_;
    bool has_credit_insurance_;

public:
    AutoLoan(const std::string& id, AutoLoanType type, const Vehicle& vehicle,
             const Obligor& borrower, Amount original_balance, Rate rate,
             int term_months, Date origination_date);
    
    // AssetBase interface
    void makePayment(Balance principalPayment, Balance interestPayment, Date paymentDate);
    void applyDefault(Date defaultDate);
    void applyPrepayment(Balance amount, Date paymentDate);
    Balance getCurrentBalance() const;
    Rate getCurrentRate() const;
    void resetToOriginal();
    
    // Auto loan specific methods
    void updateVehicleValue(Amount new_value, Date valuation_date);
    void updateMileage(int new_mileage, Date update_date);
    double getCurrentLTV() const;
    Amount calculateEquityPosition() const;
    
    // Payment processing
    void processRegularPayment(Amount payment_amount, Date payment_date);
    void processPartialPayment(Amount payment_amount, Date payment_date);
    void applyLateFee(Amount fee_amount, Date fee_date);
    void processPayoff(Amount payoff_amount, Date payoff_date);
    
    // Default and recovery
    void initiateRepossession(Date repo_date);
    void processLiquidation(Amount recovery_amount, Date liquidation_date);
    Amount calculateDeficiencyBalance() const;
    
    // Vehicle management
    void processInsuranceClaim(Amount claim_amount, Date claim_date);
    void updateVehicleCondition(const std::string& condition_report);
    Amount estimateRecoveryValue() const;
    
    // Risk assessment
    double calculateProbabilityOfDefault() const;
    Amount calculateExpectedLoss() const;
    double getRiskScore() const;
    bool isHighRisk() const;
    
    // Payment calculations
    Amount calculateMonthlyPayment() const;
    Amount calculateRemainingInterest() const;
    std::vector<Date> getPaymentSchedule(int num_payments = 12) const;
    Amount getNextPaymentAmount() const;
    Date getNextPaymentDate() const;
    
    // Performance metrics
    double getPaymentToIncomeRatio() const;
    int getDaysDelinquent() const;
    bool isNearMaturity(int months_threshold = 6) const;
    double getSeasoningMonths() const;
    
    // Getters
    const std::string& getLoanId() const { return loan_id_; }
    AutoLoanType getLoanType() const { return loan_type_; }
    const Vehicle& getVehicle() const { return vehicle_; }
    const Obligor& getBorrower() const { return borrower_; }
    Status getStatus() const { return status_; }
    
    Amount getOriginalBalance() const { return original_balance_; }
    int getOriginalTermMonths() const { return original_term_months_; }
    int getRemainingTerms() const { return remaining_terms_; }
    Amount getMonthlyPayment() const { return monthly_payment_; }
    Date getOriginationDate() const { return origination_date_; }
    Date getMaturityDate() const { return maturity_date_; }
    
    LTVCategory getLTVCategory() const { return ltv_category_; }
    bool hasGapInsurance() const { return has_gap_insurance_; }
    bool hasExtendedWarranty() const { return has_extended_warranty_; }
    bool hasCoBorrower() const { return co_borrower_id_.has_value(); }
    
    int getPaymentsMade() const { return payments_made_; }
    int getPaymentsMissed() const { return payments_missed_; }
    Amount getTotalInterestPaid() const { return total_interest_paid_; }
    Amount getTotalPrincipalPaid() const { return total_principal_paid_; }
    Date getLastPaymentDate() const { return last_payment_date_; }
    bool isInDefault() const { return is_in_default_; }
    
    Amount getDownPayment() const { return down_payment_; }
    bool hasCreditInsurance() const { return has_credit_insurance_; }
    
    // Setters
    void setStatus(Status status) { status_ = status; }
    void setCurrentRate(Rate rate) { current_rate_ = rate; }
    void setGapInsurance(bool has_gap) { has_gap_insurance_ = has_gap; }
    void setExtendedWarranty(bool has_warranty) { has_extended_warranty_ = has_warranty; }
    void setCoBorrower(const std::string& co_borrower_id) { co_borrower_id_ = co_borrower_id; }
    void setCreditInsurance(bool has_insurance) { has_credit_insurance_ = has_insurance; }
};

} // namespace Structura