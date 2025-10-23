#pragma once

#include "asset_base.h"
#include "../core/financial_types.h"
#include <vector>
#include <optional>

namespace Structura {

/**
 * Lease types for different equipment categories
 */
enum class LeaseType {
    EQUIPMENT,      // General equipment leases
    VEHICLE,        // Auto and truck leases
    AIRCRAFT,       // Aircraft leases
    REAL_ESTATE,    // Commercial real estate
    TECHNOLOGY,     // IT and software leases
    MEDICAL,        // Medical equipment
    OTHER
};

std::string leaseTypeToString(LeaseType type);

/**
 * Lease structure types
 */
enum class LeaseStructure {
    CAPITAL,        // Capital/finance lease
    OPERATING,      // Operating lease
    SYNTHETIC,      // Synthetic lease
    LEVERAGED       // Leveraged lease
};

std::string leaseStructureToString(LeaseStructure structure);

/**
 * Equipment information for lease assets
 */
struct Equipment {
    std::string equipment_id;
    std::string manufacturer;
    std::string model;
    std::string serial_number;
    Date manufacture_date;
    Amount original_cost;
    Amount current_value;
    Amount estimated_residual_value;
    int useful_life_months;
    double depreciation_rate;
    
    Equipment(const std::string& id, const std::string& mfg, const std::string& model_name,
              Amount cost, Amount residual, int life_months);
    
    // Depreciation calculations
    Amount calculateStraightLineDepreciation(int months_elapsed) const;
    Amount calculateAcceleratedDepreciation(int months_elapsed, double factor = 2.0) const;
    Amount getCurrentBookValue(int months_elapsed) const;
    Amount getEstimatedRemainingValue(int months_remaining) const;
};

/**
 * Lessee information
 */
struct Lessee {
    std::string lessee_id;
    std::string company_name;
    std::string industry;
    std::string credit_rating;
    double credit_score;
    Amount annual_revenue;
    int years_in_business;
    
    Lessee(const std::string& id, const std::string& name, const std::string& industry_sector);
    
    // Credit assessment
    double getCreditRiskScore() const;
    bool isInvestmentGrade() const;
};

/**
 * Lease payment schedule
 */
struct LeasePaymentSchedule {
    std::vector<Date> payment_dates;
    std::vector<Amount> payment_amounts;
    Amount security_deposit;
    Amount end_of_term_payment;
    
    LeasePaymentSchedule() = default;
    
    void generateSchedule(Date start_date, Date end_date, Amount monthly_payment, 
                         PaymentFrequency frequency);
    Amount getTotalPayments() const;
    Amount getOutstandingPayments(Date as_of_date) const;
};

/**
 * Lease asset implementation
 */
class Lease {
private:
    std::string lease_id_;
    LeaseType lease_type_;
    LeaseStructure lease_structure_;
    Equipment equipment_;
    Lessee lessee_;
    LeasePaymentSchedule payment_schedule_;
    Status status_;
    
    // Lease-specific terms
    Date lease_start_date_;
    Date lease_end_date_;
    int lease_term_months_;
    Amount monthly_payment_;
    Amount security_deposit_;
    Amount purchase_option_price_;
    bool has_purchase_option_;
    bool has_renewal_option_;
    int renewal_term_months_;
    
    // Financial tracking
    Amount total_payments_received_;
    Amount outstanding_lease_payments_;
    Date last_payment_date_;
    int payments_missed_;
    bool is_in_default_;
    
    // Residual value tracking
    Amount guaranteed_residual_value_;
    Amount estimated_residual_value_;
    double residual_value_guarantee_percentage_;

public:
    Lease(const std::string& id, LeaseType type, LeaseStructure structure,
          const Equipment& equipment, const Lessee& lessee,
          Date start_date, int term_months, Amount monthly_payment);
    
    // AssetBase interface
    void makePayment(Balance principalPayment, Balance interestPayment, Date paymentDate);
    void applyDefault(Date defaultDate);
    void applyPrepayment(Balance amount, Date paymentDate);
    Balance getCurrentBalance() const;
    Rate getCurrentRate() const;
    void resetToOriginal();
    
    // Lease-specific methods
    void processLeasePayment(Amount payment_amount, Date payment_date);
    void processEarlyTermination(Date termination_date, Amount termination_payment);
    void exercisePurchaseOption(Date exercise_date);
    void processRenewal(int additional_months, Amount new_monthly_payment);
    
    // Equipment and residual value management
    void updateEquipmentValue(Amount new_value, Date valuation_date);
    void updateResidualValue(Amount new_residual_estimate);
    Amount calculateCurrentEquipmentValue() const;
    Amount calculateExpectedResidualValue() const;
    
    // Payment and cashflow methods
    std::vector<Date> getPaymentDates(int numPayments = 12) const;
    Amount getNextPaymentAmount() const;
    Date getNextPaymentDate() const;
    Amount getOutstandingPayments() const;
    
    // Risk and performance metrics
    double getPaymentToValueRatio() const;
    double getResidualRiskRatio() const;
    int getDaysDelinquent() const;
    bool isNearMaturity(int months_threshold = 6) const;
    
    // Getters
    const std::string& getLeaseId() const { return lease_id_; }
    Status getStatus() const { return status_; }
    LeaseType getLeaseType() const { return lease_type_; }
    LeaseStructure getLeaseStructure() const { return lease_structure_; }
    const Equipment& getEquipment() const { return equipment_; }
    const Lessee& getLessee() const { return lessee_; }
    const LeasePaymentSchedule& getPaymentSchedule() const { return payment_schedule_; }
    
    Date getLeaseStartDate() const { return lease_start_date_; }
    Date getLeaseEndDate() const { return lease_end_date_; }
    int getLeaseTermMonths() const { return lease_term_months_; }
    Amount getMonthlyPayment() const { return monthly_payment_; }
    Amount getSecurityDeposit() const { return security_deposit_; }
    Amount getPurchaseOptionPrice() const { return purchase_option_price_; }
    bool hasPurchaseOption() const { return has_purchase_option_; }
    bool hasRenewalOption() const { return has_renewal_option_; }
    
    Amount getTotalPaymentsReceived() const { return total_payments_received_; }
    Amount getGuaranteedResidualValue() const { return guaranteed_residual_value_; }
    Amount getEstimatedResidualValue() const { return estimated_residual_value_; }
    double getResidualValueGuaranteePercentage() const { return residual_value_guarantee_percentage_; }
    
    bool isInDefault() const { return is_in_default_; }
    int getPaymentsMissed() const { return payments_missed_; }
    Date getLastPaymentDate() const { return last_payment_date_; }
    
    // Setters
    void setStatus(Status status) { status_ = status; }
    void setSecurityDeposit(Amount deposit) { security_deposit_ = deposit; }
    void setPurchaseOption(Amount price) { 
        purchase_option_price_ = price; 
        has_purchase_option_ = true; 
    }
    void setRenewalOption(int months) { 
        renewal_term_months_ = months; 
        has_renewal_option_ = true; 
    }
    void setResidualValueGuarantee(Amount guaranteed_value, double percentage) {
        guaranteed_residual_value_ = guaranteed_value;
        residual_value_guarantee_percentage_ = percentage;
    }
};

} // namespace Structura