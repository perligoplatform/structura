#pragma once

#include "../core/deal_base.h"
#include "../assets/lease.h"
#include "../assets/pool.h"
#include <map>
#include <string>

namespace Structura {

/**
 * Lease ABS tranche types
 */
enum class LeaseTrancheType {
    SENIOR_A,       // Senior A tranche
    SENIOR_B,       // Senior B tranche  
    MEZZANINE,      // Mezzanine tranche
    SUBORDINATE,    // Subordinate/equity tranche
    RESIDUAL        // Residual value tranche
};

std::string leaseTrancheTypeToString(LeaseTrancheType type);

/**
 * Lease ABS tranche structure
 */
struct LeaseABSTranche {
    std::string tranche_id;
    LeaseTrancheType tranche_type;
    Amount notional_amount;
    Amount outstanding_balance;
    Rate coupon_rate;
    PaymentFrequency payment_frequency;
    
    // Tranche-specific features
    double subordination_percentage;
    double credit_enhancement_level;
    Amount reserve_account_target;
    bool has_yield_maintenance;
    bool has_call_protection;
    
    // Performance tracking
    Amount total_principal_paid;
    Amount total_interest_paid;
    Amount cumulative_losses;
    Date last_payment_date;
    
    LeaseABSTranche() = default;
    LeaseABSTranche(const std::string& id, LeaseTrancheType type, Amount notional, Rate coupon);
    
    // Payment calculations
    Amount calculateInterestPayment() const;
    Amount calculatePrincipalPayment(Amount available_principal) const;
    void makePayment(Amount principal, Amount interest, Date payment_date);
    
    // Performance metrics
    double getCurrentYield() const;
    double getCumulativeLossRate() const;
    bool isFullyPaid() const;
};

/**
 * Lease ABS waterfall structure
 */
struct LeaseABSWaterfall {
    std::vector<std::string> payment_sequence;
    std::map<std::string, double> loss_allocation;
    std::map<std::string, Amount> reserve_requirements;
    
    // Waterfall processing
    std::map<std::string, Amount> processInterestWaterfall(Amount available_interest,
                                                           const std::map<std::string, LeaseABSTranche>& tranches) const;
    std::map<std::string, Amount> processPrincipalWaterfall(Amount available_principal,
                                                            const std::map<std::string, LeaseABSTranche>& tranches) const;
    std::map<std::string, Amount> processLossWaterfall(Amount total_losses,
                                                       const std::map<std::string, LeaseABSTranche>& tranches) const;
    
    // Reserve management
    Amount calculateTotalReserveRequirement(const std::map<std::string, LeaseABSTranche>& tranches) const;
    bool checkCoverageRatios(Amount pool_cashflow, Amount required_payments) const;
};

/**
 * Residual value management for lease ABS
 */
struct ResidualValueManager {
    Amount total_estimated_residual;
    Amount guaranteed_residual_value;
    Amount realized_residual_value;
    std::map<Date, Amount> residual_realization_schedule;
    
    // Residual risk metrics
    double residual_value_ratio;
    double guarantee_coverage_ratio;
    Amount residual_value_insurance;
    
    ResidualValueManager();
    
    // Residual value processing
    void updateResidualEstimates(const Pool<Lease>& lease_pool);
    Amount processResidualRealization(Date realization_date, Amount realized_amount);
    Amount calculateResidualRisk() const;
    
    // Insurance and guarantees
    void addResidualValueInsurance(Amount coverage_amount, Rate premium_rate);
    Amount calculateInsuranceClaim(Amount shortfall) const;
};

/**
 * Lease ABS performance analytics
 */
struct LeaseABSAnalytics {
    // Portfolio metrics
    double weighted_average_lease_term;
    double weighted_average_remaining_term;
    Amount weighted_average_payment;
    double concentration_ratio_by_lessee;
    double concentration_ratio_by_equipment_type;
    
    // Credit metrics
    double portfolio_credit_score;
    Amount expected_credit_losses;
    Amount unexpected_credit_losses;
    double default_probability;
    
    // Residual value metrics
    double residual_value_ratio;
    Amount total_residual_exposure;
    double residual_realization_rate;
    
    LeaseABSAnalytics();
    
    void calculateMetrics(const Pool<Lease>& lease_pool);
    std::map<std::string, double> getConcentrationAnalysis(const Pool<Lease>& lease_pool) const;
    std::map<std::string, Amount> getCreditLossProjections() const;
};

/**
 * Concrete deal implementation for lease ABS
 */
class LeaseABSDeal : public DealBase {
private:
    std::map<std::string, LeaseABSTranche> tranches_;
    LeaseABSWaterfall waterfall_;
    Pool<Lease>* lease_pool_;
    ResidualValueManager residual_manager_;
    LeaseABSAnalytics analytics_;
    
    // Deal-specific accounts
    std::string principal_collection_account_id_;
    std::string interest_collection_account_id_;
    std::string reserve_account_id_;
    std::string residual_account_id_;
    std::string expense_account_id_;
    
    // Performance tracking
    std::map<Date, Amount> historical_collections_;
    std::map<Date, Amount> historical_losses_;
    std::map<Date, Amount> residual_realizations_;
    std::map<Date, double> portfolio_metrics_;

public:
    explicit LeaseABSDeal(const DealInfo& deal_info);
    ~LeaseABSDeal() override = default;
    
    // Deal setup
    void setLeasePool(Pool<Lease>* pool);
    void addTranche(const LeaseABSTranche& tranche);
    void setupWaterfall(const LeaseABSWaterfall& waterfall);
    void setupResidualValueManagement(const ResidualValueManager& manager);
    
    // DealBase interface implementation
    void runWaterfall(const Date& paymentDate) override;
    Balance calculateDealValue(const Date& valuationDate) const override;
    std::vector<std::string> validate() const override;
    std::string getDealSummary() const override;
    void processPaymentDate(const Date& paymentDate) override;
    
    // Lease ABS specific methods
    std::map<std::string, LeaseABSTranche> getTranches() const { return tranches_; }
    LeaseABSWaterfall getWaterfall() const { return waterfall_; }
    ResidualValueManager getResidualManager() const { return residual_manager_; }
    LeaseABSAnalytics getAnalytics() const { return analytics_; }
    
    // Portfolio management
    void processLeaseMaturity(const std::string& lease_id, Date maturity_date);
    void processEarlyTermination(const std::string& lease_id, Date termination_date, Amount termination_value);
    void processResidualRealization(const std::string& lease_id, Amount realized_value);
    
    // Credit and loss management
    void processDefault(const std::string& lease_id, Date default_date, Amount recovery_amount);
    void allocateLosses(Amount total_losses, Date loss_date);
    Amount calculateExpectedLosses() const;
    Amount calculateUnexpectedLosses(double confidence_level = 0.99) const;
    
    // Concentration and diversification analysis
    std::map<LeaseType, Amount> getLeaseTypeConcentration() const;
    std::map<std::string, Amount> getLesseeConcentration() const;
    std::map<std::string, Amount> getGeographicConcentration() const;
    
    // Performance analytics
    double calculatePortfolioYield() const;
    double calculateExcessSpread() const;
    double calculateWeightedAverageLife() const;
    std::map<std::string, double> getTrancheMetrics() const;
    
    // Residual value analysis
    Amount getTotalResidualExposure() const;
    double getResidualValueRatio() const;
    std::map<Date, Amount> getResidualRealizationSchedule() const;
    Amount calculateResidualValueRisk() const;
    
    // Stress testing
    void applyLeaseDefaultScenario(double default_rate_multiplier);
    void applyResidualValueStress(double residual_value_shock);
    void applyInterestRateStress(double rate_shock);
    std::map<std::string, Amount> stressTestWaterfall(double loss_scenario, double residual_shock) const;

private:
    void processLeaseCollections(Date collection_date);
    void updatePortfolioAnalytics(Date update_date);
    void distributeCollections(Amount collections, Date distribution_date);
    void manageReserveAccounts(Date management_date);
    void processResidualValue(Date processing_date);
    
    // Risk calculations
    double calculateConcentrationRisk() const;
    Amount calculateRequiredReserves() const;
    void updateCreditMetrics();
    
    // Reporting and compliance
    void generateTrancheReports(Date report_date) const;
    void generatePortfolioReport(Date report_date) const;
    void generateResidualValueReport(Date report_date) const;
};

} // namespace Structura