#pragma once

#include "../core/deal_base.h"
#include "../assets/consumer_installment.h"
#include "../assets/pool.h"
#include <map>
#include <string>
#include <vector>

namespace Structura {

/**
 * Consumer ABS tranche types based on standard market classifications
 */
enum class ConsumerABSTrancheType {
    CLASS_A,        // Senior Class A tranche
    CLASS_B,        // Senior Class B tranche
    CLASS_C,        // Mezzanine Class C tranche
    CLASS_D,        // Subordinate Class D tranche
    RESIDUAL        // Residual/equity tranche
};

std::string consumerABSTrancheTypeToString(ConsumerABSTrancheType type);
ConsumerABSTrancheType stringToConsumerABSTrancheType(const std::string& str);

/**
 * Consumer ABS tranche structure with credit ratings and subordination
 */
struct ConsumerABSTranche {
    std::string tranche_id;
    ConsumerABSTrancheType tranche_type;
    std::string credit_rating;
    Amount notional_amount;
    Amount outstanding_balance;
    Rate coupon_rate;
    PaymentFrequency payment_frequency;
    
    // Tranche subordination and credit enhancement
    double subordination_percentage;
    double credit_enhancement_level;
    Amount reserve_account_target;
    
    // Consumer ABS specific features
    bool has_step_down_provisions;
    Date step_down_date;
    double step_down_threshold;
    bool has_turbo_provisions;
    double turbo_threshold;
    
    // Performance tracking
    Amount total_principal_paid;
    Amount total_interest_paid;
    Amount cumulative_losses; 
    Date last_payment_date;
    double current_rating_factor;
    
    // Revolving features (for credit card ABS)
    bool is_revolving_tranche;
    Date revolving_period_end;
    Amount available_funding_capacity;
    
    ConsumerABSTranche() = default;
    ConsumerABSTranche(const std::string& id, ConsumerABSTrancheType type, const std::string& rating,
                       Amount notional, Rate coupon);
    
    // Payment calculations
    Amount calculateInterestPayment() const;
    Amount calculatePrincipalPayment(Amount available_principal) const;
    void makePayment(Amount principal, Amount interest, Date payment_date);
    
    // Performance metrics
    double getCurrentYield() const;
    double getCumulativeLossRate() const;
    bool isFullyPaid() const;
    double getRatingFactor() const { return current_rating_factor; }
    
    // Step-down and turbo provisions
    bool canStepDown(double portfolio_performance) const;
    bool shouldActivateTurbo(double excess_spread) const;
};

/**
 * Consumer ABS waterfall structure with consumer-specific features
 */
struct ConsumerABSWaterfall {
    std::vector<std::string> interest_payment_sequence;
    std::vector<std::string> principal_payment_sequence;
    std::map<std::string, double> loss_allocation;
    
    // Consumer ABS specific features
    bool has_excess_spread_capture;
    double minimum_excess_spread_target;
    bool has_principal_payment_window;
    int payment_window_days;
    
    // Credit enhancement features
    std::map<std::string, double> overcollateralization_ratios;
    Amount cash_collateral_account_target;
    Amount spread_account_target;
    
    // Performance triggers
    std::map<std::string, double> early_amortization_triggers;
    std::map<std::string, double> rapid_amortization_triggers;
    double portfolio_yield_trigger;
    double loss_rate_trigger;
    
    ConsumerABSWaterfall();
    
    // Waterfall processing
    std::map<std::string, Amount> processInterestWaterfall(
        Amount available_interest,
        const std::map<std::string, ConsumerABSTranche>& tranches,
        const std::map<std::string, Amount>& required_fees) const;
    
    std::map<std::string, Amount> processPrincipalWaterfall(
        Amount available_principal,
        const std::map<std::string, ConsumerABSTranche>& tranches,
        bool early_amortization_triggered = false) const;
    
    std::map<std::string, Amount> processLossWaterfall(
        Amount total_losses,
        const std::map<std::string, ConsumerABSTranche>& tranches) const;
    
    // Performance triggers
    bool checkEarlyAmortizationTriggers(const Pool<ConsumerInstallment>& pool,
                                       const std::map<std::string, ConsumerABSTranche>& tranches) const;
    bool checkRapidAmortizationTriggers(const Pool<ConsumerInstallment>& pool) const;
    
    // Credit enhancement management
    Amount calculateRequiredCashCollateral(const std::map<std::string, ConsumerABSTranche>& tranches) const;
    Amount calculateRequiredSpreadAccount(const Pool<ConsumerInstallment>& pool) const;
    bool passesOvercollateralizationTests(const Pool<ConsumerInstallment>& pool,
                                         const std::map<std::string, ConsumerABSTranche>& tranches) const;
};

/**
 * Consumer portfolio servicer for managing consumer installment loans
 */
struct ConsumerABSServicer {
    std::string servicer_name;
    Rate servicing_fee_rate;
    Rate ancillary_fee_rate;
    
    // Servicing capabilities
    bool provides_customer_service;
    bool handles_collections;
    bool manages_modifications;
    bool processes_charge_offs;
    
    // Performance metrics
    double delinquency_management_score;
    double customer_satisfaction_score;
    Amount monthly_servicing_fees;
    Amount monthly_ancillary_fees;
    
    // Collection statistics
    std::map<int, double> collection_effectiveness_by_bucket; // Days past due -> recovery rate
    Amount total_collections_current_period;
    Amount total_charge_offs_current_period;
    double average_time_to_resolution;
    
    ConsumerABSServicer();
    
    // Servicing functions
    Amount calculateServicingFees(const Pool<ConsumerInstallment>& pool, Date calculation_date) const;
    Amount processCollections(Pool<ConsumerInstallment>& pool, Date collection_date);
    void processDelinquencies(Pool<ConsumerInstallment>& pool, Date processing_date);
    void processChargeOffs(Pool<ConsumerInstallment>& pool, Date charge_off_date);
    
    // Customer management
    void processCustomerPayments(Pool<ConsumerInstallment>& pool, Date payment_date);
    void handleCustomerInquiries(const std::string& customer_id, const std::string& inquiry_type);
    void processLoanModifications(Pool<ConsumerInstallment>& pool, Date modification_date);
    
    // Reporting
    std::map<std::string, double> generateServicingReport(Date report_date) const;
    std::map<int, Amount> getDelinquencyBucketReport(const Pool<ConsumerInstallment>& pool) const;
};

/**
 * Consumer ABS performance analytics and monitoring
 */
struct ConsumerABSAnalytics {
    // Portfolio composition metrics
    double weighted_average_coupon;
    double weighted_average_remaining_term;
    Amount weighted_average_balance;
    double weighted_average_fico_score;
    
    // Delinquency and loss metrics
    std::map<int, double> delinquency_rates_by_bucket; // 30+, 60+, 90+ days
    double net_charge_off_rate;
    double recovery_rate;
    double cumulative_loss_rate;
    
    // Performance metrics
    double monthly_payment_rate;
    double portfolio_yield;
    double excess_spread;
    double servicer_advance_rate;
    
    // Demographic and geographic analysis
    std::map<std::string, double> geographic_concentration;
    std::map<std::string, double> fico_score_distribution;
    std::map<std::string, double> loan_purpose_distribution;
    std::map<int, double> seasoning_distribution;
    
    ConsumerABSAnalytics();
    
    void calculateMetrics(const Pool<ConsumerInstallment>& pool,
                         const std::map<std::string, ConsumerABSTranche>& tranches);
    std::map<std::string, double> getDelinquencyAnalysis(const Pool<ConsumerInstallment>& pool) const;
    std::map<std::string, Amount> getLossProjections(const Pool<ConsumerInstallment>& pool) const;
    std::map<std::string, double> calculateTrancheMetrics(const std::map<std::string, ConsumerABSTranche>& tranches) const;
    
    // Vintage analysis
    std::map<Date, double> performVintageAnalysis(const Pool<ConsumerInstallment>& pool) const;
    std::map<int, double> calculateSeasoningCurves(const Pool<ConsumerInstallment>& pool) const;
};

/**
 * Consumer ABS early amortization and trigger management
 */
struct ConsumerABSTriggersManager {
    // Trigger definitions
    double excess_spread_trigger_level;
    double loss_rate_trigger_level;
    double delinquency_trigger_level;
    double servicer_termination_trigger;
    
    // Current status
    bool early_amortization_triggered;
    bool rapid_amortization_triggered;
    Date trigger_date;
    std::string trigger_reason;
    
    // Historical trigger events
    std::map<Date, std::string> trigger_history;
    std::map<Date, double> performance_metrics_history;
    
    ConsumerABSTriggersManager();
    
    // Trigger evaluation
    bool evaluateEarlyAmortizationTriggers(const Pool<ConsumerInstallment>& pool,
                                          const ConsumerABSAnalytics& analytics,
                                          Date evaluation_date);
    bool evaluateRapidAmortizationTriggers(const Pool<ConsumerInstallment>& pool,
                                          const ConsumerABSAnalytics& analytics,
                                          Date evaluation_date);
    
    // Trigger management
    void activateEarlyAmortization(const std::string& reason, Date activation_date);
    void activateRapidAmortization(const std::string& reason, Date activation_date);
    void resetTriggers(Date reset_date);
    
    // Reporting
    std::map<std::string, double> getCurrentTriggerStatus() const;
    std::map<Date, std::string> getTriggerHistory() const;
};

/**
 * Comprehensive Consumer ABS deal implementation
 */
class ConsumerABSDeal : public DealBase {
private:
    std::map<std::string, ConsumerABSTranche> tranches_;
    ConsumerABSWaterfall waterfall_;
    Pool<ConsumerInstallment>* consumer_pool_;
    ConsumerABSServicer servicer_;
    ConsumerABSAnalytics analytics_;
    ConsumerABSTriggersManager triggers_manager_;
    
    // Deal-specific accounts
    std::string principal_collection_account_id_;
    std::string interest_collection_account_id_;
    std::string reserve_account_id_;
    std::string spread_account_id_;
    std::string cash_collateral_account_id_;
    std::string servicer_advance_account_id_;
    std::string charge_off_account_id_;
    
    // Servicing and fees
    Rate servicing_fee_rate_;
    Rate trustee_fee_rate_;
    Amount monthly_servicing_fee_;
    Amount monthly_trustee_fee_;
    
    // Performance tracking
    std::map<Date, Amount> historical_collections_;
    std::map<Date, Amount> historical_charge_offs_;
    std::map<Date, double> historical_delinquency_rates_;
    std::map<Date, double> portfolio_metrics_history_;
    std::map<Date, bool> trigger_status_history_;

public:
    explicit ConsumerABSDeal(const DealInfo& deal_info);
    ~ConsumerABSDeal() override = default;
    
    // Deal setup
    void setConsumerPool(Pool<ConsumerInstallment>* pool);
    void addTranche(const ConsumerABSTranche& tranche);
    void setupWaterfall(const ConsumerABSWaterfall& waterfall);
    void setupServicer(const ConsumerABSServicer& servicer);
    void setServicingFees(Rate servicing_fee, Rate trustee_fee);
    
    // DealBase interface implementation
    void runWaterfall(const Date& paymentDate) override;
    Balance calculateDealValue(const Date& valuationDate) const override;
    std::vector<std::string> validate() const override;
    std::string getDealSummary() const override;
    void processPaymentDate(const Date& paymentDate) override;
    
    // Consumer ABS specific methods
    std::map<std::string, ConsumerABSTranche> getTranches() const { return tranches_; }
    ConsumerABSWaterfall getWaterfall() const { return waterfall_; }
    ConsumerABSServicer getServicer() const { return servicer_; }
    ConsumerABSAnalytics getAnalytics() const { return analytics_; }
    ConsumerABSTriggersManager getTriggersManager() const { return triggers_manager_; }
    
    // Consumer loan servicing
    void processCustomerPayments(Date payment_date);
    void processDelinquencies(Date processing_date);
    void processChargeOffs(Date charge_off_date);
    void handleLoanModifications(Date modification_date);
    
    // Performance monitoring
    bool checkPerformanceTriggers(Date evaluation_date);
    void handleEarlyAmortization(Date trigger_date, const std::string& reason);
    void handleRapidAmortization(Date trigger_date, const std::string& reason);
    
    // Credit and loss management
    void allocateChargeOffs(Amount charge_off_amount, Date charge_off_date);
    Amount calculateExpectedLosses() const;
    Amount calculateUnexpectedLosses(double confidence_level = 0.99) const;
    void updateCreditEnhancements(Date update_date);
    
    // Servicing fee calculations and payments
    Amount calculateServicingFees(Date calculation_date) const;
    Amount calculateTrusteeFees(Date calculation_date) const;
    void payServicingFees(Date payment_date);
    
    // Performance analytics
    double calculatePortfolioYield() const;
    double calculateExcessSpread() const;
    double calculateMonthlyPaymentRate() const;
    std::map<std::string, double> getTrancheMetrics() const;
    std::map<int, double> getDelinquencyRates() const;
    
    // Concentration and demographic analysis
    std::map<std::string, Amount> getGeographicConcentration() const;
    std::map<std::string, Amount> getFICODistribution() const;
    std::map<std::string, Amount> getLoanPurposeDistribution() const;
    std::map<int, Amount> getSeasoningDistribution() const;
    
    // Credit enhancement management
    Amount calculateRequiredReserves() const;
    Amount calculateCashCollateralRequirement() const;
    Amount calculateSpreadAccountRequirement() const;
    void manageReserveAccounts(Date management_date);
    
    // Stress testing and scenario analysis
    void applyDelinquencyStress(double delinquency_rate_multiplier);
    void applyChargeOffStress(double charge_off_rate_multiplier);
    void applyInterestRateStress(double rate_shock);
    std::map<std::string, Amount> stressTestWaterfall(double loss_scenario, double delinquency_shock) const;
    
    // Reporting and compliance
    std::map<std::string, double> generateMonthlyReport(Date report_date) const;
    std::map<int, Amount> generateDelinquencyReport(Date report_date) const;
    std::map<std::string, Amount> generateCashFlowReport(Date start_date, Date end_date) const;
    std::map<std::string, double> generatePortfolioStatistics() const;

private:
    // Internal processing methods
    void processConsumerCollections(Date collection_date);
    void updatePortfolioAnalytics(Date update_date);
    void distributeCollections(Amount collections, Date distribution_date);
    void manageServicerAdvances(Date management_date);
    void processCreditEnhancements(Date processing_date);
    
    // Risk calculations
    double calculateConcentrationRisk() const;
    Amount calculateRequiredOvercollateralization() const;
    void updateDelinquencyMetrics();
    void monitorPerformanceTriggers();
    
    // Fee processing
    void accrueServicingFees(Date accrual_date);
    void processServicingFeePayment(Date payment_date);
    void processTrusteeFeePayment(Date payment_date);
    
    // Credit enhancement management
    void fundReserveAccounts(Date funding_date);
    void releaseExcessReserves(Date release_date);
    void manageCashCollateral(Date management_date);
    void manageSpreadAccount(Date management_date);
    
    // Performance trigger handling
    void evaluatePerformanceTriggers(Date evaluation_date);
    void handleTriggerActivation(const std::string& trigger_type, Date activation_date);
    void adjustWaterfallForTriggers();
    
    // Servicing management
    void monitorServicingPerformance();
    void handleServicingTransfer(const std::string& new_servicer_name, Date transfer_date);
    void processServicerReplacements();
    
    // Reporting helpers
    void generateTrancheReports(Date report_date) const;
    void generatePortfolioPerformanceReport(Date report_date) const;
    void generateServicingReport(Date report_date) const;
    void generateTriggerStatusReport(Date report_date) const;
};

} // namespace Structura