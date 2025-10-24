#pragma once

#include "../core/deal_base.h"
#include "../assets/corporate_loan.h"
#include "../assets/pool.h"
#include <map>
#include <string>
#include <vector>

namespace Structura {

/**
 * CLO tranche types based on standard market classifications
 */
enum class CLOTrancheType {
    AAA,            // AAA-rated senior tranche
    AA,             // AA-rated senior tranche  
    A,              // A-rated senior tranche
    BBB,            // BBB-rated mezzanine tranche
    BB,             // BB-rated mezzanine tranche
    B,              // B-rated subordinate tranche
    EQUITY          // Equity/first-loss tranche
};

std::string cloTrancheTypeToString(CLOTrancheType type);
CLOTrancheType stringToCLOTrancheType(const std::string& str);

/**
 * CLO tranche structure with credit ratings and subordination
 */
struct CLOTranche {
    std::string tranche_id;
    CLOTrancheType tranche_type;
    std::string credit_rating;
    Amount notional_amount;
    Amount outstanding_balance;
    Rate coupon_rate;
    PaymentFrequency payment_frequency;
    
    // Tranche subordination and credit enhancement
    double subordination_percentage;
    double credit_enhancement_level;
    Amount overcollateralization_amount;
    Amount interest_coverage_reserve;
    
    // Floating rate features (common in CLOs)
    bool is_floating_rate;
    std::string reference_rate;  // e.g., "SOFR", "LIBOR"
    Rate spread_over_reference;
    Rate floor_rate;
    Rate cap_rate;
    
    // Performance tracking
    Amount total_principal_paid;
    Amount total_interest_paid;
    Amount cumulative_losses;
    Date last_payment_date;
    double current_rating_factor;
    
    CLOTranche() = default;
    CLOTranche(const std::string& id, CLOTrancheType type, const std::string& rating,
               Amount notional, Rate coupon, bool floating = false);
    
    // Payment calculations
    Amount calculateInterestPayment(Rate current_reference_rate = 0.0) const;
    Amount calculatePrincipalPayment(Amount available_principal) const;
    void makePayment(Amount principal, Amount interest, Date payment_date);
    
    // Performance metrics
    double getCurrentYield(Rate current_reference_rate = 0.0) const;
    double getCumulativeLossRate() const;
    bool isFullyPaid() const;
    double getRatingFactor() const { return current_rating_factor; }
};

/**
 * CLO waterfall structure with overcollateralization and coverage tests
 */
struct CLOWaterfall {
    std::vector<std::string> interest_payment_sequence;
    std::vector<std::string> principal_payment_sequence;
    std::map<std::string, double> loss_allocation;
    
    // Coverage ratios and tests
    std::map<std::string, double> overcollateralization_ratios;
    std::map<std::string, double> interest_coverage_ratios;
    double minimum_weighted_average_spread;
    double maximum_weighted_average_life;
    
    // Reinvestment and trading provisions
    bool reinvestment_allowed;
    Date reinvestment_period_end;
    double maximum_ccc_concentration;
    double maximum_single_obligor_concentration;
    
    CLOWaterfall();
    
    // Waterfall processing
    std::map<std::string, Amount> processInterestWaterfall(
        Amount available_interest,
        const std::map<std::string, CLOTranche>& tranches,
        const std::map<std::string, Amount>& required_fees) const;
    
    std::map<std::string, Amount> processPrincipalWaterfall(
        Amount available_principal,
        const std::map<std::string, CLOTranche>& tranches) const;
    
    std::map<std::string, Amount> processLossWaterfall(
        Amount total_losses,
        const std::map<std::string, CLOTranche>& tranches) const;
    
    // Coverage tests
    bool passesOvercollateralizationTests(const Pool<CorporateLoan>& loan_pool,
                                         const std::map<std::string, CLOTranche>& tranches) const;
    bool passesInterestCoverageTests(Amount available_interest,
                                    const std::map<std::string, CLOTranche>& tranches) const;
    bool passesQualityTests(const Pool<CorporateLoan>& loan_pool) const;
    
    // Portfolio restrictions
    bool checkConcentrationLimits(const Pool<CorporateLoan>& loan_pool) const;
    bool checkCreditQualityLimits(const Pool<CorporateLoan>& loan_pool) const;
    bool checkMaturityProfile(const Pool<CorporateLoan>& loan_pool) const;
};

/**
 * CLO portfolio manager for active management during reinvestment period
 */
struct CLOPortfolioManager {
    std::string manager_name;
    bool actively_managed;
    Amount reinvestment_capacity;
    Date reinvestment_period_end;
    
    // Trading parameters
    double maximum_trading_volume_per_period;
    std::vector<std::string> eligible_obligor_list;
    std::map<std::string, double> sector_concentration_limits;
    std::map<std::string, double> rating_concentration_limits;
    
    // Performance metrics
    Amount total_reinvestments;
    Amount total_dispositions;
    double portfolio_turnover_rate;
    double weighted_average_spread_improvement;
    
    CLOPortfolioManager();
    
    // Portfolio management functions
    bool canReinvestProceeds(Amount proceeds, const Pool<CorporateLoan>& current_pool) const;
    std::vector<std::string> identifyTradingOpportunities(const Pool<CorporateLoan>& pool) const;
    bool evaluateNewLoanAddition(const CorporateLoan& loan, const Pool<CorporateLoan>& current_pool) const;
    
    // Reporting
    std::map<std::string, double> generateTradingReport() const;
    std::map<std::string, double> calculatePortfolioMetrics(const Pool<CorporateLoan>& pool) const;
};

/**
 * CLO performance analytics and reporting
 */
struct CLOAnalytics {
    // Portfolio composition metrics
    double weighted_average_spread;
    double weighted_average_price;
    double weighted_average_life;
    double weighted_average_rating_factor;
    
    // Concentration metrics
    std::map<std::string, double> sector_concentration;
    std::map<std::string, double> obligor_concentration;
    std::map<std::string, double> rating_concentration;
    std::map<std::string, double> geographic_concentration;
    
    // Credit quality metrics
    double portfolio_credit_score;
    Amount expected_credit_losses;
    Amount unexpected_credit_losses;
    double default_probability;
    double recovery_rate_assumption;
    
    // Performance metrics
    double current_excess_spread;
    double reinvestment_yield;
    double total_return_on_equity;
    std::map<std::string, double> tranche_internal_rates_of_return;
    
    CLOAnalytics();
    
    void calculateMetrics(const Pool<CorporateLoan>& loan_pool,
                         const std::map<std::string, CLOTranche>& tranches);
    std::map<std::string, double> getConcentrationAnalysis(const Pool<CorporateLoan>& pool) const;
    std::map<std::string, Amount> getCreditLossProjections() const;
    std::map<std::string, double> calculateTrancheMetrics(const std::map<std::string, CLOTranche>& tranches) const;
};

/**
 * Comprehensive CLO deal implementation
 */
class CLODeal : public DealBase {
private:
    std::map<std::string, CLOTranche> tranches_;
    CLOWaterfall waterfall_;
    Pool<CorporateLoan>* loan_pool_;
    CLOPortfolioManager portfolio_manager_;
    CLOAnalytics analytics_;
    
    // Deal-specific accounts
    std::string principal_collection_account_id_;
    std::string interest_collection_account_id_;
    std::string expense_account_id_;
    std::string reserve_account_id_;
    std::string reinvestment_account_id_;
    std::string equity_distribution_account_id_;
    
    // Management and fees
    Rate management_fee_rate_;
    Rate trustee_fee_rate_;
    Rate administrative_fee_rate_;
    Amount senior_management_fee_;
    Amount subordinate_management_fee_;
    
    // Performance tracking
    std::map<Date, Amount> historical_collections_;
    std::map<Date, Amount> historical_defaults_;
    std::map<Date, Amount> historical_recoveries_;
    std::map<Date, double> portfolio_metrics_history_;
    std::map<Date, std::map<std::string, double>> coverage_test_history_;

public:
    explicit CLODeal(const DealInfo& deal_info);
    ~CLODeal() override = default;
    
    // Deal setup
    void setLoanPool(Pool<CorporateLoan>* pool);
    void addTranche(const CLOTranche& tranche);
    void setupWaterfall(const CLOWaterfall& waterfall);
    void setupPortfolioManager(const CLOPortfolioManager& manager);
    void setManagementFees(Rate mgmt_fee, Rate trustee_fee, Rate admin_fee);
    
    // DealBase interface implementation
    void runWaterfall(const Date& paymentDate) override;
    Balance calculateDealValue(const Date& valuationDate) const override;
    std::vector<std::string> validate() const override;
    std::string getDealSummary() const override;
    void processPaymentDate(const Date& paymentDate) override;
    
    // CLO specific methods
    std::map<std::string, CLOTranche> getTranches() const { return tranches_; }
    CLOWaterfall getWaterfall() const { return waterfall_; }
    CLOPortfolioManager getPortfolioManager() const { return portfolio_manager_; }
    CLOAnalytics getAnalytics() const { return analytics_; }
    
    // Portfolio management during reinvestment period
    void processLoanDefault(const std::string& loan_id, Date default_date, Amount recovery_amount);
    void processLoanPrepayment(const std::string& loan_id, Date prepayment_date, Amount prepayment_amount);
    void reinvestProceeds(Amount proceeds, Date reinvestment_date);
    void disposeLoan(const std::string& loan_id, Date disposition_date, Amount sale_price);
    
    // Coverage tests and portfolio compliance
    bool runCoverageTests(Date test_date);
    std::map<std::string, bool> checkPortfolioCompliance() const;
    void handleCoverageTestFailure(const std::string& test_name, Date failure_date);
    
    // Credit and loss management
    void allocateLosses(Amount total_losses, Date loss_date);
    Amount calculateExpectedLosses() const;
    Amount calculateUnexpectedLosses(double confidence_level = 0.99) const;
    void updateCreditRatings(const std::map<std::string, std::string>& rating_changes);
    
    // Fee calculations and payments
    Amount calculateManagementFees(Date calculation_date) const;
    Amount calculateTrusteeFees(Date calculation_date) const;
    Amount calculateAdministrativeFees(Date calculation_date) const;
    void payFees(Date payment_date);
    
    // Performance analytics
    double calculatePortfolioYield() const;
    double calculateExcessSpread() const;
    double calculateWeightedAverageLife() const;
    std::map<std::string, double> getTrancheMetrics() const;
    std::map<std::string, double> getCoverageRatios() const;
    
    // Concentration and diversification analysis
    std::map<std::string, Amount> getSectorConcentration() const;
    std::map<std::string, Amount> getObligorConcentration() const;
    std::map<std::string, Amount> getRatingConcentration() const;
    std::map<std::string, Amount> getGeographicConcentration() const;
    
    // Stress testing and scenario analysis
    void applyDefaultScenario(double default_rate_multiplier);
    void applyRecoveryRateStress(double recovery_rate_shock);
    void applySpreadStress(double spread_shock);
    std::map<std::string, Amount> stressTestWaterfall(double loss_scenario, double spread_shock) const;
    
    // Reporting and compliance
    std::map<std::string, double> generateMonthlyReport(Date report_date) const;
    std::map<std::string, Amount> generateCashFlowReport(Date start_date, Date end_date) const;
    std::map<std::string, double> generatePortfolioStatistics() const;

private:
    // Internal processing methods
    void processLoanCollections(Date collection_date);
    void updatePortfolioAnalytics(Date update_date);
    void distributeCollections(Amount collections, Date distribution_date);
    void manageReinvestmentAccount(Date management_date);
    void processEquityDistributions(Date distribution_date);
    
    // Risk calculations
    double calculateConcentrationRisk() const;
    Amount calculateRequiredOvercollateralization() const;
    void updateCreditMetrics();
    void monitorCoverageTests();
    
    // Fee processing
    void accrueFees(Date accrual_date);
    void processManagementFeePayment(Date payment_date);
    void processTrusteeFeePayment(Date payment_date);
    
    // Compliance monitoring
    void checkReinvestmentCompliance();
    void checkTradingCompliance();
    void checkPortfolioQualityTests();
    
    // Reporting helpers
    void generateTrancheReports(Date report_date) const;
    void generatePortfolioReport(Date report_date) const;
    void generateCoverageTestReport(Date report_date) const;
};

} // namespace Structura