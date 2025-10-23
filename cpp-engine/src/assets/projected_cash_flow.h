#pragma once

#include "assets/asset_base.h"
#include <memory>
#include <optional>
#include <vector>
#include <map>

namespace Structura {

/**
 * Types of projected cash flow assets
 */
enum class ProjectedCashFlowType {
    STRUCTURED_SETTLEMENT,        // Legal settlement payments
    LOTTERY_PAYMENTS,            // Lottery annuity payments
    PENSION_BENEFITS,            // Pension payment streams
    ROYALTY_STREAM,              // Intellectual property royalties
    MINERAL_RIGHTS,              // Oil, gas, mineral royalty streams
    FILM_ROYALTIES,              // Entertainment industry royalties
    MUSIC_ROYALTIES,             // Music publishing and performance royalties
    PATENT_ROYALTIES,            // Patent licensing income
    FRANCHISE_FEES,              // Franchise fee income streams
    INSURANCE_SETTLEMENTS,       // Insurance annuity settlements
    DEFERRED_COMPENSATION,       // Executive deferred compensation
    SPORTS_CONTRACTS,            // Athlete contract payments
    REAL_ESTATE_RENTS,           // Predictable rental income
    GOVERNMENT_CONTRACTS,        // Government payment contracts
    UTILITY_PAYMENTS,            // Regulated utility payment streams
    INFRASTRUCTURE_TOLLS,        // Toll road/bridge revenues
    AIRPORT_CONCESSIONS,         // Airport concession revenues
    PARKING_REVENUES,            // Parking facility revenues
    VENDING_REVENUES,            // Vending machine income
    ATM_REVENUES,                // ATM fee income
    BILLBOARD_REVENUES,          // Advertising space revenues
    CELL_TOWER_LEASE,            // Cell tower lease payments
    SOLAR_POWER_PPA,             // Solar power purchase agreements
    WIND_POWER_PPA,              // Wind power purchase agreements
    OTHER_PROJECTED_FLOWS        // Other projected cash flows
};

/**
 * Payment frequency for cash flows
 */
enum class CashFlowFrequency {
    DAILY,                       // Daily payments
    WEEKLY,                      // Weekly payments  
    BIWEEKLY,                    // Bi-weekly payments
    MONTHLY,                     // Monthly payments
    QUARTERLY,                   // Quarterly payments
    SEMI_ANNUAL,                 // Semi-annual payments
    ANNUAL,                      // Annual payments
    IRREGULAR,                   // Irregular payment schedule
    ONE_TIME,                    // Single lump sum payment
    SEASONAL,                    // Seasonal payments
    MILESTONE_BASED              // Milestone-based payments
};

/**
 * Risk factors affecting cash flow certainty
 */
enum class CashFlowRiskFactor {
    COUNTERPARTY_RISK,           // Risk of counterparty default
    REGULATORY_RISK,             // Regulatory changes affecting payments
    MARKET_RISK,                 // Market conditions affecting payments
    OPERATIONAL_RISK,            // Operational issues affecting generation
    TECHNOLOGY_RISK,             // Technology obsolescence risk
    LEGAL_RISK,                  // Legal challenges to payment rights
    CONCENTRATION_RISK,          // Concentration in single payer/source
    LIQUIDITY_RISK,              // Liquidity of underlying asset
    INFLATION_RISK,              // Inflation eroding real value
    CURRENCY_RISK,               // Foreign exchange risk
    FORCE_MAJEURE_RISK,          // Natural disasters, war, etc.
    PERFORMANCE_RISK             // Performance-dependent payment risk
};

/**
 * Individual cash flow payment in the projected schedule
 */
struct ProjectedPayment {
    Date payment_date;               // Expected payment date
    Amount scheduled_amount;         // Scheduled payment amount
    Amount actual_amount;            // Actual amount received (if paid)
    bool is_received;                // Whether payment was received
    Date actual_payment_date;        // Actual date payment was received
    
    // Payment characteristics
    bool is_fixed;                   // Fixed vs variable payment
    bool is_guaranteed;              // Whether payment is guaranteed
    double certainty_factor;         // Certainty of payment (0-1)
    std::string payment_source;      // Source of payment
    
    // Adjustment factors
    Rate inflation_adjustment;       // Inflation adjustment rate
    Rate escalation_rate;           // Annual escalation rate
    Amount minimum_payment;          // Minimum payment amount
    Amount maximum_payment;          // Maximum payment amount
    
    // Payment conditions
    std::string payment_condition;   // Conditions for payment
    bool requires_performance;       // Performance-dependent payment
    std::string performance_metric;  // Performance measurement
    
    ProjectedPayment(Date date, Amount amount)
        : payment_date(date), scheduled_amount(amount), actual_amount(0.0),
          is_received(false), is_fixed(true), is_guaranteed(false),
          certainty_factor(0.95), inflation_adjustment(0.0),
          escalation_rate(0.0), minimum_payment(0.0), maximum_payment(0.0),
          requires_performance(false) {}
};

/**
 * Cash flow projection model
 */
struct CashFlowProjection {
    std::string projection_id;           // Unique projection identifier
    Date projection_date;                // Date projection was created
    std::string projection_methodology;  // How projection was created
    std::string data_source;             // Source of projection data
    
    // Projection parameters
    int projection_horizon_years;        // Projection period in years
    CashFlowFrequency payment_frequency;  // Payment frequency
    Amount total_projected_value;        // Total projected cash flows
    Amount net_present_value;            // NPV of cash flows
    Rate discount_rate;                  // Discount rate used
    
    // Stress testing scenarios
    Amount best_case_value;              // Best case scenario value
    Amount worst_case_value;             // Worst case scenario value
    Amount base_case_value;              // Base case scenario value
    double confidence_interval;          // Confidence level (e.g., 95%)
    
    // Validation
    bool is_validated;                   // Whether projection is validated
    Date validation_date;                // Date of validation
    std::string validation_method;       // Validation methodology
    std::string validator;               // Who validated the projection
    
    CashFlowProjection(const std::string& id, Date projection_date)
        : projection_id(id), projection_date(projection_date),
          projection_horizon_years(10), payment_frequency(CashFlowFrequency::MONTHLY),
          total_projected_value(0.0), net_present_value(0.0), discount_rate(0.06),
          best_case_value(0.0), worst_case_value(0.0), base_case_value(0.0),
          confidence_interval(0.95), is_validated(false) {}
};

/**
 * Performance metrics for cash flow generation
 */
struct PerformanceMetrics {
    std::string metric_name;             // Name of performance metric
    std::string metric_type;             // Type (revenue, volume, etc.)
    Amount current_value;                // Current metric value
    Amount projected_value;              // Projected metric value
    std::string measurement_unit;        // Unit of measurement
    
    // Historical performance
    std::vector<Amount> historical_values; // Historical metric values
    std::vector<Date> measurement_dates;   // Dates of measurements
    
    // Benchmark comparisons
    Amount industry_benchmark;           // Industry benchmark value
    Amount peer_group_average;           // Peer group average
    double percentile_ranking;           // Percentile ranking vs peers
    
    PerformanceMetrics(const std::string& name, Amount current_value)
        : metric_name(name), current_value(current_value),
          projected_value(current_value), industry_benchmark(0.0),
          peer_group_average(0.0), percentile_ranking(0.5) {}
};

/**
 * Projected cash flow asset implementation
 */
class ProjectedCashFlow {
private:
    std::string asset_id_;
    ProjectedCashFlowType cash_flow_type_;
    Status status_;
    
    // Asset characteristics
    Amount total_face_value_;              // Total face value of cash flows
    Amount purchased_amount_;              // Amount paid to acquire asset
    Rate purchase_yield_;                  // Yield at purchase
    Date acquisition_date_;                // Date asset was acquired
    Date cash_flow_start_date_;            // When cash flows begin
    Date cash_flow_end_date_;              // When cash flows end
    
    // Payment schedule
    std::vector<ProjectedPayment> payment_schedule_; // Projected payment schedule
    CashFlowFrequency payment_frequency_;   // Payment frequency
    int total_payment_count_;              // Total number of payments
    int payments_received_count_;          // Payments received to date
    
    // Projection model
    CashFlowProjection projection_model_;  // Cash flow projection details
    std::vector<CashFlowRiskFactor> risk_factors_; // Identified risk factors
    
    // Counterparty information
    std::optional<Obligor> primary_obligor_;     // Primary payment obligor
    std::vector<Obligor> secondary_obligors_;    // Secondary/backup obligors
    bool has_guarantee_;                   // Whether payments are guaranteed
    std::string guarantee_provider_;       // Provider of guarantee
    Amount guarantee_coverage_;            // Amount of guarantee coverage
    
    // Performance tracking
    std::vector<PerformanceMetrics> performance_metrics_; // Performance indicators
    Amount cumulative_payments_received_;   // Total payments received
    Amount cumulative_shortfalls_;         // Total payment shortfalls
    double payment_reliability_ratio_;     // Ratio of actual to projected
    
    // Valuation and analysis
    Amount current_market_value_;          // Current market value estimate
    Amount book_value_;                    // Book value of asset
    Rate current_yield_;                   // Current yield
    Rate yield_to_maturity_;               // Yield to maturity
    
    // Risk management
    double volatility_measure_;            // Cash flow volatility measure
    Amount value_at_risk_;                 // Value at Risk measure
    Amount expected_shortfall_;            // Expected shortfall
    std::map<std::string, double> sensitivity_analysis_; // Sensitivity factors
    
    // Legal and structural
    std::string underlying_agreement_;     // Underlying legal agreement
    std::string payment_priority_;         // Payment priority/seniority
    bool is_transferable_;                 // Whether asset is transferable
    std::vector<std::string> transfer_restrictions_; // Transfer restrictions
    
    // Servicing and administration
    std::optional<std::string> servicer_;  // Payment servicer/administrator
    Amount servicing_fee_rate_;            // Servicing fee rate
    Date last_servicer_report_date_;       // Last servicer report date
    std::string servicing_agreement_;      // Servicing agreement details
    
    // Helper methods
    void generateBasicPaymentSchedule();

public:
    ProjectedCashFlow(const std::string& id, ProjectedCashFlowType type,
                     Amount total_face_value, const CashFlowProjection& projection,
                     Date acquisition_date);
    
    // Core asset interface
    void processPayment(Amount payment_amount, Date payment_date);
    void applyDefault(Date default_date);
    void applyPartialPayment(Amount amount, Date payment_date);
    Balance getCurrentBalance() const;
    Amount calculateExpectedValue() const;
    void resetToOriginal();
    
    // Payment schedule management
    void setPaymentSchedule(const std::vector<ProjectedPayment>& schedule);
    void addProjectedPayment(const ProjectedPayment& payment);
    void updateProjectedPayment(size_t payment_index, const ProjectedPayment& updated_payment);
    void receiveScheduledPayment(size_t payment_index, Amount actual_amount, Date payment_date);
    
    // Cash flow projections
    void updateProjection(const CashFlowProjection& new_projection);
    void recalculateProjection(Rate new_discount_rate);
    Amount calculateNetPresentValue(Rate discount_rate) const;
    Amount calculateFutureValue(Date future_date, Rate growth_rate) const;
    
    // Performance monitoring
    void addPerformanceMetric(const PerformanceMetrics& metric);
    void updatePerformanceMetric(const std::string& metric_name, Amount new_value, Date measurement_date);
    double calculatePaymentReliabilityRatio() const;
    Amount calculateAveragePaymentShortfall() const;
    
    // Risk assessment
    void addRiskFactor(CashFlowRiskFactor risk_factor);
    void removeRiskFactor(CashFlowRiskFactor risk_factor);
    double assessOverallRisk() const;
    Amount calculateValueAtRisk(double confidence_level) const;
    void performSensitivityAnalysis(const std::map<std::string, double>& scenarios);
    
    // Valuation methods
    Amount calculateDiscountedCashFlowValue(Rate discount_rate) const;
    Amount calculateComparableAssetValue() const;
    Amount calculateLiquidationValue() const;
    void updateMarketValue(Amount new_market_value, Date valuation_date);
    
    // Stress testing
    Amount stressTestScenario(const std::string& scenario_name, 
                             const std::map<std::string, double>& stress_factors) const;
    void performMonteCarloAnalysis(int simulation_count);
    Amount calculateWorstCaseScenario() const;
    Amount calculateBestCaseScenario() const;
    
    // Counterparty management
    void setPrimaryObligor(const Obligor& obligor);
    void addSecondaryObligor(const Obligor& obligor);
    void setGuarantee(const std::string& provider, Amount coverage);
    double assessCounterpartyRisk() const;
    
    // Servicing and administration
    void setServicer(const std::string& servicer, Amount fee_rate);
    void processServicerReport(const std::string& report_data, Date report_date);
    Amount calculateServicingFees() const;
    
    // Legal and compliance
    void setTransferability(bool is_transferable, const std::vector<std::string>& restrictions);
    bool validateLegalStructure() const;
    void recordComplianceEvent(const std::string& event_type, Date event_date);
    
    // Analytics and reporting
    double calculateInternalRateOfReturn() const;
    double calculateModifiedDuration() const;
    Amount calculateCashOnCashReturn() const;
    double calculateVolatilityMeasure() const;
    
    // Portfolio integration
    double calculateCorrelation(const ProjectedCashFlow& other_asset) const;
    Amount calculateDiversificationBenefit(const std::vector<ProjectedCashFlow>& portfolio) const;
    
    // Getters
    const std::string& getAssetId() const { return asset_id_; }
    ProjectedCashFlowType getCashFlowType() const { return cash_flow_type_; }
    Status getStatus() const { return status_; }
    
    Amount getTotalFaceValue() const { return total_face_value_; }
    Amount getPurchasedAmount() const { return purchased_amount_; }
    Rate getPurchaseYield() const { return purchase_yield_; }
    Date getAcquisitionDate() const { return acquisition_date_; }
    Date getCashFlowStartDate() const { return cash_flow_start_date_; }
    Date getCashFlowEndDate() const { return cash_flow_end_date_; }
    
    const std::vector<ProjectedPayment>& getPaymentSchedule() const { return payment_schedule_; }
    CashFlowFrequency getPaymentFrequency() const { return payment_frequency_; }
    int getTotalPaymentCount() const { return total_payment_count_; }
    int getPaymentsReceivedCount() const { return payments_received_count_; }
    
    const CashFlowProjection& getProjectionModel() const { return projection_model_; }
    const std::vector<CashFlowRiskFactor>& getRiskFactors() const { return risk_factors_; }
    
    const std::optional<Obligor>& getPrimaryObligor() const { return primary_obligor_; }
    const std::vector<Obligor>& getSecondaryObligors() const { return secondary_obligors_; }
    bool hasGuarantee() const { return has_guarantee_; }
    const std::string& getGuaranteeProvider() const { return guarantee_provider_; }
    Amount getGuaranteeCoverage() const { return guarantee_coverage_; }
    
    const std::vector<PerformanceMetrics>& getPerformanceMetrics() const { return performance_metrics_; }
    Amount getCumulativePaymentsReceived() const { return cumulative_payments_received_; }
    Amount getCumulativeShortfalls() const { return cumulative_shortfalls_; }
    double getPaymentReliabilityRatio() const { return payment_reliability_ratio_; }
    
    Amount getCurrentMarketValue() const { return current_market_value_; }
    Amount getBookValue() const { return book_value_; }
    Rate getCurrentYield() const { return current_yield_; }
    Rate getYieldToMaturity() const { return yield_to_maturity_; }
    
    double getVolatilityMeasure() const { return volatility_measure_; }
    Amount getValueAtRisk() const { return value_at_risk_; }
    Amount getExpectedShortfall() const { return expected_shortfall_; }
    
    const std::string& getUnderlyingAgreement() const { return underlying_agreement_; }
    const std::string& getPaymentPriority() const { return payment_priority_; }
    bool isTransferable() const { return is_transferable_; }
    const std::vector<std::string>& getTransferRestrictions() const { return transfer_restrictions_; }
    
    const std::optional<std::string>& getServicer() const { return servicer_; }
    Amount getServicingFeeRate() const { return servicing_fee_rate_; }
    Date getLastServicerReportDate() const { return last_servicer_report_date_; }
    
    // Setters
    void setStatus(Status status) { status_ = status; }
    void setCashFlowDates(Date start_date, Date end_date);
    void setBookValue(Amount book_value) { book_value_ = book_value; }
    void setUnderlyingAgreement(const std::string& agreement) { underlying_agreement_ = agreement; }
    void setPaymentPriority(const std::string& priority) { payment_priority_ = priority; }
};

// Utility functions
std::string projectedCashFlowTypeToString(ProjectedCashFlowType type);
std::string paymentFrequencyToString(CashFlowFrequency frequency);
std::string cashFlowRiskFactorToString(CashFlowRiskFactor risk_factor);

// Payment frequency conversion functions
int paymentFrequencyToPaymentsPerYear(CashFlowFrequency frequency);
CashFlowFrequency paymentsPerYearToFrequency(int payments_per_year);
int paymentFrequencyToDays(CashFlowFrequency frequency);

// Cash flow analysis functions
Amount calculatePresentValue(const std::vector<ProjectedPayment>& payments, Rate discount_rate);
Amount calculateFutureValue(const std::vector<ProjectedPayment>& payments, Rate growth_rate, Date future_date);
double calculateInternalRateOfReturn(const std::vector<ProjectedPayment>& payments, Amount initial_investment);
double calculatePaybackPeriod(const std::vector<ProjectedPayment>& payments, Amount initial_investment);

// Risk assessment functions
double calculateCashFlowVolatility(const std::vector<ProjectedPayment>& payments);
Amount calculateExpectedShortfall(const std::vector<ProjectedPayment>& payments, 
                                Amount total_investment, double confidence_level);
double assessCounterpartyRisk(const Obligor& obligor, Amount exposure);

// Portfolio analysis functions
double calculatePortfolioCorrelation(const std::vector<ProjectedCashFlow>& portfolio);
Amount calculatePortfolioDiversificationBenefit(const std::vector<ProjectedCashFlow>& portfolio);
Amount calculatePortfolioValueAtRisk(const std::vector<ProjectedCashFlow>& portfolio, double confidence_level);

// Scenario analysis functions
std::map<std::string, Amount> performScenarioAnalysis(const ProjectedCashFlow& asset,
                                                     const std::map<std::string, std::map<std::string, double>>& scenarios);
Amount calculateStressTestValue(const ProjectedCashFlow& asset, const std::map<std::string, double>& stress_factors);

} // namespace Structura