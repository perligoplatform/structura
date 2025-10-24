#pragma once

#include <map>
#include <vector>
#include <string>
#include <functional>
#include <memory>
#include <variant>
#include <chrono>

#include "core/types.h"
#include "core/deal_base.h"

namespace Structura {

/**
 * @brief Enumeration of supported analytics metrics
 */
enum class AnalyticsMetricType {
    // Performance Metrics
    PORTFOLIO_BALANCE,
    CURRENT_YIELD,
    WEIGHTED_AVERAGE_LIFE,
    DURATION,
    MODIFIED_DURATION,
    CONVEXITY,
    
    // Risk Metrics
    VALUE_AT_RISK,
    EXPECTED_SHORTFALL,
    CREDIT_LOSS_PROVISION,
    CONCENTRATION_RISK,
    LIQUIDITY_RISK,
    
    // Credit Metrics
    DEFAULT_PROBABILITY,
    LOSS_GIVEN_DEFAULT,
    EXPOSURE_AT_DEFAULT,
    EXPECTED_CREDIT_LOSSES,
    UNEXPECTED_CREDIT_LOSSES,
    
    // Operational Metrics
    PAYMENT_RATE,
    DELINQUENCY_RATE,
    CHARGE_OFF_RATE,
    RECOVERY_RATE,
    PREPAYMENT_RATE,
    
    // Tranche Metrics
    SUBORDINATION_LEVEL,
    CREDIT_ENHANCEMENT,
    EXCESS_SPREAD,
    COVERAGE_RATIO,
    TRANCHE_FACTOR,
    
    // Deal-Level Metrics
    DEAL_IRR,
    EQUITY_IRR,
    DEAL_MULTIPLE,
    BREAK_EVEN_DEFAULT_RATE,
    STRESS_TEST_RESULT
};

/**
 * @brief Time series data point for analytics
 */
struct AnalyticsDataPoint {
    Date date;
    double value;
    std::string category;
    std::map<std::string, std::string> metadata;
    
    AnalyticsDataPoint() = default;
    AnalyticsDataPoint(Date d, double v, const std::string& cat = "") 
        : date(d), value(v), category(cat) {}
};

/**
 * @brief Time series collection for analytics
 */
class AnalyticsTimeSeries {
private:
    std::vector<AnalyticsDataPoint> data_;
    std::string name_;
    AnalyticsMetricType metric_type_;

public:
    AnalyticsTimeSeries(const std::string& name, AnalyticsMetricType type);
    
    // Data management
    void addDataPoint(const AnalyticsDataPoint& point);
    void addDataPoint(Date date, double value, const std::string& category = "");
    void clearData();
    
    // Data access
    const std::vector<AnalyticsDataPoint>& getData() const { return data_; }
    std::vector<AnalyticsDataPoint> getDataForPeriod(Date start, Date end) const;
    std::vector<AnalyticsDataPoint> getDataForCategory(const std::string& category) const;
    
    // Statistics
    double getCurrentValue() const;
    double getValueAtDate(Date date) const;
    double getAverageValue() const;
    double getMinValue() const;
    double getMaxValue() const;
    double getStandardDeviation() const;
    double getVolatility() const;
    
    // Trend analysis
    double getTrend() const;
    double getGrowthRate() const;
    std::vector<double> getMovingAverage(int periods) const;
    
    // Name and type
    const std::string& getName() const { return name_; }
    AnalyticsMetricType getMetricType() const { return metric_type_; }
};

/**
 * @brief Scenario definition for stress testing
 */
struct StressScenario {
    std::string name;
    std::string description;
    std::map<std::string, double> parameters;
    double severity_level;  // 0.0 to 1.0 scale
    
    StressScenario() : severity_level(0.5) {}
    StressScenario(const std::string& n, const std::string& desc, double severity = 0.5)
        : name(n), description(desc), severity_level(severity) {}
};

/**
 * @brief Results from stress testing analysis
 */
struct StressTestResult {
    StressScenario scenario;
    std::map<std::string, double> metric_impacts;
    std::map<std::string, Amount> loss_projections;
    std::map<std::string, double> tranche_impacts;
    Date analysis_date;
    double overall_impact_score;
    
    StressTestResult() : analysis_date(Date()), overall_impact_score(0.0) {}
};

/**
 * @brief Performance attribution analysis
 */
struct PerformanceAttribution {
    std::map<std::string, double> factor_contributions;
    std::map<std::string, double> sector_contributions;
    std::map<std::string, double> selection_effects;
    std::map<std::string, double> allocation_effects;
    double total_return;
    double benchmark_return;
    double excess_return;
    
    PerformanceAttribution() : total_return(0.0), benchmark_return(0.0), excess_return(0.0) {}
};

/**
 * @brief Risk decomposition analysis
 */
struct RiskDecomposition {
    std::map<std::string, double> systematic_risk;
    std::map<std::string, double> idiosyncratic_risk;
    std::map<std::string, double> factor_exposures;
    double total_risk;
    double diversifiable_risk;
    double non_diversifiable_risk;
    
    RiskDecomposition() : total_risk(0.0), diversifiable_risk(0.0), non_diversifiable_risk(0.0) {}
};

/**
 * @brief Forward-looking analytics projections
 */
struct AnalyticsProjection {
    std::map<Date, std::map<std::string, double>> projected_metrics;
    std::map<Date, std::map<std::string, Amount>> projected_cashflows;
    std::map<std::string, double> confidence_intervals;
    Date projection_start_date;
    Date projection_end_date;
    std::string methodology;
    
    AnalyticsProjection() : projection_start_date(Date()), projection_end_date(Date()) {}
};

/**
 * @brief Core analytics engine for structured finance deals
 */
class AnalyticsEngine {
private:
    std::map<std::string, std::unique_ptr<AnalyticsTimeSeries>> time_series_;
    std::map<std::string, StressScenario> stress_scenarios_;
    std::map<std::string, StressTestResult> stress_test_results_;
    std::map<Date, std::map<std::string, double>> historical_metrics_;
    
    // Configuration
    double confidence_level_;
    int var_holding_period_;
    std::string benchmark_name_;
    
    // Calculation helpers
    double calculateVaR(const std::vector<double>& returns, double confidence) const;
    double calculateExpectedShortfall(const std::vector<double>& returns, double confidence) const;
    std::vector<double> generateMonteCarloScenarios(int num_scenarios, 
                                                   const std::map<std::string, double>& parameters) const;

public:
    AnalyticsEngine();
    explicit AnalyticsEngine(double confidence_level);
    ~AnalyticsEngine() = default;
    
    // Configuration
    void setConfidenceLevel(double confidence) { confidence_level_ = confidence; }
    void setVaRHoldingPeriod(int days) { var_holding_period_ = days; }
    void setBenchmark(const std::string& benchmark) { benchmark_name_ = benchmark; }
    
    // Time series management
    void createTimeSeries(const std::string& name, AnalyticsMetricType type);
    void addDataPoint(const std::string& series_name, Date date, double value, 
                     const std::string& category = "");
    AnalyticsTimeSeries* getTimeSeries(const std::string& name);
    const AnalyticsTimeSeries* getTimeSeries(const std::string& name) const;
    std::vector<std::string> getTimeSeriesNames() const;
    
    // Deal analytics calculation
    std::map<std::string, double> calculatePerformanceMetrics(const DealBase& deal, Date as_of_date) const;
    std::map<std::string, double> calculateRiskMetrics(const DealBase& deal, Date as_of_date) const;
    std::map<std::string, double> calculateCreditMetrics(const DealBase& deal, Date as_of_date) const;
    std::map<std::string, double> calculateOperationalMetrics(const DealBase& deal, Date as_of_date) const;
    
    // Comprehensive analytics
    std::map<std::string, double> calculateAllMetrics(const DealBase& deal, Date as_of_date) const;
    void updateHistoricalMetrics(const DealBase& deal, Date as_of_date);
    
    // Stress testing
    void addStressScenario(const StressScenario& scenario);
    StressTestResult runStressTest(const DealBase& deal, const std::string& scenario_name, 
                                  Date test_date) const;
    std::map<std::string, StressTestResult> runAllStressTests(const DealBase& deal, Date test_date) const;
    void addCustomStressTest(const std::string& name, 
                           std::function<StressTestResult(const DealBase&, Date)> test_function);
    
    // Performance attribution
    PerformanceAttribution analyzePerformanceAttribution(const DealBase& deal, 
                                                        Date start_date, Date end_date) const;
    
    // Risk decomposition
    RiskDecomposition decomposeRisk(const DealBase& deal, Date analysis_date) const;
    
    // Forward-looking analytics
    AnalyticsProjection generateProjections(const DealBase& deal, Date start_date, Date end_date,
                                           const std::map<std::string, double>& assumptions) const;
    
    // Benchmarking and comparison
    std::map<std::string, double> benchmarkPerformance(const DealBase& deal, Date as_of_date) const;
    std::map<std::string, double> compareToPeers(const DealBase& deal, 
                                                const std::vector<const DealBase*>& peer_deals,
                                                Date as_of_date) const;
    
    // Reporting and visualization
    std::map<std::string, std::vector<AnalyticsDataPoint>> generatePerformanceReport(
        const DealBase& deal, Date start_date, Date end_date) const;
    std::map<std::string, std::vector<AnalyticsDataPoint>> generateRiskReport(
        const DealBase& deal, Date start_date, Date end_date) const;
    std::string generateAnalyticsSummary(const DealBase& deal, Date as_of_date) const;
    
    // Data export
    void exportTimeSeriesData(const std::string& filename, const std::vector<std::string>& series_names) const;
    void exportMetricsHistory(const std::string& filename, Date start_date, Date end_date) const;
    
    // Utility functions
    std::string metricTypeToString(AnalyticsMetricType type) const;
    AnalyticsMetricType stringToMetricType(const std::string& type_str) const;
    
private:
    void initializeDefaultStressScenarios();
    double calculateMetricForDeal(const DealBase& deal, AnalyticsMetricType metric_type, 
                                 Date as_of_date) const;
    std::map<std::string, double> extractDealSpecificMetrics(const DealBase& deal, Date as_of_date) const;
};

} // namespace Structura