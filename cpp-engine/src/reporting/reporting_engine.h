#pragma once

#include <map>
#include <vector>
#include <string>
#include <memory>
#include <functional>
#include <variant>
#include <fstream>

#include "core/types.h"
#include "core/deal_base.h"
#include "analytics/analytics_engine.h"

namespace Structura {

/**
 * @brief Enumeration of supported report types
 */
enum class ReportType {
    DEAL_PERFORMANCE,
    CASHFLOW_PROJECTION,
    RISK_ANALYTICS,
    CREDIT_ANALYSIS,
    PORTFOLIO_SUMMARY,
    COMPLIANCE_REPORT,
    WATERFALL_ANALYSIS,
    COVENANT_TESTING,
    CONCENTRATION_ANALYSIS,
    STRESS_TEST_RESULTS,
    MONTHLY_INVESTOR,
    QUARTERLY_TRUSTEE,
    ANNUAL_COMPLIANCE,
    DEAL_COMPARISON,
    CUSTOM_REPORT
};

/**
 * @brief Report output formats
 */
enum class ReportFormat {
    JSON,
    CSV,
    HTML,
    PDF,
    EXCEL,
    XML,
    TEXT
};

/**
 * @brief Report frequency for scheduled reports
 */
enum class ReportFrequency {
    DAILY,
    WEEKLY,
    MONTHLY,
    QUARTERLY,
    SEMI_ANNUALLY,
    ANNUALLY,
    ON_DEMAND
};

/**
 * @brief Report configuration parameters
 */
struct ReportConfig {
    std::string name;
    std::string description;
    ReportType type;
    ReportFormat format;
    ReportFrequency frequency;
    Date start_date;
    Date end_date;
    std::map<std::string, std::string> parameters;
    std::vector<std::string> recipients;
    bool include_charts;
    bool include_raw_data;
    std::string template_path;
    
    ReportConfig() : type(ReportType::DEAL_PERFORMANCE), format(ReportFormat::JSON), 
                    frequency(ReportFrequency::ON_DEMAND), include_charts(true), 
                    include_raw_data(false) {}
};

/**
 * @brief Chart configuration for visual reports
 */
struct ChartConfig {
    std::string title;
    std::string chart_type; // "line", "bar", "pie", "scatter", "heatmap"
    std::string x_axis_label;
    std::string y_axis_label;
    std::vector<std::string> data_series;
    std::map<std::string, std::string> styling;
    bool show_legend;
    bool show_grid;
    
    ChartConfig() : show_legend(true), show_grid(true) {}
};

/**
 * @brief Report section containing data and formatting
 */
struct ReportSection {
    std::string title;
    std::string description;
    std::map<std::string, std::variant<double, std::string, std::vector<double>>> data;
    std::vector<ChartConfig> charts;
    std::vector<std::map<std::string, std::string>> tables;
    std::string template_section;
    int order;
    
    ReportSection() : order(0) {}
};

/**
 * @brief Complete report document
 */
struct ReportDocument {
    std::string title;
    std::string subtitle;
    Date report_date;
    Date generation_date;
    std::string author;
    std::string version;
    std::map<std::string, std::string> metadata;
    std::vector<ReportSection> sections;
    std::string footer;
    
    ReportDocument() : report_date(Date()), generation_date(Date()), version("1.0") {}
};

/**
 * @brief Cashflow projection data structure
 */
struct CashflowProjectionData {
    std::vector<Date> projection_dates;
    std::map<std::string, std::vector<Amount>> cashflow_streams;
    std::map<std::string, Amount> summary_statistics;
    std::vector<std::string> assumptions;
    double confidence_level;
    
    CashflowProjectionData() : confidence_level(0.95) {}
};

/**
 * @brief Performance metrics dashboard data
 */
struct PerformanceDashboardData {
    std::map<std::string, double> key_metrics;
    std::map<Date, std::map<std::string, double>> historical_performance;
    std::map<std::string, std::string> benchmark_comparisons;
    std::vector<std::string> alerts;
    std::vector<std::string> recommendations;
    Date last_updated;
    
    PerformanceDashboardData() : last_updated(Date()) {}
};

/**
 * @brief Risk analytics report data
 */
struct RiskAnalyticsData {
    std::map<std::string, double> risk_metrics;
    std::map<std::string, std::vector<double>> stress_test_results;
    std::map<std::string, double> concentration_analysis;
    std::vector<std::string> risk_factors;
    std::map<std::string, std::string> mitigation_strategies;
    double overall_risk_score;
    
    RiskAnalyticsData() : overall_risk_score(0.0) {}
};

/**
 * @brief Advanced reporting system with multiple output formats
 */
class ReportingEngine {
private:
    std::unique_ptr<AnalyticsEngine> analytics_engine_;
    std::map<std::string, ReportConfig> report_templates_;
    std::map<std::string, std::string> html_templates_;
    std::map<ReportType, std::function<ReportDocument(const DealBase&, const ReportConfig&, Date)>> report_generators_;
    
    // Report generation helpers
    ReportSection generateExecutiveSummary(const DealBase& deal, Date report_date) const;
    ReportSection generatePerformanceSection(const DealBase& deal, Date report_date) const;
    ReportSection generateRiskSection(const DealBase& deal, Date report_date) const;
    ReportSection generateCashflowSection(const DealBase& deal, Date report_date) const;
    ReportSection generateComplianceSection(const DealBase& deal, Date report_date) const;
    
    // Data processors
    CashflowProjectionData generateCashflowProjection(const DealBase& deal, Date start_date, Date end_date) const;
    PerformanceDashboardData generatePerformanceDashboard(const DealBase& deal, Date as_of_date) const;
    RiskAnalyticsData generateRiskAnalytics(const DealBase& deal, Date as_of_date) const;
    
    // Output formatters
    std::string formatAsJSON(const ReportDocument& report) const;
    std::string formatAsCSV(const ReportDocument& report) const;
    std::string formatAsHTML(const ReportDocument& report) const;
    std::string formatAsXML(const ReportDocument& report) const;
    std::string formatAsText(const ReportDocument& report) const;
    
    // Chart generators
    std::string generateChartHTML(const ChartConfig& config, const ReportSection& section) const;
    std::string generateTableHTML(const std::vector<std::map<std::string, std::string>>& table_data) const;
    
    // Template processing
    std::string processTemplate(const std::string& template_content, const std::map<std::string, std::string>& variables) const;
    void loadDefaultTemplates();

public:
    ReportingEngine();
    explicit ReportingEngine(std::unique_ptr<AnalyticsEngine> analytics_engine);
    ~ReportingEngine() = default;
    
    // Configuration
    void setAnalyticsEngine(std::unique_ptr<AnalyticsEngine> engine);
    void addReportTemplate(const std::string& name, const ReportConfig& config);
    void loadHTMLTemplate(const std::string& name, const std::string& template_path);
    void setHTMLTemplate(const std::string& name, const std::string& template_content);
    
    // Report generation
    ReportDocument generateReport(const DealBase& deal, const ReportConfig& config, Date report_date) const;
    ReportDocument generateDealPerformanceReport(const DealBase& deal, Date report_date) const;
    ReportDocument generateCashflowProjectionReport(const DealBase& deal, Date start_date, Date end_date) const;
    ReportDocument generateRiskAnalyticsReport(const DealBase& deal, Date as_of_date) const;
    ReportDocument generateComplianceReport(const DealBase& deal, Date report_date) const;
    ReportDocument generateMonthlyInvestorReport(const DealBase& deal, Date month_end) const;
    ReportDocument generateQuarterlyTrusteeReport(const DealBase& deal, Date quarter_end) const;
    
    // Comparative analysis
    ReportDocument generateDealComparisonReport(const std::vector<const DealBase*>& deals, Date as_of_date) const;
    ReportDocument generatePortfolioSummaryReport(const std::vector<const DealBase*>& deals, Date as_of_date) const;
    
    // Output formatting
    std::string exportReport(const ReportDocument& report, ReportFormat format) const;
    void saveReportToFile(const ReportDocument& report, const std::string& filename, ReportFormat format) const;
    
    // Dashboard capabilities
    PerformanceDashboardData generateRealTimeDashboard(const DealBase& deal, Date as_of_date) const;
    std::string generateDashboardHTML(const PerformanceDashboardData& dashboard_data) const;
    
    // Scheduled reporting
    std::vector<std::string> getScheduledReports(Date current_date) const;
    void generateScheduledReports(const std::vector<const DealBase*>& deals, Date current_date) const;
    
    // Custom reports
    void registerCustomReportGenerator(ReportType type, 
                                     std::function<ReportDocument(const DealBase&, const ReportConfig&, Date)> generator);
    
    // Utilities
    std::vector<std::string> getAvailableReportTypes() const;
    std::vector<std::string> getAvailableFormats() const;
    ReportConfig getReportTemplate(const std::string& name) const;
    
    // Data export helpers
    void exportPerformanceMetricsToCSV(const DealBase& deal, Date start_date, Date end_date, 
                                      const std::string& filename) const;
    void exportCashflowProjectionToExcel(const CashflowProjectionData& projection, 
                                        const std::string& filename) const;
    
    // Validation and quality checks
    std::vector<std::string> validateReportData(const ReportDocument& report) const;
    bool isReportComplete(const ReportDocument& report) const;
    double calculateReportQualityScore(const ReportDocument& report) const;
};

/**
 * @brief Report scheduler for automated report generation
 */
class ReportScheduler {
private:
    std::map<std::string, ReportConfig> scheduled_reports_;
    std::vector<std::string> execution_log_;
    ReportingEngine* reporting_engine_;

public:
    explicit ReportScheduler(ReportingEngine* engine);
    
    void scheduleReport(const std::string& name, const ReportConfig& config);
    void removeScheduledReport(const std::string& name);
    std::vector<std::string> getDueReports(Date current_date) const;
    void executeScheduledReports(const std::vector<const DealBase*>& deals, Date current_date);
    std::vector<std::string> getExecutionLog() const;
    void clearExecutionLog();
};

/**
 * @brief Report template builder for creating custom report layouts
 */
class ReportTemplateBuilder {
private:
    ReportConfig config_;
    std::vector<std::string> section_order_;
    std::map<std::string, std::string> custom_templates_;

public:
    ReportTemplateBuilder(const std::string& report_name);
    
    ReportTemplateBuilder& setType(ReportType type);
    ReportTemplateBuilder& setFormat(ReportFormat format);
    ReportTemplateBuilder& setFrequency(ReportFrequency frequency);
    ReportTemplateBuilder& addParameter(const std::string& key, const std::string& value);
    ReportTemplateBuilder& addRecipient(const std::string& email);
    ReportTemplateBuilder& addSection(const std::string& section_name);
    ReportTemplateBuilder& setTemplate(const std::string& template_content);
    ReportTemplateBuilder& enableCharts(bool enable = true);
    ReportTemplateBuilder& includeRawData(bool include = true);
    
    ReportConfig build() const;
};

// Utility functions
std::string reportTypeToString(ReportType type);
std::string reportFormatToString(ReportFormat format);
std::string reportFrequencyToString(ReportFrequency frequency);
ReportType stringToReportType(const std::string& type_str);
ReportFormat stringToReportFormat(const std::string& format_str);
ReportFrequency stringToReportFrequency(const std::string& frequency_str);

// Template constants
extern const std::string DEFAULT_HTML_TEMPLATE;
extern const std::string DASHBOARD_HTML_TEMPLATE;
extern const std::string INVESTOR_REPORT_TEMPLATE;
extern const std::string COMPLIANCE_REPORT_TEMPLATE;

} // namespace Structura