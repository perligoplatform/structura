#include <gtest/gtest.h>
#include "reporting/reporting_engine.h"
#include "core/deal_base.h"
#include <memory>
#include <fstream>

using namespace Structura;

// Mock deal class for testing
class MockReportingDeal : public DealBase {
private:
    mutable Balance mock_deal_value_;

public:
    MockReportingDeal(const DealInfo& info, Balance value = 1000000.0) 
        : DealBase(info), mock_deal_value_(value) {}

    void runWaterfall(const Date& paymentDate) override {
        // Mock implementation
        (void)paymentDate;
    }

    Balance calculateDealValue(const Date& valuationDate) const override {
        (void)valuationDate;
        return mock_deal_value_;
    }

    std::vector<std::string> validate() const override {
        return {};
    }

    std::string getDealSummary() const override {
        return "Mock Deal for Reporting Tests - Performance reporting test deal";
    }

    void processPaymentDate(const Date& paymentDate) override {
        (void)paymentDate;
        // Mock implementation
    }

    void setMockDealValue(Balance value) { mock_deal_value_ = value; }
};

class ReportingEngineTest : public ::testing::Test {
protected:
    void SetUp() override {
        DealInfo deal_info("Test Reporting Deal", "CLO", Date(15, QuantLib::January, 2024), Date(15, QuantLib::January, 2031));
        deal_info.currency = "USD";
        deal_info.description = "Test CLO Deal for Reporting";

        test_deal_ = std::make_unique<MockReportingDeal>(deal_info, 500000000.0); // $500M
        reporting_engine_ = std::make_unique<ReportingEngine>();
        
        test_date_ = Date(15, QuantLib::June, 2024);
    }

    std::unique_ptr<MockReportingDeal> test_deal_;
    std::unique_ptr<ReportingEngine> reporting_engine_;
    Date test_date_;
};

TEST_F(ReportingEngineTest, BasicConstruction) {
    EXPECT_TRUE(reporting_engine_ != nullptr);
    
    // Test that default templates are loaded
    auto available_types = reporting_engine_->getAvailableReportTypes();
    EXPECT_GT(available_types.size(), 0);
}

TEST_F(ReportingEngineTest, ReportTypeStringConversions) {
    EXPECT_EQ(reportTypeToString(ReportType::DEAL_PERFORMANCE), "Deal Performance");
    EXPECT_EQ(reportTypeToString(ReportType::RISK_ANALYTICS), "Risk Analytics");
    EXPECT_EQ(reportTypeToString(ReportType::CASHFLOW_PROJECTION), "Cashflow Projection");
    
    EXPECT_EQ(reportFormatToString(ReportFormat::JSON), "JSON");
    EXPECT_EQ(reportFormatToString(ReportFormat::HTML), "HTML");
    EXPECT_EQ(reportFormatToString(ReportFormat::CSV), "CSV");
}

TEST_F(ReportingEngineTest, DealPerformanceReportGeneration) {
    ReportDocument report = reporting_engine_->generateDealPerformanceReport(*test_deal_, test_date_);
    
    // Check report structure
    EXPECT_EQ(report.title, "Deal Performance Report");
    EXPECT_EQ(report.subtitle, "Test Reporting Deal");
    EXPECT_EQ(report.report_date, test_date_);
    EXPECT_EQ(report.author, "Structura Analytics Engine");
    
    // Should have multiple sections
    EXPECT_GE(report.sections.size(), 3);
    
    // Check for expected sections
    bool has_executive_summary = false;
    bool has_performance_section = false;
    bool has_risk_section = false;
    
    for (const auto& section : report.sections) {
        if (section.title == "Executive Summary") has_executive_summary = true;
        if (section.title == "Performance Analysis") has_performance_section = true;
        if (section.title == "Risk Analysis") has_risk_section = true;
    }
    
    EXPECT_TRUE(has_executive_summary);
    EXPECT_TRUE(has_performance_section);
    EXPECT_TRUE(has_risk_section);
}

TEST_F(ReportingEngineTest, ExecutiveSummarySection) {
    ReportDocument report = reporting_engine_->generateDealPerformanceReport(*test_deal_, test_date_);
    
    // Find executive summary section
    ReportSection* exec_summary = nullptr;
    for (auto& section : report.sections) {
        if (section.title == "Executive Summary") {
            exec_summary = &section;
            break;
        }
    }
    
    ASSERT_TRUE(exec_summary != nullptr);
    EXPECT_EQ(exec_summary->order, 1);
    
    // Check for key metrics
    EXPECT_TRUE(exec_summary->data.find("deal_status") != exec_summary->data.end());
    EXPECT_TRUE(exec_summary->data.find("deal_type") != exec_summary->data.end());
    
    // Deal status should be a string
    auto deal_status = exec_summary->data["deal_status"];
    EXPECT_TRUE(std::holds_alternative<std::string>(deal_status));
    
    std::string status_str = std::get<std::string>(deal_status);
    EXPECT_TRUE(status_str == "Pending" || status_str == "Active" || status_str == "Defaulted");
}

TEST_F(ReportingEngineTest, RiskAnalyticsReportGeneration) {
    ReportDocument report = reporting_engine_->generateRiskAnalyticsReport(*test_deal_, test_date_);
    
    EXPECT_EQ(report.title, "Risk Analytics Report");
    EXPECT_TRUE(report.subtitle.find("Risk Assessment") != std::string::npos);
    EXPECT_EQ(report.author, "Structura Risk Analytics");
    
    // Should have risk analysis and stress testing sections
    EXPECT_GE(report.sections.size(), 2);
    
    bool has_risk_section = false;
    bool has_stress_section = false;
    
    for (const auto& section : report.sections) {
        if (section.title == "Risk Analysis") has_risk_section = true;
        if (section.title == "Stress Testing Results") has_stress_section = true;
    }
    
    EXPECT_TRUE(has_risk_section);
    EXPECT_TRUE(has_stress_section);
}

TEST_F(ReportingEngineTest, CashflowProjectionReportGeneration) {
    Date end_date = test_date_ + 365; // 1 year projection
    ReportDocument report = reporting_engine_->generateCashflowProjectionReport(*test_deal_, test_date_, end_date);
    
    EXPECT_EQ(report.title, "Cashflow Projection Report");
    EXPECT_TRUE(report.subtitle.find("Forward Looking Analysis") != std::string::npos);
    EXPECT_EQ(report.author, "Structura Cashflow Analytics");
    
    // Should have cashflow projections
    EXPECT_GE(report.sections.size(), 1);
    
    const auto& projection_section = report.sections[0];
    EXPECT_EQ(projection_section.title, "Cashflow Projections");
    
    // Should have summary statistics
    EXPECT_TRUE(projection_section.data.find("confidence_level") != projection_section.data.end());
    EXPECT_TRUE(projection_section.data.find("projection_months") != projection_section.data.end());
    
    // Should have charts
    EXPECT_GT(projection_section.charts.size(), 0);
    
    const auto& chart = projection_section.charts[0];
    EXPECT_EQ(chart.title, "Projected Monthly Cashflows");
    EXPECT_EQ(chart.chart_type, "line");
}

TEST_F(ReportingEngineTest, JSONExportFormat) {
    ReportDocument report = reporting_engine_->generateDealPerformanceReport(*test_deal_, test_date_);
    std::string json_output = reporting_engine_->exportReport(report, ReportFormat::JSON);
    
    // Basic JSON structure checks
    EXPECT_TRUE(json_output.find("\"title\"") != std::string::npos);
    EXPECT_TRUE(json_output.find("\"subtitle\"") != std::string::npos);
    EXPECT_TRUE(json_output.find("\"sections\"") != std::string::npos);
    EXPECT_TRUE(json_output.find("Deal Performance Report") != std::string::npos);
    
    // Should be valid JSON structure
    EXPECT_TRUE(json_output.front() == '{');
    EXPECT_TRUE(json_output.back() == '\n');
    
    // Should contain deal name
    EXPECT_TRUE(json_output.find("Test Reporting Deal") != std::string::npos);
}

TEST_F(ReportingEngineTest, HTMLExportFormat) {
    ReportDocument report = reporting_engine_->generateDealPerformanceReport(*test_deal_, test_date_);
    std::string html_output = reporting_engine_->exportReport(report, ReportFormat::HTML);
    
    // Basic HTML structure checks
    EXPECT_TRUE(html_output.find("<!DOCTYPE html>") != std::string::npos);
    EXPECT_TRUE(html_output.find("<html>") != std::string::npos);
    EXPECT_TRUE(html_output.find("</html>") != std::string::npos);
    EXPECT_TRUE(html_output.find("<head>") != std::string::npos);
    EXPECT_TRUE(html_output.find("<body>") != std::string::npos);
    
    // Should contain report content
    EXPECT_TRUE(html_output.find("Deal Performance Report") != std::string::npos);
    EXPECT_TRUE(html_output.find("Test Reporting Deal") != std::string::npos);
    
    // Should have CSS styling
    EXPECT_TRUE(html_output.find("<style>") != std::string::npos);
    EXPECT_TRUE(html_output.find("font-family") != std::string::npos);
}

TEST_F(ReportingEngineTest, CSVExportFormat) {
    ReportDocument report = reporting_engine_->generateDealPerformanceReport(*test_deal_, test_date_);
    std::string csv_output = reporting_engine_->exportReport(report, ReportFormat::CSV);
    
    // Basic CSV structure checks
    EXPECT_TRUE(csv_output.find("Section,Metric,Value") != std::string::npos);
    
    // Should contain section data
    EXPECT_TRUE(csv_output.find("Executive Summary") != std::string::npos);
    EXPECT_TRUE(csv_output.find("Performance Analysis") != std::string::npos);
    
    // Should have proper CSV formatting with quotes
    size_t quote_count = std::count(csv_output.begin(), csv_output.end(), '"');
    EXPECT_GT(quote_count, 10); // Should have many quoted fields
}

TEST_F(ReportingEngineTest, TextExportFormat) {
    ReportDocument report = reporting_engine_->generateDealPerformanceReport(*test_deal_, test_date_);
    std::string text_output = reporting_engine_->exportReport(report, ReportFormat::TEXT);
    
    // Basic text structure checks
    EXPECT_TRUE(text_output.find("Deal Performance Report") != std::string::npos);
    EXPECT_TRUE(text_output.find("Test Reporting Deal") != std::string::npos);
    
    // Should have text formatting elements
    EXPECT_TRUE(text_output.find("=====") != std::string::npos); // Header separator
    EXPECT_TRUE(text_output.find("-----") != std::string::npos); // Section separator
    
    // Should contain structured information
    EXPECT_TRUE(text_output.find("Report Date:") != std::string::npos);
    EXPECT_TRUE(text_output.find("Generated:") != std::string::npos);
    EXPECT_TRUE(text_output.find("Author:") != std::string::npos);
}

TEST_F(ReportingEngineTest, XMLExportFormat) {
    ReportDocument report = reporting_engine_->generateDealPerformanceReport(*test_deal_, test_date_);
    std::string xml_output = reporting_engine_->exportReport(report, ReportFormat::XML);
    
    // Basic XML structure checks
    EXPECT_TRUE(xml_output.find("<?xml version=\"1.0\" encoding=\"UTF-8\"?>") != std::string::npos);
    EXPECT_TRUE(xml_output.find("<report>") != std::string::npos);
    EXPECT_TRUE(xml_output.find("</report>") != std::string::npos);
    
    // Should contain structured XML elements
    EXPECT_TRUE(xml_output.find("<title>") != std::string::npos);
    EXPECT_TRUE(xml_output.find("<sections>") != std::string::npos);
    EXPECT_TRUE(xml_output.find("<section>") != std::string::npos);
    EXPECT_TRUE(xml_output.find("<metric name=") != std::string::npos);
    
    // Should contain report content
    EXPECT_TRUE(xml_output.find("Deal Performance Report") != std::string::npos);
}

TEST_F(ReportingEngineTest, ReportSectionDataTypes) {
    ReportDocument report = reporting_engine_->generateDealPerformanceReport(*test_deal_, test_date_);
    
    // Find executive summary to test data types
    ReportSection* exec_summary = nullptr;
    for (auto& section : report.sections) {
        if (section.title == "Executive Summary") {
            exec_summary = &section;
            break;
        }
    }
    
    ASSERT_TRUE(exec_summary != nullptr);
    
    // Test different data types
    for (const auto& [key, value] : exec_summary->data) {
        // Should be one of the supported variant types
        bool is_valid_type = std::holds_alternative<double>(value) ||
                           std::holds_alternative<std::string>(value) ||
                           std::holds_alternative<std::vector<double>>(value);
        EXPECT_TRUE(is_valid_type) << "Invalid data type for key: " << key;
    }
}

TEST_F(ReportingEngineTest, ChartConfiguration) {
    ReportDocument report = reporting_engine_->generateDealPerformanceReport(*test_deal_, test_date_);
    
    // Find performance section which should have charts
    ReportSection* performance_section = nullptr;
    for (auto& section : report.sections) {
        if (section.title == "Performance Analysis") {
            performance_section = &section;
            break;
        }
    }
    
    ASSERT_TRUE(performance_section != nullptr);
    ASSERT_GT(performance_section->charts.size(), 0);
    
    const auto& chart = performance_section->charts[0];
    EXPECT_EQ(chart.title, "Deal Performance Over Time");
    EXPECT_EQ(chart.chart_type, "line");
    EXPECT_EQ(chart.x_axis_label, "Date");
    EXPECT_EQ(chart.y_axis_label, "Value");
    EXPECT_GT(chart.data_series.size(), 0);
    EXPECT_TRUE(chart.show_legend);
    EXPECT_TRUE(chart.show_grid);
}

TEST_F(ReportingEngineTest, ReportConfigCreation) {
    ReportConfig config;
    config.name = "Test Performance Report";
    config.type = ReportType::DEAL_PERFORMANCE;
    config.format = ReportFormat::HTML;
    config.frequency = ReportFrequency::MONTHLY;
    config.include_charts = true;
    config.include_raw_data = false;
    
    EXPECT_EQ(config.name, "Test Performance Report");
    EXPECT_EQ(config.type, ReportType::DEAL_PERFORMANCE);
    EXPECT_EQ(config.format, ReportFormat::HTML);
    EXPECT_EQ(config.frequency, ReportFrequency::MONTHLY);
    EXPECT_TRUE(config.include_charts);
    EXPECT_FALSE(config.include_raw_data);
}

TEST_F(ReportingEngineTest, ReportWithAnalyticsEngine) {
    // Create a custom analytics engine
    auto analytics_engine = std::make_unique<AnalyticsEngine>();
    
    // Add some time series data
    analytics_engine->createTimeSeries("deal_value", AnalyticsMetricType::PORTFOLIO_BALANCE);
    analytics_engine->addDataPoint("deal_value", Date(15, QuantLib::January, 2024), 500000000.0);
    analytics_engine->addDataPoint("deal_value", Date(15, QuantLib::February, 2024), 495000000.0);
    analytics_engine->addDataPoint("deal_value", Date(15, QuantLib::March, 2024), 490000000.0);
    
    // Create reporting engine with custom analytics
    ReportingEngine custom_reporting_engine(std::move(analytics_engine));
    
    ReportDocument report = custom_reporting_engine.generateDealPerformanceReport(*test_deal_, test_date_);
    
    // Should generate a valid report with enhanced analytics
    EXPECT_EQ(report.title, "Deal Performance Report");
    EXPECT_GE(report.sections.size(), 3);
    
    // Performance section should have more detailed metrics
    ReportSection* performance_section = nullptr;
    for (auto& section : report.sections) {
        if (section.title == "Performance Analysis") {
            performance_section = &section;
            break;
        }
    }
    
    ASSERT_TRUE(performance_section != nullptr);
    EXPECT_GT(performance_section->data.size(), 2); // Should have multiple metrics
}

TEST_F(ReportingEngineTest, FileExportFunctionality) {
    ReportDocument report = reporting_engine_->generateDealPerformanceReport(*test_deal_, test_date_);
    
    // Test file export (create temporary file)
    std::string test_filename = "/tmp/test_report.json";
    
    EXPECT_NO_THROW(reporting_engine_->saveReportToFile(report, test_filename, ReportFormat::JSON));
    
    // Verify file was created and has content
    std::ifstream file(test_filename);
    EXPECT_TRUE(file.is_open());
    
    if (file.is_open()) {
        std::string content((std::istreambuf_iterator<char>(file)),
                           std::istreambuf_iterator<char>());
        file.close();
        
        EXPECT_GT(content.length(), 100);
        EXPECT_TRUE(content.find("Deal Performance Report") != std::string::npos);
        
        // Clean up
        std::remove(test_filename.c_str());
    }
}

TEST_F(ReportingEngineTest, StressTestingSection) {
    ReportDocument report = reporting_engine_->generateRiskAnalyticsReport(*test_deal_, test_date_);
    
    // Find stress testing section
    ReportSection* stress_section = nullptr;
    for (auto& section : report.sections) {
        if (section.title == "Stress Testing Results") {
            stress_section = &section;
            break;
        }
    }
    
    ASSERT_TRUE(stress_section != nullptr);
    EXPECT_EQ(stress_section->order, 2);
    
    // Should have stress test scenarios
    EXPECT_TRUE(stress_section->data.find("base_case_loss") != stress_section->data.end());
    EXPECT_TRUE(stress_section->data.find("mild_stress_loss") != stress_section->data.end());
    EXPECT_TRUE(stress_section->data.find("severe_stress_loss") != stress_section->data.end());
    EXPECT_TRUE(stress_section->data.find("extreme_stress_loss") != stress_section->data.end());
    
    // Should have a chart
    EXPECT_GT(stress_section->charts.size(), 0);
    const auto& chart = stress_section->charts[0];
    EXPECT_EQ(chart.title, "Stress Test Scenarios");
    EXPECT_EQ(chart.chart_type, "bar");
}

TEST_F(ReportingEngineTest, CashflowProjectionData) {
    Date end_date = test_date_ + 365;
    ReportDocument report = reporting_engine_->generateCashflowProjectionReport(*test_deal_, test_date_, end_date);
    
    const auto& projection_section = report.sections[0];
    
    // Should have financial projections
    bool has_financial_data = false;
    for (const auto& [key, value] : projection_section.data) {
        if (key.find("total_") == 0 || key.find("collections") != std::string::npos) {
            has_financial_data = true;
            
            // Financial values should be reasonable (positive and not zero)
            if (std::holds_alternative<double>(value)) {
                double val = std::get<double>(value);
                EXPECT_GE(val, 0.0) << "Financial metric " << key << " should be non-negative";
            }
        }
    }
    
    EXPECT_TRUE(has_financial_data);
    
    // Should have confidence level
    auto confidence_it = projection_section.data.find("confidence_level");
    ASSERT_TRUE(confidence_it != projection_section.data.end());
    
    double confidence = std::get<double>(confidence_it->second);
    EXPECT_GT(confidence, 0.0);
    EXPECT_LE(confidence, 1.0);
}

TEST_F(ReportingEngineTest, PerformanceWithMultipleReports) {
    // Test generating multiple reports quickly
    auto start_time = std::chrono::high_resolution_clock::now();
    
    for (int i = 0; i < 10; ++i) {
        ReportDocument report = reporting_engine_->generateDealPerformanceReport(*test_deal_, test_date_);
        EXPECT_EQ(report.title, "Deal Performance Report");
        EXPECT_GE(report.sections.size(), 3);
    }
    
    auto end_time = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time);
    
    // Should generate 10 reports in reasonable time (less than 1 second)
    EXPECT_LT(duration.count(), 1000);
}