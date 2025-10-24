#include <gtest/gtest.h>
#include "analytics/analytics_engine.h"
#include "core/deal_base.h"
#include <memory>

using namespace Structura;

// Mock deal class for testing
class MockDeal : public DealBase {
private:
    mutable Balance mock_deal_value_;
    mutable int waterfall_call_count_;

public:
    MockDeal(const DealInfo& info, Balance value = 1000000.0) 
        : DealBase(info), mock_deal_value_(value), waterfall_call_count_(0) {}

    void runWaterfall(const Date& paymentDate) override {
        waterfall_call_count_++;
    }

    Balance calculateDealValue(const Date& valuationDate) const override {
        return mock_deal_value_;
    }

    std::vector<std::string> validate() const override {
        return {};
    }

    std::string getDealSummary() const override {
        return "Mock CLO Deal for Testing - Active portfolio with corporate loans";
    }

    void processPaymentDate(const Date& paymentDate) override {
        // Mock implementation
    }

    // Test helpers
    void setMockDealValue(Balance value) { mock_deal_value_ = value; }
    int getWaterfallCallCount() const { return waterfall_call_count_; }
};

class AnalyticsEngineTest : public ::testing::Test {
protected:
    void SetUp() override {
        DealInfo deal_info("Test CLO Deal", "CLO", Date(15, QuantLib::January, 2024), Date(15, QuantLib::January, 2031));
        deal_info.currency = "USD";
        deal_info.description = "Test CLO Deal for Analytics";

        test_deal_ = std::make_unique<MockDeal>(deal_info, 475000000.0); // $475M current
        analytics_engine_ = std::make_unique<AnalyticsEngine>(0.95);
        
        test_date_ = Date(15, QuantLib::June, 2024); // 5 months after closing
    }

    std::unique_ptr<MockDeal> test_deal_;
    std::unique_ptr<AnalyticsEngine> analytics_engine_;
    Date test_date_;
};

TEST_F(AnalyticsEngineTest, BasicConstruction) {
    EXPECT_TRUE(analytics_engine_ != nullptr);
    EXPECT_EQ(analytics_engine_->getTimeSeriesNames().size(), 0);
}

TEST_F(AnalyticsEngineTest, TimeSeriesManagement) {
    // Create time series
    analytics_engine_->createTimeSeries("deal_value", AnalyticsMetricType::PORTFOLIO_BALANCE);
    analytics_engine_->createTimeSeries("payment_rate", AnalyticsMetricType::PAYMENT_RATE);
    
    auto names = analytics_engine_->getTimeSeriesNames();
    EXPECT_EQ(names.size(), 2);
    EXPECT_TRUE(std::find(names.begin(), names.end(), "deal_value") != names.end());
    EXPECT_TRUE(std::find(names.begin(), names.end(), "payment_rate") != names.end());
    
    // Add data points
    analytics_engine_->addDataPoint("deal_value", Date(15, QuantLib::January, 2024), 500000000.0);
    analytics_engine_->addDataPoint("deal_value", Date(15, QuantLib::February, 2024), 495000000.0);
    analytics_engine_->addDataPoint("deal_value", Date(15, QuantLib::March, 2024), 490000000.0);
    
    auto* ts = analytics_engine_->getTimeSeries("deal_value");
    ASSERT_TRUE(ts != nullptr);
    EXPECT_EQ(ts->getData().size(), 3);
    EXPECT_DOUBLE_EQ(ts->getCurrentValue(), 490000000.0);
}

TEST_F(AnalyticsEngineTest, TimeSeriesStatistics) {
    analytics_engine_->createTimeSeries("test_series", AnalyticsMetricType::CURRENT_YIELD);
    
    // Add test data
    std::vector<double> test_values = {100.0, 105.0, 103.0, 108.0, 110.0, 107.0, 112.0};
    for (size_t i = 0; i < test_values.size(); ++i) {
        Date date(static_cast<int>(1 + i), QuantLib::January, 2024);
        analytics_engine_->addDataPoint("test_series", date, test_values[i]);
    }
    
    auto* ts = analytics_engine_->getTimeSeries("test_series");
    ASSERT_TRUE(ts != nullptr);
    
    // Test statistics
    EXPECT_DOUBLE_EQ(ts->getCurrentValue(), 112.0);
    EXPECT_DOUBLE_EQ(ts->getMinValue(), 100.0);
    EXPECT_DOUBLE_EQ(ts->getMaxValue(), 112.0);
    
    double expected_avg = (100 + 105 + 103 + 108 + 110 + 107 + 112) / 7.0;
    EXPECT_NEAR(ts->getAverageValue(), expected_avg, 0.01);
    
    // Test growth rate
    double expected_growth = (112.0 - 100.0) / 100.0;
    EXPECT_NEAR(ts->getGrowthRate(), expected_growth, 0.01);
    
    // Test trend (should be positive)
    EXPECT_GT(ts->getTrend(), 0.0);
}

TEST_F(AnalyticsEngineTest, PerformanceMetricsCalculation) {
    auto metrics = analytics_engine_->calculatePerformanceMetrics(*test_deal_, test_date_);
    
    // Check basic metrics
    EXPECT_TRUE(metrics.find("deal_value") != metrics.end());
    EXPECT_DOUBLE_EQ(metrics["deal_value"], 475000000.0);
    
    EXPECT_TRUE(metrics.find("deal_factor") != metrics.end());
    EXPECT_NEAR(metrics["deal_factor"], 0.95, 0.01); // 475M / 500M
    
    EXPECT_TRUE(metrics.find("deal_age_days") != metrics.end());
    EXPECT_GT(metrics["deal_age_days"], 100.0); // Should be around 150 days
    
    EXPECT_TRUE(metrics.find("approximate_irr") != metrics.end());
    // IRR should be negative given the value decline
    EXPECT_LT(metrics["approximate_irr"], 0.0);
}

TEST_F(AnalyticsEngineTest, RiskMetricsCalculation) {
    // Create some historical data for VaR calculation
    analytics_engine_->createTimeSeries("deal_value", AnalyticsMetricType::PORTFOLIO_BALANCE);
    
    // Add historical deal values with some volatility
    std::vector<double> values = {500000000, 495000000, 498000000, 492000000, 
                                 490000000, 485000000, 488000000, 475000000};
    std::vector<QuantLib::Month> months = {QuantLib::January, QuantLib::February, QuantLib::March, QuantLib::April,
                                          QuantLib::May, QuantLib::June, QuantLib::July, QuantLib::August};
    for (size_t i = 0; i < values.size(); ++i) {
        Date date(15, months[i], 2024);
        analytics_engine_->addDataPoint("deal_value", date, values[i]);
    }
    
    auto metrics = analytics_engine_->calculateRiskMetrics(*test_deal_, test_date_);
    
    // Check risk metrics
    EXPECT_TRUE(metrics.find("value_at_risk") != metrics.end());
    EXPECT_GT(metrics["value_at_risk"], 0.0);
    
    EXPECT_TRUE(metrics.find("expected_shortfall") != metrics.end());
    EXPECT_GT(metrics["expected_shortfall"], 0.0);
    
    EXPECT_TRUE(metrics.find("volatility") != metrics.end());
    EXPECT_GT(metrics["volatility"], 0.0);
    
    EXPECT_TRUE(metrics.find("concentration_risk_score") != metrics.end());
}

TEST_F(AnalyticsEngineTest, CreditMetricsCalculation) {
    auto metrics = analytics_engine_->calculateCreditMetrics(*test_deal_, test_date_);
    
    // Check credit metrics
    EXPECT_TRUE(metrics.find("default_probability") != metrics.end());
    EXPECT_GE(metrics["default_probability"], 0.0);
    EXPECT_LE(metrics["default_probability"], 1.0);
    
    EXPECT_TRUE(metrics.find("loss_given_default") != metrics.end());
    EXPECT_GE(metrics["loss_given_default"], 0.0);
    EXPECT_LE(metrics["loss_given_default"], 1.0);
    
    EXPECT_TRUE(metrics.find("expected_credit_losses") != metrics.end());
    EXPECT_GE(metrics["expected_credit_losses"], 0.0);
    
    EXPECT_TRUE(metrics.find("credit_quality_score") != metrics.end());
    EXPECT_GE(metrics["credit_quality_score"], 0.0);
    EXPECT_LE(metrics["credit_quality_score"], 1.0);
}

TEST_F(AnalyticsEngineTest, OperationalMetricsCalculation) {
    // Create payment rate time series
    analytics_engine_->createTimeSeries("payment_rate", AnalyticsMetricType::PAYMENT_RATE);
    
    // Add some payment rate data
    std::vector<double> payment_rates = {0.98, 0.97, 0.99, 0.96, 0.98, 0.97};
    std::vector<QuantLib::Month> months = {QuantLib::January, QuantLib::February, QuantLib::March, 
                                          QuantLib::April, QuantLib::May, QuantLib::June};
    for (size_t i = 0; i < payment_rates.size(); ++i) {
        Date date(15, months[i], 2024);
        analytics_engine_->addDataPoint("payment_rate", date, payment_rates[i]);
    }
    
    auto metrics = analytics_engine_->calculateOperationalMetrics(*test_deal_, test_date_);
    
    // Check operational metrics
    EXPECT_TRUE(metrics.find("average_payment_rate") != metrics.end());
    EXPECT_GT(metrics["average_payment_rate"], 0.9);
    EXPECT_LT(metrics["average_payment_rate"], 1.0);
    
    EXPECT_TRUE(metrics.find("operational_efficiency") != metrics.end());
    EXPECT_GE(metrics["operational_efficiency"], 0.0);
    EXPECT_LE(metrics["operational_efficiency"], 1.0);
    
    EXPECT_TRUE(metrics.find("account_utilization") != metrics.end());
}

TEST_F(AnalyticsEngineTest, AllMetricsCalculation) {
    auto metrics = analytics_engine_->calculateAllMetrics(*test_deal_, test_date_);
    
    // Should have metrics from all categories
    EXPECT_GT(metrics.size(), 10);
    
    // Should have calculation timestamp
    EXPECT_TRUE(metrics.find("calculation_timestamp") != metrics.end());
    
    // Verify key metrics exist
    EXPECT_TRUE(metrics.find("deal_value") != metrics.end());
    EXPECT_TRUE(metrics.find("default_probability") != metrics.end());
    EXPECT_TRUE(metrics.find("operational_efficiency") != metrics.end());
}

TEST_F(AnalyticsEngineTest, HistoricalMetricsUpdate) {
    // Update historical metrics multiple times
    analytics_engine_->updateHistoricalMetrics(*test_deal_, Date(15, QuantLib::January, 2024));
    analytics_engine_->updateHistoricalMetrics(*test_deal_, Date(15, QuantLib::February, 2024));
    analytics_engine_->updateHistoricalMetrics(*test_deal_, Date(15, QuantLib::March, 2024));
    
    // Should have created time series automatically
    auto names = analytics_engine_->getTimeSeriesNames();
    EXPECT_GT(names.size(), 5);
    
    // Check that deal_value series has data
    auto* deal_value_ts = analytics_engine_->getTimeSeries("deal_value");
    ASSERT_TRUE(deal_value_ts != nullptr);
    EXPECT_EQ(deal_value_ts->getData().size(), 3);
}

TEST_F(AnalyticsEngineTest, ConfigurationMethods) {
    // Test configuration
    analytics_engine_->setConfidenceLevel(0.99);
    analytics_engine_->setVaRHoldingPeriod(10);
    analytics_engine_->setBenchmark("Investment Grade CLO Index");
    
    // Configuration should not cause errors
    auto metrics = analytics_engine_->calculateRiskMetrics(*test_deal_, test_date_);
    EXPECT_TRUE(metrics.find("value_at_risk") != metrics.end() || 
                metrics.find("concentration_risk_score") != metrics.end());
}

TEST_F(AnalyticsEngineTest, AnalyticsSummaryGeneration) {
    std::string summary = analytics_engine_->generateAnalyticsSummary(*test_deal_, test_date_);
    
    // Should contain key information
    EXPECT_TRUE(summary.find("Test CLO Deal") != std::string::npos);
    EXPECT_TRUE(summary.find("ANALYTICS SUMMARY") != std::string::npos);
    EXPECT_TRUE(summary.find("Pending") != std::string::npos || summary.find("Active") != std::string::npos);
    EXPECT_TRUE(summary.find("Deal Value") != std::string::npos);
    
    // Should not be empty
    EXPECT_GT(summary.length(), 100);
}

TEST_F(AnalyticsEngineTest, MetricTypeStringConversion) {
    std::string portfolio_balance_str = analytics_engine_->metricTypeToString(AnalyticsMetricType::PORTFOLIO_BALANCE);
    EXPECT_EQ(portfolio_balance_str, "Portfolio Balance");
    
    std::string yield_str = analytics_engine_->metricTypeToString(AnalyticsMetricType::CURRENT_YIELD);
    EXPECT_EQ(yield_str, "Current Yield");
    
    std::string var_str = analytics_engine_->metricTypeToString(AnalyticsMetricType::VALUE_AT_RISK);
    EXPECT_EQ(var_str, "Value at Risk");
}

TEST_F(AnalyticsEngineTest, TimeSeriesDataFiltering) {
    analytics_engine_->createTimeSeries("test_data", AnalyticsMetricType::PAYMENT_RATE);
    
    // Add data with different categories
    analytics_engine_->addDataPoint("test_data", Date(15, QuantLib::January, 2024), 0.98, "monthly");
    analytics_engine_->addDataPoint("test_data", Date(30, QuantLib::January, 2024), 0.97, "monthly");
    analytics_engine_->addDataPoint("test_data", Date(1, QuantLib::February, 2024), 0.99, "daily");
    analytics_engine_->addDataPoint("test_data", Date(15, QuantLib::February, 2024), 0.96, "monthly");
    
    auto* ts = analytics_engine_->getTimeSeries("test_data");
    ASSERT_TRUE(ts != nullptr);
    
    // Test period filtering
    auto period_data = ts->getDataForPeriod(Date(1, QuantLib::January, 2024), Date(31, QuantLib::January, 2024));
    EXPECT_EQ(period_data.size(), 2);
    
    // Test category filtering
    auto monthly_data = ts->getDataForCategory("monthly");
    EXPECT_EQ(monthly_data.size(), 3);
    
    auto daily_data = ts->getDataForCategory("daily");
    EXPECT_EQ(daily_data.size(), 1);
}

TEST_F(AnalyticsEngineTest, MovingAverageCalculation) {
    analytics_engine_->createTimeSeries("price_series", AnalyticsMetricType::PORTFOLIO_BALANCE);
    
    // Add 10 data points
    std::vector<double> prices = {100, 102, 101, 105, 103, 107, 106, 109, 108, 110};
    for (size_t i = 0; i < prices.size(); ++i) {
        Date date(static_cast<int>(1 + i), QuantLib::January, 2024);
        analytics_engine_->addDataPoint("price_series", date, prices[i]);
    }
    
    auto* ts = analytics_engine_->getTimeSeries("price_series");
    ASSERT_TRUE(ts != nullptr);
    
    // Test 3-period moving average
    auto ma3 = ts->getMovingAverage(3);
    EXPECT_EQ(ma3.size(), 8); // 10 - 3 + 1
    
    // First MA should be average of first 3 prices
    double expected_first_ma = (100 + 102 + 101) / 3.0;
    EXPECT_NEAR(ma3[0], expected_first_ma, 0.01);
    
    // Test 5-period moving average
    auto ma5 = ts->getMovingAverage(5);
    EXPECT_EQ(ma5.size(), 6); // 10 - 5 + 1
}

// Performance test for large datasets
TEST_F(AnalyticsEngineTest, PerformanceWithLargeDataset) {
    analytics_engine_->createTimeSeries("large_dataset", AnalyticsMetricType::PORTFOLIO_BALANCE);
    
    // Add 1000 data points
    auto start_time = std::chrono::high_resolution_clock::now();
    Date base_date(1, QuantLib::January, 2024);
    for (int i = 0; i < 1000; ++i) {
        Date date = base_date + i; // Add i days
        double value = 1000000 + (i * 100) + (std::sin(i * 0.1) * 10000);
        analytics_engine_->addDataPoint("large_dataset", date, value);
    }
    auto end_time = std::chrono::high_resolution_clock::now();
    
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time);
    EXPECT_LT(duration.count(), 1000); // Should complete in less than 1 second
    
    auto* ts = analytics_engine_->getTimeSeries("large_dataset");
    ASSERT_TRUE(ts != nullptr);
    EXPECT_EQ(ts->getData().size(), 1000);
    
    // Test statistics on large dataset
    start_time = std::chrono::high_resolution_clock::now();
    double avg = ts->getAverageValue();
    double std_dev = ts->getStandardDeviation();
    double trend = ts->getTrend();
    end_time = std::chrono::high_resolution_clock::now();
    
    duration = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time);
    EXPECT_LT(duration.count(), 100); // Statistics should be fast
    
    EXPECT_GT(avg, 1000000);
    EXPECT_GT(std_dev, 0);
    EXPECT_GT(trend, 0); // Should have positive trend
}