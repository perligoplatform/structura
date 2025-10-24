#include <gtest/gtest.h>
#include "../src/core/cashflow.h"

using namespace Structura;

class CashflowTest : public ::testing::Test {
protected:
    void SetUp() override {
        // Set up test data
        test_date = Date(15, QuantLib::Month::January, 2024);
        test_balance = 1000.0;
        test_rate = 0.05;
    }

    Date test_date;
    Balance test_balance;
    Rate test_rate;
};

// Test CumulativeStat basic operations
TEST_F(CashflowTest, CumulativeStatOperations) {
    CumulativeStat stat1(100.0, 50.0, 25.0, 10.0, 5.0, 2.0);
    CumulativeStat stat2(200.0, 75.0, 30.0, 15.0, 8.0, 3.0);
    
    // Test addition
    CumulativeStat sum = stat1 + stat2;
    EXPECT_DOUBLE_EQ(sum.cum_principal, 300.0);
    EXPECT_DOUBLE_EQ(sum.cum_prepay, 125.0);
    EXPECT_DOUBLE_EQ(sum.cum_delinq, 55.0);
    
    // Test subtraction
    CumulativeStat diff = stat2 - stat1;
    EXPECT_DOUBLE_EQ(diff.cum_principal, 100.0);
    EXPECT_DOUBLE_EQ(diff.cum_prepay, 25.0);
    
    // Test scaling
    CumulativeStat scaled = stat1.scale(2.0);
    EXPECT_DOUBLE_EQ(scaled.cum_principal, 200.0);
    EXPECT_DOUBLE_EQ(scaled.cum_prepay, 100.0);
}

// Test TsRow variant creation and date extraction
TEST_F(CashflowTest, TsRowCreation) {
    // Test CashFlow
    CashFlow cash_flow(test_date, 500.0);
    TsRow cash_row = cash_flow;
    EXPECT_EQ(getDate(cash_row), test_date);
    
    // Test BondFlow
    BondFlow bond_flow(test_date, test_balance, 100.0, 50.0);
    TsRow bond_row = bond_flow;
    EXPECT_EQ(getDate(bond_row), test_date);
    
    // Test MortgageFlow
    MortgageFlow mortgage_flow(test_date, test_balance, 80.0, 30.0, 20.0, 0.0, 0.0, 0.0, test_rate);
    TsRow mortgage_row = mortgage_flow;
    EXPECT_EQ(getDate(mortgage_row), test_date);
}

// Test BeginStatus
TEST_F(CashflowTest, BeginStatus) {
    BeginStatus status(test_balance, test_date);
    EXPECT_DOUBLE_EQ(status.begin_balance, test_balance);
    EXPECT_EQ(status.begin_date, test_date);
    EXPECT_FALSE(status.accrued_interest.has_value());
    
    // Test with accrued interest
    BeginStatus status_with_interest(test_balance, test_date, 25.0);
    EXPECT_TRUE(status_with_interest.accrued_interest.has_value());
    EXPECT_DOUBLE_EQ(*status_with_interest.accrued_interest, 25.0);
}

// Test CashFlowFrame basic operations
TEST_F(CashflowTest, CashFlowFrameBasics) {
    // Test empty cash flow frame
    CashFlowFrame empty_cf = CashFlowFrame::createEmpty();
    EXPECT_TRUE(empty_cf.empty());
    EXPECT_EQ(empty_cf.size(), 0);
    
    // Test cash flow frame with data
    BeginStatus status(test_balance, test_date);
    std::vector<TsRow> rows;
    rows.emplace_back(CashFlow(test_date, 100.0));
    rows.emplace_back(BondFlow(Date(16, QuantLib::Month::January, 2024), 900.0, 100.0, 25.0));
    
    CashFlowFrame cf(status, std::move(rows));
    EXPECT_FALSE(cf.empty());
    EXPECT_EQ(cf.size(), 2);
    
    // Test getDates
    std::vector<Date> dates = cf.getDates();
    EXPECT_EQ(dates.size(), 2);
    EXPECT_EQ(dates[0], test_date);
    EXPECT_EQ(dates[1], Date(16, QuantLib::Month::January, 2024));
}

// Test CashFlowFrame combine operation
TEST_F(CashflowTest, CashFlowFrameCombine) {
    BeginStatus status1(1000.0, test_date);
    std::vector<TsRow> rows1;
    rows1.emplace_back(CashFlow(test_date, 100.0));
    CashFlowFrame cf1(status1, std::move(rows1));
    
    BeginStatus status2(2000.0, Date(20, QuantLib::Month::January, 2024));
    std::vector<TsRow> rows2;
    rows2.emplace_back(CashFlow(Date(16, QuantLib::Month::January, 2024), 200.0));
    CashFlowFrame cf2(status2, std::move(rows2));
    
    // Combine the cash flow frames
    CashFlowFrame combined = cf1.combine(cf2);
    EXPECT_EQ(combined.size(), 2);
    
    // Check that dates are sorted
    std::vector<Date> dates = combined.getDates();
    EXPECT_EQ(dates[0], test_date); // Earlier date first
    EXPECT_EQ(dates[1], Date(16, QuantLib::Month::January, 2024)); // Later date second
}

// Test statistics utility functions
TEST_F(CashflowTest, StatisticsUtilities) {
    CumulativeStat stat1(100.0, 50.0, 25.0, 10.0, 5.0, 2.0);
    CumulativeStat stat2(200.0, 75.0, 30.0, 15.0, 8.0, 3.0);
    
    std::optional<CumulativeStat> opt_stat1 = stat1;
    std::optional<CumulativeStat> opt_stat2 = stat2;
    std::optional<CumulativeStat> empty_stat = std::nullopt;
    
    // Test sumStats
    auto sum_result = sumStats(opt_stat1, opt_stat2);
    EXPECT_TRUE(sum_result.has_value());
    EXPECT_DOUBLE_EQ(sum_result->cum_principal, 300.0);
    
    auto sum_with_empty = sumStats(opt_stat1, empty_stat);
    EXPECT_TRUE(sum_with_empty.has_value());
    EXPECT_DOUBLE_EQ(sum_with_empty->cum_principal, 100.0);
    
    auto sum_both_empty = sumStats(empty_stat, empty_stat);
    EXPECT_FALSE(sum_both_empty.has_value());
    
    // Test maxStats
    auto max_result = maxStats(opt_stat1, opt_stat2);
    EXPECT_TRUE(max_result.has_value());
    EXPECT_DOUBLE_EQ(max_result->cum_principal, 200.0); // max of 100 and 200
    EXPECT_DOUBLE_EQ(max_result->cum_prepay, 75.0);     // max of 50 and 75
}