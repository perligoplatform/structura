#include <gtest/gtest.h>
#include "../src/core/interest_rate.h"
#include <cmath>

using namespace Structura;

class InterestRateTest : public ::testing::Test {
protected:
    void SetUp() override {
        // Set up test data
        test_date_1 = Date(15, QuantLib::Month::January, 2024);
        test_date_2 = Date(15, QuantLib::Month::July, 2024);  // 6 months later
        test_date_3 = Date(15, QuantLib::Month::January, 2025); // 1 year later
        test_rate = 0.05; // 5%
        test_balance = 1000.0;
        test_spread = 0.02; // 200 basis points
    }

    Date test_date_1, test_date_2, test_date_3;
    Rate test_rate;
    Balance test_balance;
    Spread test_spread;
};

// Test Index enum and string conversions
TEST_F(InterestRateTest, IndexConversions) {
    // Test index to string
    EXPECT_EQ(indexToString(Index::LIBOR_USD_3M), "LIBOR_USD_3M");
    EXPECT_EQ(indexToString(Index::SOFR), "SOFR");
    EXPECT_EQ(indexToString(Index::EURIBOR_6M), "EURIBOR_6M");
    
    // Test string to index
    EXPECT_EQ(stringToIndex("LIBOR_USD_3M"), Index::LIBOR_USD_3M);
    EXPECT_EQ(stringToIndex("SOFR"), Index::SOFR);
    EXPECT_EQ(stringToIndex("EURIBOR_6M"), Index::EURIBOR_6M);
    
    // Test invalid string
    EXPECT_THROW(stringToIndex("INVALID_INDEX"), std::invalid_argument);
}

// Test DayCount enum and string conversions
TEST_F(InterestRateTest, DayCountConversions) {
    // Test day count to string
    EXPECT_EQ(dayCountToString(DayCount::DC_ACT_360), "ACT/360");
    EXPECT_EQ(dayCountToString(DayCount::DC_30_360_US), "30/360_US");
    EXPECT_EQ(dayCountToString(DayCount::DC_ACT_365A), "ACT/365A");
    
    // Test string to day count
    EXPECT_EQ(stringToDayCount("ACT/360"), DayCount::DC_ACT_360);
    EXPECT_EQ(stringToDayCount("30/360_US"), DayCount::DC_30_360_US);
    EXPECT_EQ(stringToDayCount("ACT/365A"), DayCount::DC_ACT_365A);
    
    // Test invalid string
    EXPECT_THROW(stringToDayCount("INVALID_DC"), std::invalid_argument);
}

// Test DatePattern conversions
TEST_F(InterestRateTest, DatePatternConversions) {
    EXPECT_EQ(datePatternToString(DatePattern::QUARTERLY), "QUARTERLY");
    EXPECT_EQ(datePatternToString(DatePattern::MONTHLY), "MONTHLY");
    
    EXPECT_EQ(stringToDatePattern("QUARTERLY"), DatePattern::QUARTERLY);
    EXPECT_EQ(stringToDatePattern("MONTHLY"), DatePattern::MONTHLY);
    
    EXPECT_THROW(stringToDatePattern("INVALID_PATTERN"), std::invalid_argument);
}

// Test RoundingBy template
TEST_F(InterestRateTest, RoundingOperations) {
    RoundingBy<double> ceil_rounder(RoundingBy<double>::Method::ROUND_CEIL, 0.01);
    RoundingBy<double> floor_rounder(RoundingBy<double>::Method::ROUND_FLOOR, 0.01);
    RoundingBy<double> nearest_rounder(RoundingBy<double>::Method::ROUND_NEAREST, 0.01);
    
    double test_value = 0.0567;
    
    EXPECT_NEAR(ceil_rounder.apply(test_value), 0.06, 1e-10);
    EXPECT_NEAR(floor_rounder.apply(test_value), 0.05, 1e-10);
    EXPECT_NEAR(nearest_rounder.apply(test_value), 0.06, 1e-10);
}

// Test RateType - Fixed Rate
TEST_F(InterestRateTest, FixedRateType) {
    RateType fixed_rate = RateType::createFixed(DayCount::DC_ACT_360, test_rate);
    
    EXPECT_TRUE(fixed_rate.isFixed());
    EXPECT_FALSE(fixed_rate.isFloating());
    EXPECT_EQ(fixed_rate.getDayCount(), DayCount::DC_ACT_360);
    EXPECT_FALSE(fixed_rate.getSpread().has_value());
    
    const auto& fixed = fixed_rate.getFixed();
    EXPECT_EQ(fixed.day_count, DayCount::DC_ACT_360);
    EXPECT_DOUBLE_EQ(fixed.rate, test_rate);
}

// Test RateType - Floating Rate
TEST_F(InterestRateTest, FloatingRateType) {
    RateType floating_rate = RateType::createFloating(
        DayCount::DC_ACT_360, Index::LIBOR_USD_3M, test_spread, 
        test_rate, DatePattern::QUARTERLY, 0.01, 0.10);
    
    EXPECT_FALSE(floating_rate.isFixed());
    EXPECT_TRUE(floating_rate.isFloating());
    EXPECT_EQ(floating_rate.getDayCount(), DayCount::DC_ACT_360);
    EXPECT_TRUE(floating_rate.getSpread().has_value());
    EXPECT_DOUBLE_EQ(*floating_rate.getSpread(), test_spread);
    
    const auto& floating = floating_rate.getFloating();
    EXPECT_EQ(floating.day_count, DayCount::DC_ACT_360);
    EXPECT_EQ(floating.index, Index::LIBOR_USD_3M);
    EXPECT_DOUBLE_EQ(floating.spread, test_spread);
    EXPECT_DOUBLE_EQ(floating.initial_rate, test_rate);
    EXPECT_EQ(floating.reset_pattern, DatePattern::QUARTERLY);
    EXPECT_TRUE(floating.floor.has_value());
    EXPECT_DOUBLE_EQ(*floating.floor, 0.01);
    EXPECT_TRUE(floating.cap.has_value());
    EXPECT_DOUBLE_EQ(*floating.cap, 0.10);
}

// Test ARM - Standard ARM
TEST_F(InterestRateTest, StandardARM) {
    ARM standard_arm(36, 0.02, 0.02, 0.06, 0.01); // 36 month init, 2% init cap, etc.
    
    EXPECT_TRUE(standard_arm.isStandardARM());
    EXPECT_EQ(standard_arm.getType(), ARM::Type::STANDARD_ARM);
    EXPECT_EQ(standard_arm.getInitPeriod(), 36);
    EXPECT_TRUE(standard_arm.getInitCap().has_value());
    EXPECT_DOUBLE_EQ(*standard_arm.getInitCap(), 0.02);
    EXPECT_TRUE(standard_arm.getPeriodicCap().has_value());
    EXPECT_DOUBLE_EQ(*standard_arm.getPeriodicCap(), 0.02);
    EXPECT_TRUE(standard_arm.getLifetimeCap().has_value());
    EXPECT_DOUBLE_EQ(*standard_arm.getLifetimeCap(), 0.06);
    EXPECT_TRUE(standard_arm.getFloor().has_value());
    EXPECT_DOUBLE_EQ(*standard_arm.getFloor(), 0.01);
}

// Test ARM - Other ARM
TEST_F(InterestRateTest, OtherARM) {
    ARM other_arm; // Default constructor
    
    EXPECT_FALSE(other_arm.isStandardARM());
    EXPECT_EQ(other_arm.getType(), ARM::Type::OTHER_ARM);
    EXPECT_EQ(other_arm.getInitPeriod(), 0);
}

// Test Year Count Fraction calculations
TEST_F(InterestRateTest, YearCountFractions) {
    using namespace InterestCalculations;
    
    // Test ACT/360 - 6 months (approximately 181 days)
    double yf_act360 = yearCountFraction(DayCount::DC_ACT_360, test_date_1, test_date_2);
    EXPECT_NEAR(yf_act360, 182.0 / 360.0, 0.01); // Allow some tolerance for exact day count
    
    // Test ACT/365 - 6 months
    double yf_act365 = yearCountFraction(DayCount::DC_ACT_365, test_date_1, test_date_2);
    EXPECT_NEAR(yf_act365, 182.0 / 365.0, 0.01);
    
    // Test 30/360 - 6 months should be exactly 180/360 = 0.5
    double yf_30360 = yearCountFraction(DayCount::DC_30_360_US, test_date_1, test_date_2);
    EXPECT_NEAR(yf_30360, 0.5, 0.01);
    
    // Test full year
    double yf_year = yearCountFraction(DayCount::DC_ACT_365, test_date_1, test_date_3);
    EXPECT_NEAR(yf_year, 1.0, 0.01);
}

// Test Interest Rate Calculations
TEST_F(InterestRateTest, InterestRateCalculations) {
    using namespace InterestCalculations;
    
    // Test calcIntRate - should be annual rate * year fraction
    Rate period_rate = calcIntRate(test_date_1, test_date_2, test_rate, DayCount::DC_ACT_365);
    double expected_yf = yearCountFraction(DayCount::DC_ACT_365, test_date_1, test_date_2);
    EXPECT_NEAR(period_rate, test_rate * expected_yf, 1e-10);
    
    // Test calcInt - interest amount calculation
    Balance interest_amount = calcInt(test_balance, test_date_1, test_date_2, test_rate, DayCount::DC_ACT_365);
    Balance expected_interest = test_balance * (test_rate * expected_yf);
    EXPECT_NEAR(interest_amount, expected_interest, 1e-8);
}

// Test Multiple Period Rate Calculations
TEST_F(InterestRateTest, MultiPeriodRateCalculations) {
    using namespace InterestCalculations;
    
    std::vector<Date> dates = {test_date_1, test_date_2, test_date_3};
    std::vector<Rate> rates = calcIntRates(DayCount::DC_ACT_365, test_rate, dates);
    
    EXPECT_EQ(rates.size(), 2); // Should have rates for 2 periods
    
    // Each rate should be positive and reasonable
    for (Rate rate : rates) {
        EXPECT_GT(rate, 0.0);
        EXPECT_LT(rate, test_rate); // Should be less than annual rate (since periods < 1 year)
    }
}

// Test Rate Reset Date Generation
TEST_F(InterestRateTest, RateResetDates) {
    using namespace InterestCalculations;
    
    // Test fixed rate - should return empty vector
    RateType fixed_rate = RateType::createFixed(DayCount::DC_ACT_360, test_rate);
    std::vector<Date> fixed_resets = getRateResetDates(test_date_1, test_date_3, fixed_rate);
    EXPECT_TRUE(fixed_resets.empty());
    
    // Test floating rate - quarterly resets
    RateType floating_rate = RateType::createFloating(
        DayCount::DC_ACT_360, Index::LIBOR_USD_3M, test_spread,
        test_rate, DatePattern::QUARTERLY);
    std::vector<Date> floating_resets = getRateResetDates(test_date_1, test_date_3, floating_rate);
    
    // Should have multiple reset dates for quarterly over 1 year
    EXPECT_GT(floating_resets.size(), 0);
    EXPECT_LE(floating_resets.size(), 5); // At most 5 quarters in a year
    
    // First reset should be start date
    EXPECT_EQ(floating_resets[0], test_date_1);
    
    // Test monthly resets
    RateType monthly_floating = RateType::createFloating(
        DayCount::DC_ACT_360, Index::LIBOR_USD_1M, test_spread,
        test_rate, DatePattern::MONTHLY);
    std::vector<Date> monthly_resets = getRateResetDates(test_date_1, test_date_2, monthly_floating);
    
    // Should have more resets for monthly vs quarterly
    EXPECT_GT(monthly_resets.size(), floating_resets.size());
}

// Test edge cases
TEST_F(InterestRateTest, EdgeCases) {
    using namespace InterestCalculations;
    
    // Test same start and end date
    double zero_yf = yearCountFraction(DayCount::DC_ACT_365, test_date_1, test_date_1);
    EXPECT_DOUBLE_EQ(zero_yf, 0.0);
    
    // Test zero rate
    Balance zero_interest = calcInt(test_balance, test_date_1, test_date_2, 0.0, DayCount::DC_ACT_365);
    EXPECT_DOUBLE_EQ(zero_interest, 0.0);
    
    // Test zero balance
    Balance zero_balance_interest = calcInt(0.0, test_date_1, test_date_2, test_rate, DayCount::DC_ACT_365);
    EXPECT_DOUBLE_EQ(zero_balance_interest, 0.0);
    
    // Test empty date vector for calcIntRates
    std::vector<Date> empty_dates;
    std::vector<Rate> empty_rates = calcIntRates(DayCount::DC_ACT_365, test_rate, empty_dates);
    EXPECT_TRUE(empty_rates.empty());
    
    // Test single date in vector
    std::vector<Date> single_date = {test_date_1};
    std::vector<Rate> single_rates = calcIntRates(DayCount::DC_ACT_365, test_rate, single_date);
    EXPECT_TRUE(single_rates.empty());
}