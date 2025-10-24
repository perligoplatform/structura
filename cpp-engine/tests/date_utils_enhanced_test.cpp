#include <gtest/gtest.h>
#include "../src/core/date_utils_enhanced.h"
#include <ql/time/date.hpp>
#include <ql/time/period.hpp>
#include <ql/time/calendars/nullcalendar.hpp>
#include <algorithm>

using namespace Structura::DateUtils;
using namespace QuantLib;

class DateUtilsEnhancedTest : public ::testing::Test {
protected:
    void SetUp() override {
        base_date = Date(15, January, 2024);
        end_date = Date(15, December, 2024);
    }
    
    Date base_date;
    Date end_date;
};

// Test serial date generation with different patterns
TEST_F(DateUtilsEnhancedTest, GenerateSerialDatesMonthly) {
    auto dates = generateSerialDates(DatePattern::MONTHLY, CutoffType::INCLUDE, base_date, 5);
    
    EXPECT_EQ(dates.size(), 5);
    EXPECT_EQ(dates[0], base_date);
    EXPECT_EQ(dates[1], Date(15, February, 2024));
    EXPECT_EQ(dates[2], Date(15, March, 2024));
    EXPECT_EQ(dates[3], Date(15, April, 2024));
    EXPECT_EQ(dates[4], Date(15, May, 2024));
}

TEST_F(DateUtilsEnhancedTest, GenerateSerialDatesQuarterly) {
    auto dates = generateSerialDates(DatePattern::QUARTERLY, CutoffType::INCLUDE, base_date, 4);
    
    EXPECT_EQ(dates.size(), 4);
    EXPECT_EQ(dates[0], base_date);
    EXPECT_EQ(dates[1], Date(15, April, 2024));
    EXPECT_EQ(dates[2], Date(15, July, 2024));
    EXPECT_EQ(dates[3], Date(15, October, 2024));
}

TEST_F(DateUtilsEnhancedTest, GenerateSerialDatesSemiAnnually) {
    auto dates = generateSerialDates(DatePattern::SEMI_ANNUALLY, CutoffType::INCLUDE, base_date, 3);
    
    EXPECT_EQ(dates.size(), 3);
    EXPECT_EQ(dates[0], base_date);
    EXPECT_EQ(dates[1], Date(15, July, 2024));
    EXPECT_EQ(dates[2], Date(15, January, 2025));
}

TEST_F(DateUtilsEnhancedTest, GenerateSerialDatesAnnually) {
    auto dates = generateSerialDates(DatePattern::ANNUALLY, CutoffType::INCLUDE, base_date, 3);
    
    EXPECT_EQ(dates.size(), 3);
    EXPECT_EQ(dates[0], base_date);
    EXPECT_EQ(dates[1], Date(15, January, 2025));
    EXPECT_EQ(dates[2], Date(15, January, 2026));
}

TEST_F(DateUtilsEnhancedTest, GenerateSerialDatesWithExclude) {
    auto dates = generateSerialDates(DatePattern::MONTHLY, CutoffType::EXCLUDE, base_date, 5);
    
    EXPECT_EQ(dates.size(), 4); // First date excluded
    EXPECT_EQ(dates[0], Date(15, February, 2024));
    EXPECT_EQ(dates[1], Date(15, March, 2024));
    EXPECT_EQ(dates[2], Date(15, April, 2024));
    EXPECT_EQ(dates[3], Date(15, May, 2024));
}

// Test end-of-period patterns
TEST_F(DateUtilsEnhancedTest, GenerateMonthEndDates) {
    auto dates = generateSerialDates(DatePattern::MONTH_END, CutoffType::INCLUDE, base_date, 4);
    
    EXPECT_EQ(dates.size(), 4);
    EXPECT_EQ(dates[0], Date(31, January, 2024));  // Jan end
    EXPECT_EQ(dates[1], Date(29, February, 2024)); // Feb end (leap year)
    EXPECT_EQ(dates[2], Date(31, March, 2024));    // Mar end
    EXPECT_EQ(dates[3], Date(30, April, 2024));    // Apr end
}

TEST_F(DateUtilsEnhancedTest, GenerateQuarterEndDates) {
    auto dates = generateSerialDates(DatePattern::QUARTER_END, CutoffType::INCLUDE, base_date, 4);
    
    EXPECT_EQ(dates.size(), 4);
    EXPECT_EQ(dates[0], Date(31, March, 2024));     // Q1 end
    EXPECT_EQ(dates[1], Date(30, June, 2024));      // Q2 end
    EXPECT_EQ(dates[2], Date(30, September, 2024)); // Q3 end
    EXPECT_EQ(dates[3], Date(31, December, 2024));  // Q4 end
}

TEST_F(DateUtilsEnhancedTest, GenerateYearEndDates) {
    auto dates = generateSerialDates(DatePattern::YEAR_END, CutoffType::INCLUDE, base_date, 3);
    
    EXPECT_EQ(dates.size(), 3);
    EXPECT_EQ(dates[0], Date(31, December, 2024));
    EXPECT_EQ(dates[1], Date(31, December, 2025));
    EXPECT_EQ(dates[2], Date(31, December, 2026));
}

// Test first-of-period patterns
TEST_F(DateUtilsEnhancedTest, GenerateMonthFirstDates) {
    auto dates = generateSerialDates(DatePattern::MONTH_FIRST, CutoffType::INCLUDE, base_date, 4);
    
    EXPECT_EQ(dates.size(), 4);
    EXPECT_EQ(dates[0], Date(1, January, 2024));   // Jan first
    EXPECT_EQ(dates[1], Date(1, February, 2024));  // Feb first
    EXPECT_EQ(dates[2], Date(1, March, 2024));     // Mar first
    EXPECT_EQ(dates[3], Date(1, April, 2024));     // Apr first
}

TEST_F(DateUtilsEnhancedTest, GenerateQuarterFirstDates) {
    auto dates = generateSerialDates(DatePattern::QUARTER_FIRST, CutoffType::INCLUDE, base_date, 4);
    
    EXPECT_EQ(dates.size(), 4);
    EXPECT_EQ(dates[0], Date(1, January, 2024));   // Q1 first
    EXPECT_EQ(dates[1], Date(1, April, 2024));     // Q2 first
    EXPECT_EQ(dates[2], Date(1, July, 2024));      // Q3 first
    EXPECT_EQ(dates[3], Date(1, October, 2024));   // Q4 first
}

TEST_F(DateUtilsEnhancedTest, GenerateYearFirstDates) {
    auto dates = generateSerialDates(DatePattern::YEAR_FIRST, CutoffType::INCLUDE, base_date, 3);
    
    EXPECT_EQ(dates.size(), 3);
    EXPECT_EQ(dates[0], Date(1, January, 2024));
    EXPECT_EQ(dates[1], Date(1, January, 2025));
    EXPECT_EQ(dates[2], Date(1, January, 2026));
}

// Test date generation until end date
TEST_F(DateUtilsEnhancedTest, GenerateDatesTillEndDate) {
    Date start = Date(1, January, 2024);
    Date end = Date(1, June, 2024);
    
    auto dates = generateDatesTill(start, DatePattern::MONTHLY, end);
    
    EXPECT_GE(dates.size(), 5);
    EXPECT_EQ(dates[0], start);
    EXPECT_LE(dates.back(), end);
    
    // Check monthly progression
    for (size_t i = 1; i < dates.size(); ++i) {
        int months_diff = (dates[i].year() - dates[i-1].year()) * 12 + 
                         (dates[i].month() - dates[i-1].month());
        EXPECT_EQ(months_diff, 1);
    }
}

// Test advanced date generation with range control
TEST_F(DateUtilsEnhancedTest, GenerateDatesTill2WithInclusive) {
    Date start = Date(1, January, 2024);
    Date end = Date(15, March, 2024);
    
    auto dates = generateDatesTill2(RangeType::INCLUSIVE_INCLUSIVE, start, DatePattern::MONTHLY, end);
    
    EXPECT_FALSE(dates.empty());
    EXPECT_GE(dates[0], start);
    EXPECT_LE(dates.back(), end);
}

TEST_F(DateUtilsEnhancedTest, GenerateDatesTill2WithExclusive) {
    Date start = Date(1, January, 2024);
    Date end = Date(1, March, 2024);
    
    auto dates = generateDatesTill2(RangeType::EXCLUSIVE_EXCLUSIVE, start, DatePattern::MONTHLY, end);
    
    for (const auto& date : dates) {
        EXPECT_GT(date, start);
        EXPECT_LT(date, end);
    }
}

// Test date splitting
TEST_F(DateUtilsEnhancedTest, SplitDatesByDate) {
    std::vector<Date> dates = {
        Date(1, January, 2024),
        Date(1, February, 2024),
        Date(1, March, 2024),
        Date(1, April, 2024),
        Date(1, May, 2024)
    };
    Date split_date = Date(1, March, 2024);
    
    auto [before, after] = splitByDate(dates, split_date, CutoffType::INCLUDE);
    
    EXPECT_EQ(before.size(), 3); // Jan, Feb, Mar
    EXPECT_EQ(after.size(), 2);  // Apr, May
    EXPECT_EQ(before.back(), split_date);
}

TEST_F(DateUtilsEnhancedTest, SplitDatesByDateExclude) {
    std::vector<Date> dates = {
        Date(1, January, 2024),
        Date(1, February, 2024),
        Date(1, March, 2024),
        Date(1, April, 2024),
        Date(1, May, 2024)
    };
    Date split_date = Date(1, March, 2024);
    
    auto [before, after] = splitByDate(dates, split_date, CutoffType::EXCLUDE);
    
    EXPECT_EQ(before.size(), 2); // Jan, Feb only
    EXPECT_EQ(after.size(), 2);  // Apr, May (Mar excluded)
    EXPECT_EQ(before.back(), Date(1, February, 2024));
}

// Test date projection
TEST_F(DateUtilsEnhancedTest, ProjectDatesByPattern) {
    auto dates = projectDatesByPattern(base_date, DatePattern::QUARTERLY, 3);
    
    EXPECT_EQ(dates.size(), 3);
    EXPECT_EQ(dates[0], base_date);
    EXPECT_EQ(dates[1], Date(15, April, 2024));
    EXPECT_EQ(dates[2], Date(15, July, 2024));
}

// Test month arithmetic
TEST_F(DateUtilsEnhancedTest, MonthsAfter) {
    Date result = monthsAfter(base_date, 3);
    EXPECT_EQ(result, Date(15, April, 2024));
    
    Date result_negative = monthsAfter(base_date, -2);
    EXPECT_EQ(result_negative, Date(15, November, 2023));
}

// Test interval factors
TEST_F(DateUtilsEnhancedTest, GetIntervalFactors) {
    std::vector<Date> dates = {
        Date(1, January, 2024),
        Date(1, February, 2024),  // 31 days
        Date(1, March, 2024),     // 29 days (leap year)
        Date(1, April, 2024)      // 31 days
    };
    
    auto factors = getIntervalFactors(dates);
    
    EXPECT_EQ(factors.size(), 3);
    EXPECT_NEAR(factors[0], 31.0/365.0, 0.001);  // Jan to Feb
    EXPECT_NEAR(factors[1], 29.0/365.0, 0.001);  // Feb to Mar
    EXPECT_NEAR(factors[2], 31.0/365.0, 0.001);  // Mar to Apr
}

// Test days between calculation
TEST_F(DateUtilsEnhancedTest, DaysBetween) {
    Date start = Date(1, January, 2024);
    Date end = Date(31, January, 2024);
    
    int days = Structura::DateUtils::daysBetween(start, end);
    EXPECT_EQ(days, 30);
    
    // Test negative case
    int negative_days = Structura::DateUtils::daysBetween(end, start);
    EXPECT_EQ(negative_days, -30);
}

// Test QuantLib day count fractions
TEST_F(DateUtilsEnhancedTest, YearCountFractionACT360) {
    Date start = Date(1, January, 2024);
    Date end = Date(1, July, 2024);  // 182 days in leap year
    
    double fraction = yearCountFraction(Structura::DayCount::DC_ACT_360, start, end);
    EXPECT_NEAR(fraction, 182.0/360.0, 0.001);
}

TEST_F(DateUtilsEnhancedTest, YearCountFractionACT365) {
    Date start = Date(1, January, 2024);
    Date end = Date(1, July, 2024);  // 182 days
    
    double fraction = yearCountFraction(Structura::DayCount::DC_ACT_365, start, end);
    EXPECT_NEAR(fraction, 182.0/365.0, 0.001);
}

TEST_F(DateUtilsEnhancedTest, YearCountFraction30360) {
    Date start = Date(1, January, 2024);
    Date end = Date(1, July, 2024);  // 6 months = 180 days in 30/360
    
    double fraction = yearCountFraction(Structura::DayCount::DC_30_360_US, start, end);
    EXPECT_NEAR(fraction, 180.0/360.0, 0.001);
}

// Test date slicing
TEST_F(DateUtilsEnhancedTest, SliceDatesInclusiveInclusive) {
    std::vector<Date> dates = {
        Date(1, January, 2024),
        Date(1, February, 2024),
        Date(1, March, 2024),
        Date(1, April, 2024),
        Date(1, May, 2024)
    };
    
    Date start = Date(1, February, 2024);
    Date end = Date(1, April, 2024);
    
    auto sliced = sliceDates(dates, start, end, RangeType::INCLUSIVE_INCLUSIVE);
    
    EXPECT_EQ(sliced.size(), 3); // Feb, Mar, Apr
    EXPECT_EQ(sliced[0], start);
    EXPECT_EQ(sliced.back(), end);
}

TEST_F(DateUtilsEnhancedTest, SliceDatesExclusiveExclusive) {
    std::vector<Date> dates = {
        Date(1, January, 2024),
        Date(1, February, 2024),
        Date(1, March, 2024),
        Date(1, April, 2024),
        Date(1, May, 2024)
    };
    
    Date start = Date(1, February, 2024);
    Date end = Date(1, April, 2024);
    
    auto sliced = sliceDates(dates, start, end, RangeType::EXCLUSIVE_EXCLUSIVE);
    
    EXPECT_EQ(sliced.size(), 1); // Only Mar
    EXPECT_EQ(sliced[0], Date(1, March, 2024));
}

// Test string conversions
TEST_F(DateUtilsEnhancedTest, DatePatternStringConversions) {
    EXPECT_EQ(datePatternToString(DatePattern::MONTHLY), "MONTHLY");
    EXPECT_EQ(datePatternToString(DatePattern::QUARTER_END), "QUARTER_END");
    
    EXPECT_EQ(stringToDatePattern("MONTHLY"), DatePattern::MONTHLY);
    EXPECT_EQ(stringToDatePattern("QUARTER_END"), DatePattern::QUARTER_END);
    
    EXPECT_THROW(stringToDatePattern("INVALID"), std::invalid_argument);
}

TEST_F(DateUtilsEnhancedTest, RangeTypeStringConversions) {
    EXPECT_EQ(rangeTypeToString(RangeType::INCLUSIVE_INCLUSIVE), "II");
    EXPECT_EQ(rangeTypeToString(RangeType::EXCLUSIVE_EXCLUSIVE), "EE");
    
    EXPECT_EQ(stringToRangeType("II"), RangeType::INCLUSIVE_INCLUSIVE);
    EXPECT_EQ(stringToRangeType("EE"), RangeType::EXCLUSIVE_EXCLUSIVE);
    
    EXPECT_THROW(stringToRangeType("INVALID"), std::invalid_argument);
}

TEST_F(DateUtilsEnhancedTest, CutoffTypeStringConversions) {
    EXPECT_EQ(cutoffTypeToString(CutoffType::INCLUDE), "INCLUDE");
    EXPECT_EQ(cutoffTypeToString(CutoffType::EXCLUDE), "EXCLUDE");
    
    EXPECT_EQ(stringToCutoffType("INCLUDE"), CutoffType::INCLUDE);
    EXPECT_EQ(stringToCutoffType("EXCLUDE"), CutoffType::EXCLUDE);
    
    EXPECT_THROW(stringToCutoffType("INVALID"), std::invalid_argument);
}

// Test internal helper functions
TEST_F(DateUtilsEnhancedTest, InternalHelperMonthEnds) {
    auto month_ends = Internal::getMonthEnds(2024);
    
    EXPECT_EQ(month_ends.size(), 12);
    EXPECT_EQ(month_ends[0], Date(31, January, 2024));
    EXPECT_EQ(month_ends[1], Date(29, February, 2024)); // Leap year
    EXPECT_EQ(month_ends[2], Date(31, March, 2024));
    EXPECT_EQ(month_ends[11], Date(31, December, 2024));
}

TEST_F(DateUtilsEnhancedTest, InternalHelperQuarterEnds) {
    auto quarter_ends = Internal::getQuarterEnds(2024);
    
    EXPECT_EQ(quarter_ends.size(), 4);
    EXPECT_EQ(quarter_ends[0], Date(31, March, 2024));
    EXPECT_EQ(quarter_ends[1], Date(30, June, 2024));
    EXPECT_EQ(quarter_ends[2], Date(30, September, 2024));
    EXPECT_EQ(quarter_ends[3], Date(31, December, 2024));
}

TEST_F(DateUtilsEnhancedTest, InternalHelperIsEndOfMonth) {
    EXPECT_TRUE(Internal::isEndOfMonth(Date(31, January, 2024)));
    EXPECT_TRUE(Internal::isEndOfMonth(Date(29, February, 2024))); // Leap year
    EXPECT_FALSE(Internal::isEndOfMonth(Date(15, January, 2024)));
    EXPECT_FALSE(Internal::isEndOfMonth(Date(28, February, 2024))); // Not end in leap year
}

TEST_F(DateUtilsEnhancedTest, InternalHelperGetLastDayOfMonth) {
    EXPECT_EQ(Internal::getLastDayOfMonth(2024, 1), Date(31, January, 2024));
    EXPECT_EQ(Internal::getLastDayOfMonth(2024, 2), Date(29, February, 2024)); // Leap year
    EXPECT_EQ(Internal::getLastDayOfMonth(2023, 2), Date(28, February, 2023)); // Non-leap year
    EXPECT_EQ(Internal::getLastDayOfMonth(2024, 4), Date(30, April, 2024));
    EXPECT_EQ(Internal::getLastDayOfMonth(2024, 12), Date(31, December, 2024));
}

// Edge case tests
TEST_F(DateUtilsEnhancedTest, EdgeCaseEmptyDates) {
    std::vector<Date> empty_dates;
    
    auto factors = getIntervalFactors(empty_dates);
    EXPECT_TRUE(factors.empty());
    
    auto [before, after] = splitByDate(empty_dates, base_date, CutoffType::INCLUDE);
    EXPECT_TRUE(before.empty());
    EXPECT_TRUE(after.empty());
}

TEST_F(DateUtilsEnhancedTest, EdgeCaseSingleDate) {
    std::vector<Date> single_date = {base_date};
    
    auto factors = getIntervalFactors(single_date);
    EXPECT_TRUE(factors.empty());
    
    auto sliced = sliceDates(single_date, base_date, base_date, RangeType::INCLUSIVE_INCLUSIVE);
    EXPECT_EQ(sliced.size(), 1);
    EXPECT_EQ(sliced[0], base_date);
}

TEST_F(DateUtilsEnhancedTest, EdgeCaseLeapYearHandling) {
    // Test February 29 in leap year
    Date leap_feb = Date(29, February, 2024);
    EXPECT_TRUE(Internal::isEndOfMonth(leap_feb));
    
    // Test adding months across leap year boundary
    Date feb_start = Date(29, February, 2024);
    Date march_result = monthsAfter(feb_start, 1);
    
    // QuantLib should handle this gracefully
    EXPECT_EQ(march_result.month(), March);
    EXPECT_EQ(march_result.year(), 2024);
}