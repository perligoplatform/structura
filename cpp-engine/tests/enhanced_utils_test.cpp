#include <gtest/gtest.h>
#include "../src/core/enhanced_utils.h"
#include <ql/time/date.hpp>
#include <limits>
#include <cmath>

using namespace Structura::Utils;
using namespace Structura;
using namespace QuantLib;

class EnhancedUtilsTest : public ::testing::Test {
protected:
    void SetUp() override {
        base_date = Date(15, January, 2024);
        dates = {
            Date(1, January, 2024),
            Date(1, February, 2024),
            Date(1, March, 2024),
            Date(1, April, 2024)
        };
        values = {100.0, 200.0, 300.0, 400.0};
        balances = {1000.0, 2000.0, 3000.0};
    }
    
    Date base_date;
    std::vector<Date> dates;
    std::vector<Rational> values;
    std::vector<Balance> balances;
};

// ============================================================================
// MATHEMATICAL OPERATIONS TESTS
// ============================================================================

TEST_F(EnhancedUtilsTest, MathematicalOperations) {
    Balance balance = 1000.0;
    Rate rate = 0.05;
    IRate irate = 0.03;
    
    // Test balance-rate multiplication
    EXPECT_DOUBLE_EQ(mulBR(balance, rate), 50.0);
    EXPECT_DOUBLE_EQ(mulBIR(balance, irate), 30.0);
    EXPECT_DOUBLE_EQ(mulBI(balance, irate), 30.0);
    
    // Test integer-rational multiplication
    EXPECT_DOUBLE_EQ(mulIR(5, 0.2), 1.0);
    
    // Test division
    EXPECT_DOUBLE_EQ(divideBI(balance, 4), 250.0);
    EXPECT_DOUBLE_EQ(divideBB(1000.0, 500.0), 2.0);
}

TEST_F(EnhancedUtilsTest, SafeDivision) {
    // Test safe division with different behaviors
    EXPECT_TRUE(std::isinf(safeDivide(1.0, 0.0, DivisionBehavior::RETURN_INFINITY)));
    EXPECT_DOUBLE_EQ(safeDivide(1.0, 0.0, DivisionBehavior::RETURN_ZERO), 0.0);
    EXPECT_DOUBLE_EQ(safeDivide(10.0, 2.0), 5.0);
    
    // Test optional safe division
    auto result1 = safeDiv(10.0, 2.0);
    auto result2 = safeDiv(10.0, 0.0);
    
    EXPECT_TRUE(result1.has_value());
    EXPECT_DOUBLE_EQ(*result1, 5.0);
    EXPECT_FALSE(result2.has_value());
}

TEST_F(EnhancedUtilsTest, SplitBalance) {
    Balance balance = 1000.0;
    Rate rate = 0.3;
    
    auto [first, second] = splitBal(rate, balance);
    EXPECT_DOUBLE_EQ(first, 300.0);
    EXPECT_DOUBLE_EQ(second, 700.0);
    EXPECT_DOUBLE_EQ(first + second, balance);
}

TEST_F(EnhancedUtilsTest, RoundingOperations) {
    // Test rounding by precision
    EXPECT_DOUBLE_EQ(roundingBy(15.7, 5.0, RoundingDirection::FLOOR), 15.0);
    EXPECT_DOUBLE_EQ(roundingBy(15.7, 5.0, RoundingDirection::CEIL), 20.0);
    EXPECT_DOUBLE_EQ(roundingBy(15.7, 5.0, RoundingDirection::ROUND), 15.0);
    
    // Test optional rounding
    auto result1 = roundingByM(std::make_optional(0.25), 15.7, RoundingDirection::ROUND);
    auto result2 = roundingByM(std::nullopt, 15.7, RoundingDirection::ROUND);
    
    EXPECT_DOUBLE_EQ(result1.value(), 15.75);
    EXPECT_DOUBLE_EQ(result2.value(), 15.7);
}

TEST_F(EnhancedUtilsTest, MinMaxOperations) {
    std::vector<double> values = {3.5, 1.2, 4.8, 2.1};
    
    EXPECT_DOUBLE_EQ(maximum(values), 4.8);
    EXPECT_DOUBLE_EQ(minimum(values), 1.2);
    
    // Test with empty vector
    std::vector<double> empty;
    EXPECT_THROW(maximum(empty), std::invalid_argument);
    EXPECT_THROW(minimum(empty), std::invalid_argument);
}

// ============================================================================
// TIME SERIES OPERATIONS TESTS
// ============================================================================

TEST_F(EnhancedUtilsTest, TimeSeriesCreation) {
    // Test zipTs
    auto ts = zipTs(dates, values);
    EXPECT_EQ(getTsSize(ts), 4);
    
    // Test zipBalTs with matching sizes
    std::vector<Date> balDates = {dates[0], dates[1], dates[2]}; // First 3 dates
    auto balTs = zipBalTs(balDates, balances);
    EXPECT_EQ(getTsSize(balTs), 3); // balances has 3 elements
    
    // Test extraction functions
    auto extractedDates = getTsDates(ts);
    auto extractedValues = getTsVals(ts);
    
    EXPECT_EQ(extractedDates.size(), dates.size());
    EXPECT_EQ(extractedValues.size(), values.size());
    
    for (size_t i = 0; i < dates.size(); ++i) {
        EXPECT_EQ(extractedDates[i], dates[i]);
        EXPECT_DOUBLE_EQ(extractedValues[i], values[i]);
    }
}

TEST_F(EnhancedUtilsTest, TimeSeriesValueLookup) {
    auto ts = zipTs(dates, values);
    
    // Test getValByDate with INCLUDE
    Date lookupDate = Date(15, January, 2024); // Between first and second dates
    Rational val = getValByDate(ts, CutoffType::INCLUDE, lookupDate);
    EXPECT_DOUBLE_EQ(val, 100.0); // Should get first value
    
    // Test getValByDates
    std::vector<Date> lookupDates = {dates[0], dates[2]};
    auto valsByDates = getValByDates(ts, CutoffType::INCLUDE, lookupDates);
    EXPECT_EQ(valsByDates.size(), 2);
    EXPECT_DOUBLE_EQ(valsByDates[0], values[0]);
    EXPECT_DOUBLE_EQ(valsByDates[1], values[2]);
}

TEST_F(EnhancedUtilsTest, TimeSeriesOperations) {
    auto ts1 = zipTs(dates, values);
    auto ts2 = zipTs(dates, {2.0, 3.0, 4.0, 5.0});
    
    // Test multiplication
    auto multiplied = multiplyTs(CutoffType::INCLUDE, ts1, ts2);
    auto multipliedValues = getTsVals(multiplied);
    
    EXPECT_EQ(multipliedValues.size(), 4);
    EXPECT_DOUBLE_EQ(multipliedValues[0], 100.0 * 2.0);
    EXPECT_DOUBLE_EQ(multipliedValues[1], 200.0 * 3.0);
    
    // Test shifting
    auto shifted = shiftTsByAmt(ts1, 10.0);
    auto shiftedValues = getTsVals(shifted);
    
    EXPECT_EQ(shiftedValues.size(), 4);
    EXPECT_DOUBLE_EQ(shiftedValues[0], 110.0);
    EXPECT_DOUBLE_EQ(shiftedValues[1], 210.0);
}

// ============================================================================
// COLLECTION UTILITIES TESTS  
// ============================================================================

TEST_F(EnhancedUtilsTest, ListManipulation) {
    std::vector<int> vec = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    
    // Test lastN
    auto last3 = lastN(3, vec);
    EXPECT_EQ(last3.size(), 3);
    EXPECT_EQ(last3[0], 8);
    EXPECT_EQ(last3[2], 10);
    
    // Test dropLastN
    auto dropped = dropLastN(2, vec);
    EXPECT_EQ(dropped.size(), 8);
    EXPECT_EQ(dropped.back(), 8);
    
    // Test replace
    auto replaced = replace(vec, 5, 99);
    EXPECT_EQ(replaced[5], 99);
    EXPECT_EQ(replaced.size(), vec.size());
    
    // Test out of range
    EXPECT_THROW(replace(vec, 15, 99), std::out_of_range);
}

TEST_F(EnhancedUtilsTest, PaddingAndCapping) {
    std::vector<int> vec = {1, 2, 3};
    
    // Test padding
    auto padded = paddingDefault(0, vec, 6);
    EXPECT_EQ(padded.size(), 6);
    EXPECT_EQ(padded[0], 1);
    EXPECT_EQ(padded[5], 0);
    
    // Test truncating when target size is smaller
    auto truncated = paddingDefault(0, vec, 2);
    EXPECT_EQ(truncated.size(), 2);
    EXPECT_EQ(truncated[1], 2);
    
    // Test capping
    std::vector<double> values = {1.5, 3.7, 2.1, 4.9};
    auto capped = capWith(3.0, values);
    EXPECT_EQ(capped.size(), 4);
    EXPECT_DOUBLE_EQ(capped[1], 3.0); // 3.7 capped to 3.0
    EXPECT_DOUBLE_EQ(capped[2], 2.1); // unchanged
    
    // Test flooring
    auto floored = floorWith(2.5, values);
    EXPECT_EQ(floored.size(), 4);
    EXPECT_DOUBLE_EQ(floored[0], 2.5); // 1.5 floored to 2.5
    EXPECT_DOUBLE_EQ(floored[1], 3.7); // unchanged
}

TEST_F(EnhancedUtilsTest, SlicingAndDifferences) {
    std::vector<int> vec = {10, 20, 30, 40, 50};
    
    // Test slicing
    auto sliced = slice(1, 4, vec);
    EXPECT_EQ(sliced.size(), 3);
    EXPECT_EQ(sliced[0], 20);
    EXPECT_EQ(sliced[2], 40);
    
    // Test boundary conditions
    auto emptySlice = slice(3, 2, vec); // from > to
    EXPECT_TRUE(emptySlice.empty());
    
    // Test differences
    auto diffs = diffNum(vec);
    EXPECT_EQ(diffs.size(), 4);
    EXPECT_EQ(diffs[0], 10); // 20 - 10
    EXPECT_EQ(diffs[3], 10); // 50 - 40
}

TEST_F(EnhancedUtilsTest, ScalingOperations) {
    std::vector<double> vec = {2.0, 4.0, 6.0, 8.0};
    
    // Test scaling by first element
    auto scaled = scaleByFstElement(10.0, vec);
    EXPECT_EQ(scaled.size(), 4);
    EXPECT_DOUBLE_EQ(scaled[0], 10.0);
    EXPECT_DOUBLE_EQ(scaled[1], 20.0); // 4.0 * (10.0/2.0)
    EXPECT_DOUBLE_EQ(scaled[3], 40.0); // 8.0 * (10.0/2.0)
    
    // Test scale up to one
    std::vector<Rational> ratios = {0.1, 0.2, 0.3, 0.3}; // sum = 0.9
    auto normalized = scaleUpToOne(ratios);
    Rational sum = std::accumulate(normalized.begin(), normalized.end(), 0.0);
    EXPECT_NEAR(sum, 1.0, 1e-10);
}

TEST_F(EnhancedUtilsTest, FindOperations) {
    std::vector<int> vec = {1, 3, 5, 7, 9, 11};
    
    // Test lastOf with predicate
    auto found = lastOf(vec, [](int x) { return x < 8; });
    EXPECT_TRUE(found.has_value());
    EXPECT_EQ(*found, 7);
    
    auto notFound = lastOf(vec, [](int x) { return x > 20; });
    EXPECT_FALSE(notFound.has_value());
    
    // Test findBox
    std::vector<std::pair<double, double>> ranges = {{1.0, 5.0}, {10.0, 15.0}, {20.0, 25.0}};
    
    auto box1 = findBox({CutoffType::INCLUDE, CutoffType::INCLUDE}, 3.5, ranges);
    EXPECT_TRUE(box1.has_value());
    EXPECT_DOUBLE_EQ(box1->first, 1.0);
    EXPECT_DOUBLE_EQ(box1->second, 5.0);
    
    auto box2 = findBox({CutoffType::EXCLUDE, CutoffType::EXCLUDE}, 5.0, ranges);
    EXPECT_FALSE(box2.has_value()); // 5.0 not > 1.0 and < 5.0
}

// ============================================================================
// FINANCIAL CALCULATIONS TESTS
// ============================================================================

TEST_F(EnhancedUtilsTest, ProRataCalculations) {
    std::vector<Balance> dueAmounts = {100.0, 200.0, 300.0};
    Amount totalToPay = 300.0; // Half of total due (600.0)
    
    auto allocations = prorataFactors(dueAmounts, totalToPay);
    
    EXPECT_EQ(allocations.size(), 3);
    EXPECT_DOUBLE_EQ(allocations[0], 50.0);  // (100/600) * 300
    EXPECT_DOUBLE_EQ(allocations[1], 100.0); // (200/600) * 300
    EXPECT_DOUBLE_EQ(allocations[2], 150.0); // (300/600) * 300
    
    // Test when total to pay exceeds total due
    auto allocations2 = prorataFactors(dueAmounts, 1000.0);
    Amount totalAllocated = std::accumulate(allocations2.begin(), allocations2.end(), 0.0);
    EXPECT_DOUBLE_EQ(totalAllocated, 600.0); // Should not exceed total due
}

TEST_F(EnhancedUtilsTest, PaymentStrategies) {
    // Mock objects for payment testing
    struct TestAccount {
        std::string name;
        Balance due;
        Balance paid = 0.0;
        
        TestAccount(std::string n, Balance d) : name(std::move(n)), due(d) {}
    };
    
    std::vector<TestAccount> accounts = {
        TestAccount("A", 100.0),
        TestAccount("B", 200.0),
        TestAccount("C", 150.0)
    };
    
    auto getDue = [](const TestAccount& acc) { return acc.due - acc.paid; };
    auto makePayment = [](Amount amount, const TestAccount& acc) {
        TestAccount result = acc;
        result.paid += std::min(amount, acc.due - acc.paid);
        return result;
    };
    
    // Test sequential payment
    auto seqResult = paySequentially(base_date, 250.0, getDue, makePayment, accounts);
    EXPECT_EQ(seqResult.paidObjects.size(), 3);
    EXPECT_DOUBLE_EQ(seqResult.paidObjects[0].paid, 100.0); // A fully paid
    EXPECT_DOUBLE_EQ(seqResult.paidObjects[1].paid, 150.0); // B partially paid
    EXPECT_DOUBLE_EQ(seqResult.paidObjects[2].paid, 0.0);   // C not paid
    EXPECT_DOUBLE_EQ(seqResult.remainingAmount, 0.0);
    
    // Test pro-rata payment
    auto proRataResult = payProRata(base_date, 225.0, getDue, makePayment, accounts);
    EXPECT_EQ(proRataResult.paidObjects.size(), 3);
    // Pro-rata should allocate proportionally
    EXPECT_NEAR(proRataResult.paidObjects[0].paid, 50.0, 1e-10);  // (100/450) * 225
    EXPECT_NEAR(proRataResult.paidObjects[1].paid, 100.0, 1e-10); // (200/450) * 225
    EXPECT_NEAR(proRataResult.paidObjects[2].paid, 75.0, 1e-10);  // (150/450) * 225
}

TEST_F(EnhancedUtilsTest, WeightedBalance) {
    // Test weighted balance calculation
    std::vector<Balance> balances = {1000.0, 1500.0, 2000.0};
    std::vector<Date> dates = {
        Date(1, January, 2024),
        Date(1, April, 2024),   // 3 months = ~90 days
        Date(1, July, 2024),    // 3 months = ~91 days  
        Date(1, October, 2024)  // 3 months = ~92 days
    };
    
    // This should work with our DateUtil integration
    Balance weighted = calcWeightBalanceByDates(DayCount::DC_ACT_365, balances, dates);
    EXPECT_GT(weighted, 0.0);
    EXPECT_LT(weighted, 2000.0); // Should be reasonable weighted average
}

TEST_F(EnhancedUtilsTest, RateConversions) {
    Rate annualRate = 0.12; // 12% annual
    
    // Test period rate conversion
    Rate monthlyRate = toPeriodRateByInterval(annualRate, 30);
    Rate dailyRate = toPeriodRateByInterval(annualRate, 1);
    
    EXPECT_GT(monthlyRate, 0.0);
    EXPECT_LT(monthlyRate, annualRate);
    EXPECT_GT(dailyRate, 0.0);
    EXPECT_LT(dailyRate, monthlyRate);
    
    // Test edge cases
    EXPECT_THROW(toPeriodRateByInterval(annualRate, 0), std::invalid_argument);
    EXPECT_THROW(toPeriodRateByInterval(annualRate, -5), std::invalid_argument);
}

// ============================================================================
// MAP OPERATIONS TESTS
// ============================================================================

TEST_F(EnhancedUtilsTest, MapOperations) {
    std::map<std::string, int> testMap = {
        {"A", 10}, {"B", 20}, {"C", 30}
    };
    
    // Test lookupM
    auto result1 = lookupM(std::string("B"), testMap);
    auto result2 = lookupM(std::string("D"), testMap);
    
    EXPECT_TRUE(result1.has_value());
    EXPECT_EQ(*result1, 20);
    EXPECT_FALSE(result2.has_value());
    
    // Test lookupVs
    std::vector<std::string> keys = {"A", "C", "D"};
    auto results = lookupVs(keys, testMap);
    
    EXPECT_EQ(results.size(), 3);
    EXPECT_TRUE(results[0].has_value());
    EXPECT_EQ(*results[0], 10);
    EXPECT_TRUE(results[1].has_value());
    EXPECT_EQ(*results[1], 30);
    EXPECT_FALSE(results[2].has_value());
    
    // Test mapWithinMap
    auto doubled = mapWithinMap([](int x) { return x * 2; }, 
                               std::vector<std::string>{"A", "C"}, 
                               testMap);
    
    EXPECT_EQ(doubled["A"], 20); // 10 * 2
    EXPECT_EQ(doubled["B"], 20); // unchanged
    EXPECT_EQ(doubled["C"], 60); // 30 * 2
}

// ============================================================================
// UTILITY FUNCTIONS TESTS
// ============================================================================

TEST_F(EnhancedUtilsTest, ValidationFunctions) {
    // Test sum validation
    std::vector<Rate> rates1 = {0.3, 0.3, 0.4};
    std::vector<Rate> rates2 = {0.3, 0.3, 0.41};
    
    EXPECT_TRUE(testSumToOne(rates1));
    EXPECT_FALSE(testSumToOne(rates2));
    EXPECT_TRUE(testSumToOne(rates2, 0.02)); // With tolerance
}

TEST_F(EnhancedUtilsTest, SplitByLengths) {
    std::vector<int> values = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    std::vector<int> lengths = {3, 2, 4, 1};
    
    auto split = splitByLengths(values, lengths);
    
    EXPECT_EQ(split.size(), 4);
    EXPECT_EQ(split[0].size(), 3);
    EXPECT_EQ(split[0][0], 1);
    EXPECT_EQ(split[0][2], 3);
    
    EXPECT_EQ(split[1].size(), 2);
    EXPECT_EQ(split[1][0], 4);
    EXPECT_EQ(split[1][1], 5);
    
    EXPECT_EQ(split[2].size(), 4);
    EXPECT_EQ(split[2][0], 6);
    EXPECT_EQ(split[2][3], 9);
    
    EXPECT_EQ(split[3].size(), 1);
    EXPECT_EQ(split[3][0], 10);
}

TEST_F(EnhancedUtilsTest, ListToMapConversion) {
    struct TestObject {
        std::string id;
        int value;
        
        TestObject() = default;  // Default constructor for map operations
        TestObject(std::string i, int v) : id(std::move(i)), value(v) {}
    };
    
    std::vector<TestObject> objects = {
        TestObject("obj1", 100),
        TestObject("obj2", 200),
        TestObject("obj3", 300)
    };
    
    auto keyFn = [](const TestObject& obj) { return obj.id; };
    auto objectMap = lstToMapByFn(keyFn, objects);
    
    EXPECT_EQ(objectMap.size(), 3);
    EXPECT_EQ(objectMap["obj1"].value, 100);
    EXPECT_EQ(objectMap["obj2"].value, 200);
    EXPECT_EQ(objectMap["obj3"].value, 300);
}

TEST_F(EnhancedUtilsTest, DebugUtilities) {
    Date start = Date(1, January, 2024);
    Date end = Date(31, January, 2024);
    Date current = Date(15, January, 2024);
    Date outside = Date(15, February, 2024);
    
    std::string debug1 = debugOnDate(start, end, current);
    std::string debug2 = debugOnDate(start, end, outside);
    
    EXPECT_FALSE(debug1.empty());
    EXPECT_TRUE(debug2.empty());
    
    // Test showLength
    std::vector<int> vec = {1, 2, 3, 4, 5};
    std::string lengthStr = showLength(vec);
    EXPECT_EQ(lengthStr, "Length: 5");
}

// ============================================================================
// EDGE CASES AND ERROR HANDLING
// ============================================================================

TEST_F(EnhancedUtilsTest, EdgeCases) {
    // Test empty vectors
    std::vector<int> empty;
    EXPECT_TRUE(lastN(5, empty).empty());
    EXPECT_TRUE(dropLastN(5, empty).empty());
    EXPECT_TRUE(diffNum(empty).empty());
    
    // Test single element vectors
    std::vector<int> single = {42};
    EXPECT_EQ(lastN(1, single)[0], 42);
    EXPECT_TRUE(dropLastN(1, single).empty());
    EXPECT_TRUE(diffNum(single).empty());
    
    // Test zero values in scaling
    std::vector<double> withZero = {0.0, 1.0, 2.0};
    auto scaled = scaleByFstElement(5.0, withZero);
    EXPECT_EQ(scaled, withZero); // Should remain unchanged due to zero division protection
    
    // Test empty time series
    std::vector<Date> emptyDates;
    std::vector<Rational> emptyValues;
    EXPECT_THROW(zipTs(dates, emptyValues), std::invalid_argument); // Size mismatch
}

TEST_F(EnhancedUtilsTest, NumericalStability) {
    // Test with very small numbers
    std::vector<Rational> small = {1e-15, 2e-15, 3e-15};
    auto scaledUp = scaleUpToOne(small);
    Rational sum = std::accumulate(scaledUp.begin(), scaledUp.end(), 0.0);
    EXPECT_NEAR(sum, 1.0, 1e-12);
    
    // Test pro-rata with very small amounts
    std::vector<Balance> tiny = {1e-10, 2e-10};
    auto allocations = prorataFactors(tiny, 1e-10);
    EXPECT_EQ(allocations.size(), 2);
    EXPECT_GT(allocations[0], 0.0);
    EXPECT_GT(allocations[1], 0.0);
}