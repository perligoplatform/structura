/**
 * @file new_asset_test.cpp
 * @brief Basic compilation tests for newly implemented asset types
 */

#include <gtest/gtest.h>
#include "../src/assets/fixed_asset.h"
#include "../src/assets/receivable.h"
#include "../src/assets/projected_cash_flow.h"

using namespace Structura;

TEST(NewAssetCompilationTest, BasicIncludeTest) {
    // Test that headers compile without errors
    EXPECT_TRUE(true);
}

TEST(NewAssetCompilationTest, EnumConversions) {
    // Test FixedAssetType conversion
    std::string solar_str = fixedAssetTypeToString(FixedAssetType::SOLAR_PANELS);
    EXPECT_FALSE(solar_str.empty());

    // Test ReceivableType conversion  
    std::string trade_str = receivableTypeToString(ReceivableType::TRADE_RECEIVABLE);
    EXPECT_FALSE(trade_str.empty());

    // Test ProjectedCashFlowType conversion
    std::string royalty_str = projectedCashFlowTypeToString(ProjectedCashFlowType::ROYALTY_STREAM);
    EXPECT_FALSE(royalty_str.empty());
}

TEST(NewAssetCompilationTest, FrequencyConversions) {
    // Test PaymentFrequency conversion functions
    std::string monthly_str = paymentFrequencyToString(CashFlowFrequency::MONTHLY);
    EXPECT_FALSE(monthly_str.empty());
    
    int payments_per_year = paymentFrequencyToPaymentsPerYear(CashFlowFrequency::QUARTERLY);
    EXPECT_EQ(payments_per_year, 4);
    
    CashFlowFrequency freq = paymentsPerYearToFrequency(12);
    EXPECT_EQ(freq, CashFlowFrequency::MONTHLY);
    
    int days = paymentFrequencyToDays(CashFlowFrequency::ANNUAL);  
    EXPECT_EQ(days, 365);
}