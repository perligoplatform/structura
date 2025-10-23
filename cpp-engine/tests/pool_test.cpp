#include <gtest/gtest.h>
#include "assets/pool.h"
#include "assets/mortgage.h"
#include "core/types.h"

using namespace Structura;

class PoolTest : public ::testing::Test {
protected:
    Date originDate = DateUtils::makeDate(2024, 1, 15);
    Date currentDate = DateUtils::makeDate(2024, 6, 15);
    
    // Create standard mortgage for pool testing
    Mortgage createStandardMortgage(Balance balance = 100000.0, Rate rate = 0.06, int terms = 360) {
        OriginalInfo info(balance, rate, terms, originDate, Period::Monthly, AmortPlan::Level);
        return Mortgage(info, balance, rate, terms);
    }
    
    // Create a pool of mortgages
    Pool<Mortgage> createMortgagePool(int numMortgages = 3) {
        std::vector<Mortgage> mortgages;
        for (int i = 0; i < numMortgages; ++i) {
            Balance balance = 100000.0 + (i * 50000.0); // Varying balances
            mortgages.push_back(createStandardMortgage(balance));
        }
        return Pool<Mortgage>(mortgages, currentDate);
    }
};

// Test CutoffFields enum conversion
TEST_F(PoolTest, CutoffFieldsStringConversion) {
    EXPECT_EQ(toString(CutoffFields::IssuanceBalance), "IssuanceBalance");
    EXPECT_EQ(toString(CutoffFields::HistoryPrincipal), "HistoryPrincipal");
    EXPECT_EQ(toString(CutoffFields::HistoryDefaults), "HistoryDefaults");
    EXPECT_EQ(toString(CutoffFields::RuntimeCurrentPoolBalance), "RuntimeCurrentPoolBalance");
}

// Test PoolCashflow basic functionality
TEST_F(PoolTest, PoolCashflowBasics) {
    PoolCashflow cashflow;
    
    // Test empty cashflow
    EXPECT_TRUE(cashflow.isEmpty());
    EXPECT_EQ(cashflow.size(), 0);
    EXPECT_EQ(cashflow.getTotalCash(0), 0.0);
    
    // Add some data
    cashflow.dates.push_back(DateUtils::makeDate(2024, 7, 15));
    cashflow.principalPayments.push_back(1000.0);
    cashflow.interestPayments.push_back(500.0);
    cashflow.prepayments.push_back(200.0);
    
    EXPECT_FALSE(cashflow.isEmpty());
    EXPECT_EQ(cashflow.size(), 1);
    EXPECT_EQ(cashflow.getTotalCash(0), 1700.0); // 1000 + 500 + 200
}

// Test PoolStats functionality
TEST_F(PoolTest, PoolStatsOperations) {
    PoolStats stats;
    
    // Test initial state
    EXPECT_EQ(stats.getField(CutoffFields::IssuanceBalance), 0.0);
    
    // Set fields
    stats.setField(CutoffFields::IssuanceBalance, 1000000.0);
    stats.setField(CutoffFields::RuntimeCurrentPoolBalance, 900000.0);
    
    EXPECT_EQ(stats.getField(CutoffFields::IssuanceBalance), 1000000.0);
    EXPECT_EQ(stats.getField(CutoffFields::RuntimeCurrentPoolBalance), 900000.0);
    
    // Test combining stats
    PoolStats otherStats;
    otherStats.setField(CutoffFields::IssuanceBalance, 500000.0);
    otherStats.setField(CutoffFields::HistoryPrincipal, 100000.0);
    
    stats += otherStats;
    EXPECT_EQ(stats.getField(CutoffFields::IssuanceBalance), 1500000.0);
    EXPECT_EQ(stats.getField(CutoffFields::HistoryPrincipal), 100000.0);
}

// Test Pool construction and basic operations
TEST_F(PoolTest, BasicPoolConstruction) {
    Pool<Mortgage> pool = createMortgagePool(3);
    
    EXPECT_EQ(pool.getAssetCount(), 3);
    EXPECT_EQ(pool.getAsOfDate(), currentDate);
    
    // Test balance calculations
    Balance expectedTotal = 100000.0 + 150000.0 + 200000.0; // 450,000
    EXPECT_EQ(pool.getCurrentBalance(), expectedTotal);
    EXPECT_EQ(pool.getOriginalBalance(), expectedTotal);
    
    // All assets should be performing initially
    EXPECT_EQ(pool.getPerformingAssetCount(), 3);
    EXPECT_EQ(pool.getDefaultedAssetCount(), 0);
}

// Test Pool asset management
TEST_F(PoolTest, AssetManagement) {
    Pool<Mortgage> pool;
    
    EXPECT_EQ(pool.getAssetCount(), 0);
    
    // Add assets
    Mortgage mortgage1 = createStandardMortgage(100000.0);
    Mortgage mortgage2 = createStandardMortgage(200000.0);
    
    pool.addAsset(mortgage1);
    pool.addAsset(std::move(mortgage2));
    
    EXPECT_EQ(pool.getAssetCount(), 2);
    EXPECT_EQ(pool.getCurrentBalance(), 300000.0);
}

// Test Pool statistics calculation
TEST_F(PoolTest, PoolStatisticsCalculation) {
    Pool<Mortgage> pool = createMortgagePool(2);
    
    PoolStats stats = pool.calculateCurrentStats();
    
    EXPECT_EQ(stats.getField(CutoffFields::IssuanceBalance), 250000.0); // 100k + 150k
    EXPECT_EQ(stats.getField(CutoffFields::RuntimeCurrentPoolBalance), 250000.0);
    EXPECT_EQ(stats.getField(CutoffFields::HistoryDefaults), 0.0);
}

// Test Pool cashflow aggregation
TEST_F(PoolTest, CashflowAggregation) {
    Pool<Mortgage> pool = createMortgagePool(2);
    
    PoolCashflow aggregated = pool.aggregateAssetCashflows(12); // 12 months
    
    EXPECT_EQ(aggregated.size(), 12);
    EXPECT_FALSE(aggregated.isEmpty());
    
    // Check that we have cashflows
    EXPECT_GT(aggregated.interestPayments[0], 0.0);
    EXPECT_GT(aggregated.principalPayments[0], 0.0);
    
    // First month should have positive cash flows
    EXPECT_GT(aggregated.getTotalCash(0), 0.0);
}

// Test Balance Factor Pricing
TEST_F(PoolTest, BalanceFactorPricing) {
    BalanceFactorPricing pricing(1.0, 0.5); // 100% current, 50% default
    
    EXPECT_EQ(pricing.getMethodName(), "BalanceFactor");
    
    // Create simple cashflow for testing
    PoolCashflow cashflow;
    cashflow.dates.push_back(DateUtils::makeDate(2024, 7, 15));
    cashflow.remainingBalances.push_back(100000.0);
    cashflow.defaults.push_back(10000.0);
    cashflow.recoveries.push_back(2000.0);
    
    Balance value = pricing.calculateValue(cashflow, DateUtils::makeDate(2024, 7, 15));
    
    // Expected: 100,000 * 1.0 + (10,000 - 2,000) * 0.5 = 104,000
    EXPECT_EQ(value, 104000.0);
}

// Test Present Value Pricing
TEST_F(PoolTest, PresentValuePricing) {
    PresentValuePricing pricing(0.05, 0.3); // 5% discount, 30% recovery
    
    EXPECT_EQ(pricing.getMethodName(), "PresentValue");
    
    // Create cashflow with future payments
    PoolCashflow cashflow;
    Date futureDate = DateUtils::makeDate(2025, 1, 15); // 6 months future
    cashflow.dates.push_back(futureDate);
    cashflow.principalPayments.push_back(5000.0);
    cashflow.interestPayments.push_back(3000.0);
    cashflow.defaults.push_back(10000.0);
    
    Balance value = pricing.calculateValue(cashflow, DateUtils::makeDate(2024, 7, 15));
    
    // Should be positive (PV of future cashflow + recovery)
    EXPECT_GT(value, 0.0);
    EXPECT_LT(value, 8000.0 + 3000.0); // Less than undiscounted amount
}

// Test Pool pricing
TEST_F(PoolTest, PoolPricing) {
    Pool<Mortgage> pool = createMortgagePool(2);
    
    BalanceFactorPricing pricing(0.9, 0.4);
    Balance price = pool.pricePool(pricing, currentDate);
    
    // Should be close to 90% of current balance (no defaults)
    Balance expectedPrice = pool.getCurrentBalance() * 0.9;
    EXPECT_NEAR(price, expectedPrice, 1000.0); // Within $1000
}

// Test Pool liquidation amount calculation
TEST_F(PoolTest, LiquidationAmountCalculation) {
    Pool<Mortgage> pool = createMortgagePool(2);
    
    // Set some future cashflow
    PoolCashflow futureCf;
    futureCf.dates.push_back(DateUtils::makeDate(2024, 8, 15));
    futureCf.remainingBalances.push_back(240000.0);
    futureCf.defaults.push_back(5000.0);
    pool.setFutureCashflow(futureCf);
    
    BalanceFactorPricing pricing(0.95, 0.5);
    Balance liquidationAmount = pool.calculateLiquidationAmount(pricing, DateUtils::makeDate(2024, 8, 15));
    
    // Expected: 240,000 * 0.95 + 5,000 * 0.5 = 230,500
    EXPECT_EQ(liquidationAmount, 230500.0);
}

// Test Pool projection
TEST_F(PoolTest, PoolProjection) {
    Pool<Mortgage> pool = createMortgagePool(2);
    
    std::vector<PoolCashflow> projections = pool.runProjection(6);
    
    EXPECT_EQ(projections.size(), 1); // Simple implementation returns 1 scenario
    EXPECT_EQ(projections[0].size(), 6);
    EXPECT_GT(projections[0].getTotalCash(0), 0.0);
}

// Test Pool with issuance statistics
TEST_F(PoolTest, IssuanceStatistics) {
    Pool<Mortgage> pool = createMortgagePool(2);
    
    PoolStats issuanceStats;
    issuanceStats.setField(CutoffFields::IssuanceBalance, 300000.0);
    issuanceStats.setField(CutoffFields::AccruedInterest, 5000.0);
    
    pool.setIssuanceStats(issuanceStats);
    
    EXPECT_EQ(pool.getIssuanceField(CutoffFields::IssuanceBalance), 300000.0);
    EXPECT_EQ(pool.getIssuanceField(CutoffFields::AccruedInterest), 5000.0);
    
    // Test error case
    Pool<Mortgage> emptyPool;
    EXPECT_THROW(emptyPool.getIssuanceField(CutoffFields::IssuanceBalance), std::runtime_error);
}

// Test PoolUtils functions
TEST_F(PoolTest, PoolUtilsFunctions) {
    // Test cashflow aggregation
    std::vector<PoolCashflow> cashflows(2);
    
    // First cashflow
    cashflows[0].dates.push_back(DateUtils::makeDate(2024, 7, 15));
    cashflows[0].principalPayments.push_back(1000.0);
    cashflows[0].interestPayments.push_back(500.0);
    
    // Second cashflow
    cashflows[1].dates.push_back(DateUtils::makeDate(2024, 7, 15));
    cashflows[1].principalPayments.push_back(2000.0);
    cashflows[1].interestPayments.push_back(800.0);
    
    PoolCashflow aggregated = PoolUtils::aggregateCashflows(cashflows);
    
    EXPECT_EQ(aggregated.size(), 1);
    EXPECT_EQ(aggregated.principalPayments[0], 3000.0); // 1000 + 2000
    EXPECT_EQ(aggregated.interestPayments[0], 1300.0);  // 500 + 800
    
    // Test pool statistics calculation
    std::vector<Mortgage> mortgages;
    mortgages.push_back(createStandardMortgage(100000.0));
    mortgages.push_back(createStandardMortgage(200000.0));
    
    PoolStats stats = PoolUtils::calculatePoolStats(mortgages);
    EXPECT_EQ(stats.getField(CutoffFields::IssuanceBalance), 300000.0);
    EXPECT_EQ(stats.getField(CutoffFields::RuntimeCurrentPoolBalance), 300000.0);
    
    // Test mortgage pool creation
    Pool<Mortgage> mortgagePool = PoolUtils::createMortgagePool(mortgages, currentDate);
    EXPECT_EQ(mortgagePool.getAssetCount(), 2);
    EXPECT_EQ(mortgagePool.getCurrentBalance(), 300000.0);
}

// Test Pool with defaulted assets
TEST_F(PoolTest, PoolWithDefaultedAssets) {
    std::vector<Mortgage> mortgages;
    mortgages.push_back(createStandardMortgage(100000.0));
    mortgages.push_back(createStandardMortgage(200000.0));
    
    // Default the second mortgage
    mortgages[1].applyDefault(currentDate);
    
    Pool<Mortgage> pool(mortgages, currentDate);
    
    EXPECT_EQ(pool.getPerformingAssetCount(), 1);
    EXPECT_EQ(pool.getDefaultedAssetCount(), 1);
    
    // Calculate statistics
    PoolStats stats = pool.calculateCurrentStats();
    EXPECT_EQ(stats.getField(CutoffFields::HistoryDefaults), 200000.0);
}