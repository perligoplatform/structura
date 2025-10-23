#include <gtest/gtest.h>
#include "core/financial_types.h"
#include "core/types.h"

using namespace Structura;

class FinancialTypesTest : public ::testing::Test {
protected:
    void SetUp() override {
        // Test setup if needed
    }
};

TEST_F(FinancialTypesTest, BasicTypeAliases) {
    // Test basic type aliases work
    BondName bondName = "TestBond";
    EXPECT_EQ(bondName, "TestBond");
    
    BondNames bondNames = {"Bond1", "Bond2", "Bond3"};
    EXPECT_EQ(bondNames.size(), 3);
    EXPECT_EQ(bondNames[0], "Bond1");
    
    Balance balance = 1000000.0;
    EXPECT_DOUBLE_EQ(balance, 1000000.0);
    
    Rate rate = 0.05; // 5%
    EXPECT_DOUBLE_EQ(rate, 0.05);
}

TEST_F(FinancialTypesTest, PoolIdCreation) {
    // Test PoolName creation
    auto poolName = PoolId::createPoolName("MyPool");
    EXPECT_EQ(poolName.toString(), "MyPool");
    
    // Test PoolConsol creation
    auto poolConsol = PoolId::createPoolConsol();
    EXPECT_EQ(poolConsol.toString(), "PoolConsol");
    
    // Test DealBondFlow creation
    auto date = QuantLib::Date(15, QuantLib::January, 2024);
    auto dealBondFlow = PoolId::createDealBondFlow("TestDeal", "TestBond", date, 0.05);
    std::string expected = "BondFlow:TestDeal:TestBond:January 15th, 2024:0.050000";
    EXPECT_EQ(dealBondFlow.toString(), expected);
}

TEST_F(FinancialTypesTest, PoolIdComparison) {
    auto pool1 = PoolId::createPoolName("Pool1");
    auto pool2 = PoolId::createPoolName("Pool1");
    auto pool3 = PoolId::createPoolName("Pool2");
    
    // Test equality
    EXPECT_TRUE(pool1 == pool2);
    EXPECT_FALSE(pool1 == pool3);
    
    // Test ordering
    EXPECT_TRUE(pool1 < pool3);
    EXPECT_FALSE(pool3 < pool1);
}

TEST_F(FinancialTypesTest, EnumToString) {
    // Test Cmp enum
    EXPECT_EQ(toString(Cmp::G), ">");
    EXPECT_EQ(toString(Cmp::GE), ">=");
    EXPECT_EQ(toString(Cmp::L), "<");
    EXPECT_EQ(toString(Cmp::LE), "<=");
    EXPECT_EQ(toString(Cmp::E), "==");
    
    // Test DayCount enum
    EXPECT_EQ(toString(DayCount::DC_ACT_360), "DC_ACT_360");
    EXPECT_EQ(toString(DayCount::DC_30_360_US), "DC_30_360_US");
    
    // Test Index enum
    EXPECT_EQ(toString(Index::LIBOR3M), "LIBOR3M");
    EXPECT_EQ(toString(Index::USTSY10Y), "USTSY10Y");
}

TEST_F(FinancialTypesTest, FloaterType) {
    // Test Floater as pair of Index and Spread
    Floater floater = std::make_pair(Index::LIBOR3M, 0.0125); // 1.25% spread
    EXPECT_EQ(floater.first, Index::LIBOR3M);
    EXPECT_DOUBLE_EQ(floater.second, 0.0125);
}

TEST_F(FinancialTypesTest, DateIntegration) {
    // Test that our Date types work with QuantLib
    Date date1 = QuantLib::Date(15, QuantLib::June, 2024);
    Date date2 = QuantLib::Date(15, QuantLib::December, 2024);
    
    Dates dateVector = {date1, date2};
    EXPECT_EQ(dateVector.size(), 2);
    EXPECT_TRUE(date1 < date2);
    
    StartDate startDate = date1;
    EndDate endDate = date2;
    EXPECT_TRUE(startDate < endDate);
}