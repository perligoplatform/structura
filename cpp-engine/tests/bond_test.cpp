#include <gtest/gtest.h>
#include "entities/bond.h"

using namespace Structura;

TEST(BondTest, BasicConstruction) {
    Date maturity = DateUtils::makeDate(2025, 12, 15);
    Bond bond("TEST_BOND", "Test Bond", 1000000.0, 0.05, maturity);
    
    EXPECT_EQ(bond.getId(), "TEST_BOND");
    EXPECT_EQ(bond.getName(), "Test Bond");
    EXPECT_EQ(bond.getOriginalBalance(), 1000000.0);
    EXPECT_EQ(bond.getCurrentBalance(), 1000000.0);
    EXPECT_EQ(bond.getCurrentRate(), 0.05);
}

TEST(BondTest, PrincipalPayment) {
    Date maturity = DateUtils::makeDate(2025, 12, 15);
    Bond bond("TEST_BOND", "Test Bond", 1000000.0, 0.05, maturity);
    
    bond.payPrincipal(250000.0);
    EXPECT_EQ(bond.getCurrentBalance(), 750000.0);
    
    // Can't pay more than outstanding
    bond.payPrincipal(1000000.0);
    EXPECT_EQ(bond.getCurrentBalance(), 0.0);
}

TEST(BondTest, InterestCalculation) {
    Date maturity = DateUtils::makeDate(2025, 12, 15);
    Bond bond("TEST_BOND", "Test Bond", 1000000.0, 0.05, maturity);
    
    Date start = DateUtils::makeDate(2024, 1, 1);
    Date end = DateUtils::makeDate(2024, 2, 1);
    
    Balance interest = bond.calculateInterest(start, end);
    
    // QuantLib gives exact day count - January has 31 days
    Balance expected = 1000000.0 * 0.05 * 31.0 / 365.0;
    EXPECT_NEAR(interest, expected, 1.0); // Within $1
}
