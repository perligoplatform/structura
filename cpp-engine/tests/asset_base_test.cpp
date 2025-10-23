#include <gtest/gtest.h>
#include "assets/asset_base.h"
#include "core/types.h"  // For DateUtils

using namespace Structura;

class AssetBaseTest : public ::testing::Test {
protected:
    Date testDate = DateUtils::makeDate(2024, 1, 15);
};

// Test Status enum
TEST_F(AssetBaseTest, StatusStringConversion) {
    EXPECT_EQ(toString(Status::Current), "Current");
    EXPECT_EQ(toString(Status::Defaulted), "Defaulted");
}

// Test AmortPlan enum
TEST_F(AssetBaseTest, AmortPlanStringConversion) {
    EXPECT_EQ(toString(AmortPlan::Level), "Level");
    EXPECT_EQ(toString(AmortPlan::Even), "Even");
    EXPECT_EQ(toString(AmortPlan::I_P), "I_P");
    EXPECT_EQ(toString(AmortPlan::F_P), "F_P");
    EXPECT_EQ(toString(AmortPlan::Balloon), "Balloon");
}

// Test AmortRule enum
TEST_F(AssetBaseTest, AmortRuleStringConversion) {
    EXPECT_EQ(toString(AmortRule::DecliningBalance), "DecliningBalance");
    EXPECT_EQ(toString(AmortRule::StraightLine), "StraightLine");
}

// Test Obligor class
TEST_F(AssetBaseTest, ObligorCreation) {
    Obligor obligor("BORROWER_001");
    
    EXPECT_EQ(obligor.getId(), "BORROWER_001");
    EXPECT_TRUE(obligor.getTags().empty());
    
    obligor.addTag("PRIME");
    obligor.addTag("RESIDENTIAL");
    EXPECT_EQ(obligor.getTags().size(), 2);
    EXPECT_EQ(obligor.getTags()[0], "PRIME");
    EXPECT_EQ(obligor.getTags()[1], "RESIDENTIAL");
}

TEST_F(AssetBaseTest, ObligorFields) {
    Obligor obligor("BORROWER_002");
    
    obligor.setField("credit_score", 750.0);
    obligor.setField("employment_status", std::string("EMPLOYED"));
    obligor.setField("income", 85000.0);
    
    auto creditScore = obligor.getDoubleField("credit_score");
    ASSERT_TRUE(creditScore.has_value());
    EXPECT_EQ(creditScore.value(), 750.0);
    
    auto employment = obligor.getStringField("employment_status");
    ASSERT_TRUE(employment.has_value());
    EXPECT_EQ(employment.value(), "EMPLOYED");
    
    auto income = obligor.getDoubleField("income");
    ASSERT_TRUE(income.has_value());
    EXPECT_EQ(income.value(), 85000.0);
    
    // Test non-existent field
    auto nonExistent = obligor.getStringField("non_existent");
    EXPECT_FALSE(nonExistent.has_value());
}

// Test OriginalInfo class
TEST_F(AssetBaseTest, OriginalInfoCreation) {
    OriginalInfo info(500000.0, 0.045, 360, testDate, Period::Monthly, AmortPlan::Level);
    
    EXPECT_EQ(info.getOriginBalance(), 500000.0);
    EXPECT_EQ(info.getOriginRate(), 0.045);
    EXPECT_EQ(info.getOriginTerm(), 360);
    EXPECT_EQ(info.getStartDate(), testDate);
    EXPECT_EQ(info.getPeriod(), Period::Monthly);
    EXPECT_EQ(info.getPrinType(), AmortPlan::Level);
    EXPECT_FALSE(info.hasObligor());
}

TEST_F(AssetBaseTest, OriginalInfoWithObligor) {
    OriginalInfo info(300000.0, 0.035, 240, testDate, Period::Monthly, AmortPlan::Even);
    
    Obligor obligor("BORROWER_003");
    obligor.setField("fico_score", 720.0);
    info.setObligor(obligor);
    
    EXPECT_TRUE(info.hasObligor());
    ASSERT_TRUE(info.getObligor().has_value());
    EXPECT_EQ(info.getObligor()->getId(), "BORROWER_003");
    
    auto fico = info.getObligor()->getDoubleField("fico_score");
    ASSERT_TRUE(fico.has_value());
    EXPECT_EQ(fico.value(), 720.0);
}

// Test payment calculations
TEST_F(AssetBaseTest, PaymentCalculationZeroRate) {
    Balance balance = 100000.0;
    Rate rate = 0.0;
    int periods = 100;
    
    Amount payment = calculatePayment(balance, rate, periods);
    EXPECT_EQ(payment, 1000.0); // 100000 / 100
}

TEST_F(AssetBaseTest, PaymentCalculationNonZeroRate) {
    Balance balance = 100000.0;
    Rate rate = 0.05 / 12; // 5% annual, monthly
    int periods = 12;
    
    Amount payment = calculatePayment(balance, rate, periods);
    
    // Expected payment for level annuity
    double expectedPayment = 8560.75; // Approximate
    EXPECT_NEAR(payment, expectedPayment, 1.0);
}

// Test principal/interest calculations
TEST_F(AssetBaseTest, LevelAmortizationCalculation) {
    Balance balance = 100000.0;
    Rate rate = 0.06 / 12; // 6% annual, monthly  
    int originalTerm = 360;
    int remainingTerm = 360;
    std::pair<Balance, int> amortInfo = {balance, originalTerm};
    
    auto [interest, principal] = calculatePrincipalInterest(
        AmortPlan::Level, balance, rate, originalTerm, remainingTerm, amortInfo);
    
    // First payment should be mostly interest
    EXPECT_GT(interest, principal);
    EXPECT_EQ(interest, balance * rate); // Interest = balance * rate
    
    // Total payment should equal interest + principal
    Amount totalPayment = calculatePayment(balance, rate, remainingTerm);
    EXPECT_NEAR(interest + principal, totalPayment, 0.01);
}

TEST_F(AssetBaseTest, EvenAmortizationCalculation) {
    Balance balance = 120000.0;
    Rate rate = 0.04 / 12; // 4% annual, monthly
    int originalTerm = 120;
    int remainingTerm = 120;
    std::pair<Balance, int> amortInfo = {balance, originalTerm};
    
    auto [interest, principal] = calculatePrincipalInterest(
        AmortPlan::Even, balance, rate, originalTerm, remainingTerm, amortInfo);
    
    EXPECT_EQ(interest, balance * rate);
    EXPECT_EQ(principal, balance / remainingTerm); // Even principal distribution
}

TEST_F(AssetBaseTest, InterestOnlyCalculation) {
    Balance balance = 200000.0;
    Rate rate = 0.05 / 12;
    int originalTerm = 60;
    int remainingTerm = 30; // Not last payment
    std::pair<Balance, int> amortInfo = {balance, originalTerm};
    
    auto [interest, principal] = calculatePrincipalInterest(
        AmortPlan::I_P, balance, rate, originalTerm, remainingTerm, amortInfo);
    
    EXPECT_EQ(interest, balance * rate);
    EXPECT_EQ(principal, 0.0); // Interest only
}

TEST_F(AssetBaseTest, InterestOnlyFinalPayment) {
    Balance balance = 200000.0;
    Rate rate = 0.05 / 12;
    int originalTerm = 60;
    int remainingTerm = 1; // Final payment
    std::pair<Balance, int> amortInfo = {balance, originalTerm};
    
    auto [interest, principal] = calculatePrincipalInterest(
        AmortPlan::I_P, balance, rate, originalTerm, remainingTerm, amortInfo);
    
    EXPECT_EQ(interest, balance * rate);
    EXPECT_EQ(principal, balance); // Pay all principal at end
}

TEST_F(AssetBaseTest, BalloonPaymentCalculation) {
    Balance balance = 150000.0;
    Rate rate = 0.045 / 12;
    int originalTerm = 60;
    int remainingTerm = 1; // Final balloon payment
    std::pair<Balance, int> amortInfo = {balance, 360}; // Amortized over 30 years
    
    auto [interest, principal] = calculatePrincipalInterest(
        AmortPlan::Balloon, balance, rate, originalTerm, remainingTerm, amortInfo);
    
    EXPECT_EQ(interest, balance * rate);
    EXPECT_EQ(principal, balance); // Balloon payment = remaining balance
}