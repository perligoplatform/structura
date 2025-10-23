#include <gtest/gtest.h>
#include "assets/mortgage.h"
#include "core/types.h"

using namespace Structura;

class MortgageTest : public ::testing::Test {
protected:
    Date originDate = DateUtils::makeDate(2024, 1, 15);
    Date currentDate = DateUtils::makeDate(2024, 6, 15);
    
    // Standard 30-year mortgage parameters
    Balance originBalance = 300000.0;
    Rate originRate = 0.045; // 4.5% annual
    int originTerms = 360;   // 30 years monthly
    Period period = Period::Monthly;
    AmortPlan amortPlan = AmortPlan::Level;
    
    OriginalInfo createStandardMortgageInfo() {
        return OriginalInfo(originBalance, originRate, originTerms, 
                           originDate, period, amortPlan);
    }
};

// Test Mortgage Construction
TEST_F(MortgageTest, BasicMortgageConstruction) {
    OriginalInfo info = createStandardMortgageInfo();
    Mortgage mortgage(info, originBalance, originRate, originTerms);
    
    EXPECT_EQ(mortgage.getCurrentBalance(), originBalance);
    EXPECT_EQ(mortgage.getOriginBalance(), originBalance);
    EXPECT_EQ(mortgage.getCurrentRate(), originRate);
    EXPECT_EQ(mortgage.getOriginRate(), originRate);
    EXPECT_EQ(mortgage.getRemainingTerms(), originTerms);
    EXPECT_EQ(mortgage.getOriginTerms(), originTerms);
    EXPECT_EQ(mortgage.getStatus(), Status::Current);
    EXPECT_TRUE(mortgage.isCurrent());
    EXPECT_FALSE(mortgage.isDefaulted());
}

TEST_F(MortgageTest, MortgageWithBorrowerNumber) {
    OriginalInfo info = createStandardMortgageInfo();
    int borrowerCount = 2;
    Mortgage mortgage(info, originBalance, originRate, originTerms, borrowerCount);
    
    EXPECT_TRUE(mortgage.getBorrowerNumber().has_value());
    EXPECT_EQ(mortgage.getBorrowerNumber().value(), borrowerCount);
    EXPECT_EQ(mortgage.getBorrowerCount(), borrowerCount);
}

// Test Payment Calculations
TEST_F(MortgageTest, PaymentCalculation) {
    OriginalInfo info = createStandardMortgageInfo();
    Mortgage mortgage(info, originBalance, originRate, originTerms);
    
    auto [interest, principal] = mortgage.calculateNextPayment();
    
    // For a level payment mortgage, first payment should be mostly interest
    EXPECT_GT(interest, principal);
    EXPECT_GT(interest, 1000.0); // Should be over $1000 for this mortgage
    EXPECT_GT(principal, 0.0);   // Should have some principal
    
    // Total payment should be reasonable for a 30-year mortgage
    Amount totalPayment = interest + principal;
    EXPECT_GT(totalPayment, 1500.0);
    EXPECT_LT(totalPayment, 2000.0);
}

TEST_F(MortgageTest, MortgagePaymentUtils) {
    Balance balance = 200000.0;
    Rate rate = 0.06 / 12; // 6% annual, monthly
    int periods = 360;
    
    Amount payment = MortgageUtils::calculateMortgagePayment(balance, rate, periods);
    
    // Should be around $1199 for this mortgage
    EXPECT_GT(payment, 1100.0);
    EXPECT_LT(payment, 1300.0);
}

// Test Payment Processing
TEST_F(MortgageTest, MakePaymentReducesBalance) {
    OriginalInfo info = createStandardMortgageInfo();
    Mortgage mortgage(info, originBalance, originRate, originTerms);
    
    auto [interest, principal] = mortgage.calculateNextPayment();
    Balance originalBalance = mortgage.getCurrentBalance();
    int originalTerms = mortgage.getRemainingTerms();
    
    mortgage.makePayment(principal, interest, currentDate);
    
    EXPECT_EQ(mortgage.getCurrentBalance(), originalBalance - principal);
    EXPECT_EQ(mortgage.getRemainingTerms(), originalTerms - 1);
}

TEST_F(MortgageTest, PrepaymentsReduceBalance) {
    OriginalInfo info = createStandardMortgageInfo();
    Mortgage mortgage(info, originBalance, originRate, originTerms);
    
    Balance prepaymentAmount = 10000.0;
    Balance originalBalance = mortgage.getCurrentBalance();
    
    mortgage.applyPrepayment(prepaymentAmount, currentDate);
    
    EXPECT_EQ(mortgage.getCurrentBalance(), originalBalance - prepaymentAmount);
}

TEST_F(MortgageTest, FullPrepaymentZerosBalance) {
    OriginalInfo info = createStandardMortgageInfo();
    Mortgage mortgage(info, originBalance, originRate, originTerms);
    
    mortgage.applyPrepayment(originBalance + 1000.0, currentDate); // Overpay
    
    EXPECT_EQ(mortgage.getCurrentBalance(), 0.0);
    EXPECT_EQ(mortgage.getRemainingTerms(), 0);
}

// Test Default Handling
TEST_F(MortgageTest, DefaultStatusChange) {
    OriginalInfo info = createStandardMortgageInfo();
    Mortgage mortgage(info, originBalance, originRate, originTerms);
    
    mortgage.applyDefault(currentDate);
    
    EXPECT_EQ(mortgage.getStatus(), Status::Defaulted);
    EXPECT_TRUE(mortgage.isDefaulted());
    EXPECT_FALSE(mortgage.isCurrent());
}

// Test Reset Functionality
TEST_F(MortgageTest, ResetToOriginal) {
    OriginalInfo info = createStandardMortgageInfo();
    Mortgage mortgage(info, originBalance, originRate, originTerms);
    
    // Make some changes
    mortgage.makePayment(1000.0, 500.0, currentDate);
    mortgage.updateRate(0.055);
    
    Mortgage resetMortgage = mortgage.resetToOriginal();
    
    EXPECT_EQ(resetMortgage.getCurrentBalance(), originBalance);
    EXPECT_EQ(resetMortgage.getCurrentRate(), originRate);
    EXPECT_EQ(resetMortgage.getRemainingTerms(), originTerms);
    EXPECT_EQ(resetMortgage.getStatus(), Status::Current);
}

// Test Payment Date Generation
TEST_F(MortgageTest, PaymentDateGeneration) {
    OriginalInfo info = createStandardMortgageInfo();
    Mortgage mortgage(info, originBalance, originRate, originTerms);
    
    std::vector<Date> paymentDates = mortgage.getPaymentDates(12); // First year
    
    EXPECT_EQ(paymentDates.size(), originTerms + 12);
    EXPECT_GT(paymentDates[0], originDate); // First payment after origin
}

// Test Borrower Number Updates
TEST_F(MortgageTest, BorrowerNumberDecreasesWithBalance) {
    OriginalInfo info = createStandardMortgageInfo();
    Mortgage mortgage(info, originBalance, originRate, originTerms, 4); // 4 borrowers
    
    // Pay down 50% of balance
    mortgage.applyPrepayment(originBalance * 0.5, currentDate);
    
    // Borrower number should decrease proportionally (but round up)
    EXPECT_LE(mortgage.getBorrowerCount(), 4);
    EXPECT_GE(mortgage.getBorrowerCount(), 2);
}

// ARM Tests
TEST_F(MortgageTest, ARMConstruction) {
    OriginalInfo info = createStandardMortgageInfo();
    ARMInfo armInfo(60, 0.02, 0.01, 0.12, 0.025); // 5-year initial, caps
    
    AdjustableRateMortgage arm(info, armInfo, originBalance, originRate, originTerms);
    
    EXPECT_EQ(arm.getCurrentBalance(), originBalance);
    EXPECT_EQ(arm.getARMInfo().getInitPeriod(), 60);
    EXPECT_TRUE(arm.isInInitialPeriod()); // New mortgage is in initial period
}

TEST_F(MortgageTest, ARMRateAdjustment) {
    OriginalInfo info = createStandardMortgageInfo();
    ARMInfo armInfo(60, 0.02, 0.01, 0.12, 0.025);
    
    AdjustableRateMortgage arm(info, armInfo, originBalance, originRate, 300); // 60 periods passed
    
    EXPECT_FALSE(arm.isInInitialPeriod()); // Past initial period
    
    Rate newIndexRate = 0.06; // 6%
    Rate adjustedRate = arm.calculateAdjustedRate(newIndexRate, currentDate);
    
    // Should be capped by periodic cap
    EXPECT_LE(adjustedRate, originRate + armInfo.getPeriodicCap());
    EXPECT_GE(adjustedRate, originRate - armInfo.getPeriodicCap());
}

TEST_F(MortgageTest, ARMRateAdjustmentDates) {
    OriginalInfo info = createStandardMortgageInfo();
    ARMInfo armInfo(60, 0.02, 0.01, 0.12, 0.025);
    
    AdjustableRateMortgage arm(info, armInfo, originBalance, originRate, originTerms);
    
    std::vector<Date> adjustmentDates = arm.getRateAdjustmentDates();
    
    EXPECT_GT(adjustmentDates.size(), 0);
    // First adjustment should be after initial period
}

// Test Amortization Schedule Generation
TEST_F(MortgageTest, AmortizationScheduleGeneration) {
    OriginalInfo info = createStandardMortgageInfo();
    Mortgage mortgage(info, 100000.0, 0.06, 360); // 30-year monthly mortgage, 6% annual
    
    auto schedule = MortgageUtils::generateAmortizationSchedule(mortgage, 12); // First 12 payments
    
    EXPECT_EQ(schedule.size(), 12);
    
    // First payment should have higher interest than principal (for level payment)
    if (mortgage.getAmortPlan() == AmortPlan::Level) {
        EXPECT_GT(schedule[0].interestPayment, schedule[0].principalPayment);
    }
    
    // For partial schedule, balance should have decreased from original
    EXPECT_LT(schedule.back().endingBalance, mortgage.getCurrentBalance());
    
    // Balance should decrease monotonically
    for (size_t i = 1; i < schedule.size(); ++i) {
        EXPECT_LE(schedule[i].beginningBalance, schedule[i-1].beginningBalance);
    }
}

// Test Different Amortization Types
TEST_F(MortgageTest, EvenAmortizationPayments) {
    OriginalInfo evenInfo(100000.0, 0.06 / 12, 12, originDate, period, AmortPlan::Even);
    Mortgage evenMortgage(evenInfo, 100000.0, 0.06 / 12, 12);
    
    auto [interest, principal] = evenMortgage.calculateNextPayment();
    
    // For even amortization, principal should be balance / remaining terms
    Balance expectedPrincipal = 100000.0 / 12;
    EXPECT_NEAR(principal, expectedPrincipal, 0.01);
}

TEST_F(MortgageTest, InterestOnlyPayments) {
    OriginalInfo ioInfo(100000.0, 0.06 / 12, 12, originDate, period, AmortPlan::I_P);
    Mortgage ioMortgage(ioInfo, 100000.0, 0.06 / 12, 12);
    
    auto [interest, principal] = ioMortgage.calculateNextPayment();
    
    // Interest-only: should have no principal payment (except final payment)
    EXPECT_EQ(principal, 0.0);
    EXPECT_GT(interest, 0.0);
}

TEST_F(MortgageTest, InterestOnlyFinalPayment) {
    OriginalInfo ioInfo(100000.0, 0.06 / 12, 1, originDate, period, AmortPlan::I_P);
    Mortgage ioMortgage(ioInfo, 100000.0, 0.06 / 12, 1); // Final payment
    
    auto [interest, principal] = ioMortgage.calculateNextPayment();
    
    // Final payment: should pay all remaining principal
    EXPECT_EQ(principal, 100000.0);
    EXPECT_GT(interest, 0.0);
}