#include <gtest/gtest.h>
#include "../src/assets/credit_card.h"
#include <memory>
#include <stdexcept>

using namespace Structura;

class CreditCardTest : public ::testing::Test {
protected:
    void SetUp() override {
        // Create a test credit card with typical parameters
        test_card = std::make_unique<CreditCard>(
            "CC123456789",           // account_number
            "Rewards Card",          // product_type  
            5000.0,                  // credit_limit
            0.18,                    // apr (18%)
            Date(2024, 1, 1),       // origination_date
            "OBL001",               // obligor_id
            720.0                   // initial_fico
        );
    }

    void TearDown() override {
        test_card.reset();
    }

    std::unique_ptr<CreditCard> test_card;
};

// Basic construction and initialization tests
TEST_F(CreditCardTest, BasicConstruction) {
    EXPECT_EQ(test_card->getCurrentBalance(), 0.0);
    EXPECT_EQ(test_card->getOriginalBalance(), 5000.0);
    EXPECT_EQ(test_card->getCreditLimit(), 5000.0);
    EXPECT_EQ(test_card->getAvailableCredit(), 5000.0);
    EXPECT_EQ(test_card->getCurrentRate(), 0.18);
    EXPECT_EQ(test_card->getStatus(), Status::Current);
    EXPECT_EQ(test_card->getUtilizationCategory(), UtilizationCategory::LOW_UTILIZATION);
}

TEST_F(CreditCardTest, InvalidConstruction) {
    // Test invalid credit limit
    EXPECT_THROW(
        CreditCard("CC123", "Test", -1000.0, 0.18, Date(2024, 1, 1), "OBL001"),
        std::invalid_argument
    );
    
    // Test negative APR
    EXPECT_THROW(
        CreditCard("CC123", "Test", 5000.0, -0.05, Date(2024, 1, 1), "OBL001"),
        std::invalid_argument
    );
}

// Transaction processing tests
TEST_F(CreditCardTest, BasicTransaction) {
    Date transaction_date(2024, 2, 1);
    
    // Process a $1000 transaction
    test_card->processTransaction(1000.0, transaction_date, "Purchase");
    
    EXPECT_EQ(test_card->getCurrentBalance(), 1000.0);
    EXPECT_EQ(test_card->getAvailableCredit(), 4000.0);
    EXPECT_DOUBLE_EQ(test_card->getUtilizationRatio(), 0.20); // 20% utilization
    EXPECT_EQ(test_card->getUtilizationCategory(), UtilizationCategory::LOW_UTILIZATION);
}

TEST_F(CreditCardTest, MultipleTransactions) {
    Date transaction_date(2024, 2, 1);
    
    test_card->processTransaction(1500.0, transaction_date, "Purchase 1");
    test_card->processTransaction(800.0, transaction_date, "Purchase 2");  
    test_card->processTransaction(200.0, transaction_date, "Purchase 3");
    
    EXPECT_EQ(test_card->getCurrentBalance(), 2500.0);
    EXPECT_EQ(test_card->getAvailableCredit(), 2500.0);
    EXPECT_DOUBLE_EQ(test_card->getUtilizationRatio(), 0.50); // 50% utilization
    EXPECT_EQ(test_card->getUtilizationCategory(), UtilizationCategory::MODERATE_UTILIZATION);
}

TEST_F(CreditCardTest, OverlimitTransaction) {
    // Try to exceed credit limit by more than 10%
    Date transaction_date(2024, 2, 1);
    
    EXPECT_THROW(
        test_card->processTransaction(6000.0, transaction_date, "Large Purchase"),
        std::runtime_error
    );
}

// Utilization category tests
TEST_F(CreditCardTest, UtilizationCategories) {
    Date transaction_date(2024, 2, 1);
    
    // Test LOW_UTILIZATION (0-30%)
    test_card->processTransaction(1000.0, transaction_date);
    EXPECT_EQ(test_card->getUtilizationCategory(), UtilizationCategory::LOW_UTILIZATION);
    
    // Test MODERATE_UTILIZATION (30-60%)
    test_card->processTransaction(1500.0, transaction_date); // Total: 2500 (50%)
    EXPECT_EQ(test_card->getUtilizationCategory(), UtilizationCategory::MODERATE_UTILIZATION);
    
    // Test HIGH_UTILIZATION (60-80%)
    test_card->processTransaction(1000.0, transaction_date); // Total: 3500 (70%)
    EXPECT_EQ(test_card->getUtilizationCategory(), UtilizationCategory::HIGH_UTILIZATION);
    
    // Test MAXED_OUT (80%+)
    test_card->processTransaction(700.0, transaction_date); // Total: 4200 (84%)
    EXPECT_EQ(test_card->getUtilizationCategory(), UtilizationCategory::MAXED_OUT);
}

// Payment processing tests
TEST_F(CreditCardTest, BasicPayment) {
    Date transaction_date(2024, 2, 1);
    Date payment_date(2024, 2, 15);
    
    // Create balance
    test_card->processTransaction(2000.0, transaction_date, "Purchase");
    EXPECT_EQ(test_card->getCurrentBalance(), 2000.0);
    
    // Make payment
    test_card->makePayment(500.0, payment_date);
    EXPECT_EQ(test_card->getCurrentBalance(), 1500.0);
    EXPECT_EQ(test_card->getAvailableCredit(), 3500.0);
}

TEST_F(CreditCardTest, MinimumPayment) {
    Date transaction_date(2024, 2, 1);
    Date payment_date(2024, 2, 15);
    
    // Create $3000 balance
    test_card->processTransaction(3000.0, transaction_date, "Purchase");
    
    // Calculate minimum payment (2% of balance, min $25)
    Amount minimum_due = test_card->getMinimumPayment();
    EXPECT_EQ(minimum_due, 60.0); // 2% of $3000 = $60
    
    // Make minimum payment
    test_card->makeMinimumPayment(payment_date);
    EXPECT_EQ(test_card->getCurrentBalance(), 2940.0); // $3000 - $60
}

TEST_F(CreditCardTest, MinimumPaymentFloor) {
    Date transaction_date(2024, 2, 1);
    Date payment_date(2024, 2, 15);
    
    // Create small balance
    test_card->processTransaction(100.0, transaction_date, "Small Purchase");
    
    // Minimum payment should be floor amount ($25)
    Amount minimum_due = test_card->getMinimumPayment();
    EXPECT_EQ(minimum_due, 25.0); // Floor amount since 2% of $100 = $2 < $25
}

TEST_F(CreditCardTest, InvalidPayment) {
    EXPECT_THROW(
        test_card->makePayment(-100.0, Date(2024, 2, 1)),
        std::invalid_argument
    );
    
    EXPECT_THROW(
        test_card->makePayment(0.0, Date(2024, 2, 1)),
        std::invalid_argument
    );
}

// Cash advance tests
TEST_F(CreditCardTest, CashAdvance) {
    Date advance_date(2024, 2, 1);
    
    // Cash advance limit is typically 50% of credit limit
    Amount advance_amount = 1000.0;
    test_card->processCashAdvance(advance_amount, advance_date);
    
    // Should include 3% fee
    Amount expected_balance = advance_amount + (advance_amount * 0.03);
    EXPECT_EQ(test_card->getCurrentBalance(), expected_balance);
}

TEST_F(CreditCardTest, CashAdvanceExceedsLimit) {
    Date advance_date(2024, 2, 1);
    
    // Try to exceed cash advance limit (50% of $5000 = $2500)
    EXPECT_THROW(
        test_card->processCashAdvance(3000.0, advance_date),
        std::runtime_error
    );
}

// Interest calculation tests
TEST_F(CreditCardTest, InterestCharges) {
    Date transaction_date(2024, 2, 1);
    Date interest_date(2024, 2, 2);
    
    // Create balance
    test_card->processTransaction(1000.0, transaction_date, "Purchase");
    
    // Apply interest charges
    Amount balance_before = test_card->getCurrentBalance();
    test_card->applyInterestCharges(interest_date);
    Amount balance_after = test_card->getCurrentBalance();
    
    // Should have daily interest applied (18% APR / 365 days)
    Rate daily_rate = 0.18 / 365.0;
    Amount expected_interest = balance_before * daily_rate;
    EXPECT_NEAR(balance_after - balance_before, expected_interest, 0.01);
}

// Credit limit management tests
TEST_F(CreditCardTest, CreditLimitIncrease) {
    Date effective_date(2024, 3, 1);
    
    // Increase credit limit
    test_card->updateCreditLimit(7500.0, effective_date);
    
    EXPECT_EQ(test_card->getCreditLimit(), 7500.0);
    EXPECT_EQ(test_card->getAvailableCredit(), 7500.0);
}

TEST_F(CreditCardTest, CreditLimitDecrease) {
    Date transaction_date(2024, 2, 1);
    Date effective_date(2024, 3, 1);
    
    // Create balance first
    test_card->processTransaction(2000.0, transaction_date, "Purchase");
    
    // Decrease credit limit
    test_card->updateCreditLimit(3000.0, effective_date);
    
    EXPECT_EQ(test_card->getCreditLimit(), 3000.0);
    EXPECT_EQ(test_card->getAvailableCredit(), 1000.0); // $3000 - $2000
}

// Risk assessment tests
TEST_F(CreditCardTest, ProbabilityOfDefault) {
    // Test with current good credit profile (FICO 720, low utilization)
    double pd = test_card->calculateProbabilityOfDefault();
    EXPECT_GT(pd, 0.0);
    EXPECT_LT(pd, 0.05); // Should be low for good credit
    
    // Test with high utilization
    test_card->processTransaction(4500.0, Date(2024, 2, 1)); // 90% utilization
    double high_util_pd = test_card->calculateProbabilityOfDefault();
    EXPECT_GT(high_util_pd, pd); // Should be higher with high utilization
}

TEST_F(CreditCardTest, LossGivenDefault) {
    double lgd = test_card->calculateLossGivenDefault();
    EXPECT_GT(lgd, 0.7); // Credit cards are unsecured, so LGD should be high
    EXPECT_LE(lgd, 0.95);
}

TEST_F(CreditCardTest, ExpectedLoss) {
    Date transaction_date(2024, 2, 1);
    
    // Create balance
    test_card->processTransaction(3000.0, transaction_date, "Purchase");
    
    double expected_loss = test_card->calculateExpectedLoss();
    double pd = test_card->calculateProbabilityOfDefault();
    double lgd = test_card->calculateLossGivenDefault();
    double balance = test_card->getCurrentBalance();
    
    EXPECT_NEAR(expected_loss, pd * lgd * balance, 0.01);
}

TEST_F(CreditCardTest, HighRiskAccount) {
    // Initially should not be high risk
    EXPECT_FALSE(test_card->isHighRiskAccount());
    
    // Create high utilization
    test_card->processTransaction(4200.0, Date(2024, 2, 1)); // 84% utilization
    EXPECT_TRUE(test_card->isHighRiskAccount());
}

// Delinquency and default tests
TEST_F(CreditCardTest, LateFees) {
    Date transaction_date(2024, 2, 1);
    Date fee_date(2024, 3, 1);
    
    // Create balance
    test_card->processTransaction(1000.0, transaction_date, "Purchase");
    Amount balance_before = test_card->getCurrentBalance();
    
    // Apply late fee
    test_card->applyLateFees(fee_date);
    
    EXPECT_GT(test_card->getCurrentBalance(), balance_before);
    EXPECT_EQ(test_card->getStatus(), Status::Delinquent);
}

TEST_F(CreditCardTest, DefaultProcessing) {
    Date default_date(2024, 6, 1);
    
    // Create balance and apply default
    test_card->processTransaction(2000.0, Date(2024, 2, 1), "Purchase");
    test_card->applyDefault(default_date);
    
    EXPECT_EQ(test_card->getStatus(), Status::Defaulted);
}

TEST_F(CreditCardTest, RecoveryProcessing) {
    Date transaction_date(2024, 2, 1);
    Date recovery_date(2024, 6, 1);
    
    // Create balance and default
    test_card->processTransaction(2000.0, transaction_date, "Purchase");
    test_card->applyDefault(Date(2024, 5, 1));
    
    // Process recovery
    Amount recovery_amount = 500.0;
    test_card->processRecovery(recovery_amount, recovery_date);
    
    EXPECT_EQ(test_card->getCurrentBalance(), 1500.0); // $2000 - $500
}

// Account lifecycle tests
TEST_F(CreditCardTest, AccountFreeze) {
    Date freeze_date(2024, 3, 1);
    
    test_card->freezeAccount(freeze_date, "Fraud suspected");
    EXPECT_EQ(test_card->getStatus(), Status::Frozen);
}

TEST_F(CreditCardTest, AccountClosure) {
    Date close_date(2024, 12, 31);
    
    test_card->closeAccount(close_date, "Customer request");
    EXPECT_EQ(test_card->getStatus(), Status::Closed);
    EXPECT_EQ(test_card->getAvailableCredit(), 0.0);
}

// Summary and reporting tests
TEST_F(CreditCardTest, AccountSummary) {
    Date transaction_date(2024, 2, 1);
    
    // Create some activity
    test_card->processTransaction(1500.0, transaction_date, "Purchase");
    
    std::string summary = test_card->getAccountSummary();
    
    // Summary should contain key information
    EXPECT_TRUE(summary.find("CC123456789") != std::string::npos);
    EXPECT_TRUE(summary.find("Rewards Card") != std::string::npos);
    EXPECT_TRUE(summary.find("5000") != std::string::npos); // Credit limit
    EXPECT_TRUE(summary.find("1500") != std::string::npos); // Current balance
}

TEST_F(CreditCardTest, UtilizationTracking) {
    // Test current utilization tracking
    test_card->processTransaction(2500.0, Date(2024, 2, 1), "Purchase");
    
    double avg_utilization = test_card->getAverageUtilization(12);
    EXPECT_DOUBLE_EQ(avg_utilization, 0.50); // 50% utilization
}