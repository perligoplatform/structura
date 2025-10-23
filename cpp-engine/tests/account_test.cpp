#include <gtest/gtest.h>
#include "core/account.h"
#include "core/types.h"

using namespace Structura;

class AccountTest : public ::testing::Test {
protected:
    void SetUp() override {
        testDate = QuantLib::Date(15, QuantLib::March, 2024);
        futureDate = QuantLib::Date(15, QuantLib::June, 2024);
    }
    
    Date testDate;
    Date futureDate;
};

TEST_F(AccountTest, BasicAccountCreation) {
    Account account("TestAccount", 1000.0);
    
    EXPECT_EQ(account.getName(), "TestAccount");
    EXPECT_DOUBLE_EQ(account.getBalance(), 1000.0);
    EXPECT_FALSE(account.getInterestInfo().has_value());
    EXPECT_FALSE(account.getReserveAmount().has_value());
    EXPECT_TRUE(account.getTransactions().empty());
}

TEST_F(AccountTest, DepositAndWithdraw) {
    Account account("TestAccount", 1000.0);
    
    // Test deposit
    account.deposit(testDate, 500.0, "Test deposit");
    EXPECT_DOUBLE_EQ(account.getBalance(), 1500.0);
    EXPECT_EQ(account.getTransactions().size(), 1);
    EXPECT_EQ(account.getTransactions()[0].comment, "Test deposit");
    EXPECT_DOUBLE_EQ(account.getTransactions()[0].amount, 500.0);
    
    // Test withdrawal
    bool success = account.withdraw(testDate, 300.0, "Test withdrawal");
    EXPECT_TRUE(success);
    EXPECT_DOUBLE_EQ(account.getBalance(), 1200.0);
    EXPECT_EQ(account.getTransactions().size(), 2);
    EXPECT_EQ(account.getTransactions()[1].comment, "Test withdrawal");
    EXPECT_DOUBLE_EQ(account.getTransactions()[1].amount, -300.0);
    
    // Test insufficient funds
    bool failedWithdraw = account.withdraw(testDate, 2000.0, "Should fail");
    EXPECT_FALSE(failedWithdraw);
    EXPECT_DOUBLE_EQ(account.getBalance(), 1200.0); // Balance unchanged
    EXPECT_EQ(account.getTransactions().size(), 2); // No new transaction
}

TEST_F(AccountTest, InterestInfoCreation) {
    // Test bank account interest
    auto bankInterest = InterestInfo::createBankAccount(0.025, testDate); // 2.5%
    EXPECT_EQ(bankInterest.getType(), InterestInfo::Type::BankAccount);
    EXPECT_DOUBLE_EQ(bankInterest.getRate(), 0.025);
    EXPECT_EQ(bankInterest.getLastAccrueDate(), testDate);
    
    // Test investment account interest
    auto investmentInterest = InterestInfo::createInvestmentAccount(
        Index::LIBOR3M, 0.01, testDate, 0.035); // LIBOR3M + 1% spread, current rate 3.5%
    EXPECT_EQ(investmentInterest.getType(), InterestInfo::Type::InvestmentAccount);
    EXPECT_EQ(investmentInterest.getIndex(), Index::LIBOR3M);
    EXPECT_DOUBLE_EQ(investmentInterest.getSpread(), 0.01);
    EXPECT_DOUBLE_EQ(investmentInterest.getLastResetRate(), 0.035);
}

TEST_F(AccountTest, InterestAccrual) {
    Account account("InterestAccount", 10000.0);
    
    // Set up bank account interest at 3.65% annual (1% for 100 days)
    auto interestInfo = InterestInfo::createBankAccount(0.0365, testDate);
    account.setInterestInfo(interestInfo);
    
    // Calculate accrued interest for 100 days later
    Date futureDate100 = testDate + 100;
    Balance accruedInterest = account.calculateAccruedInterest(futureDate100);
    
    // Expected: 10000 * 0.0365 * 100/365 = 100
    EXPECT_NEAR(accruedInterest, 100.0, 0.01);
    
    // Accrue the interest
    account.accrueInterest(futureDate100);
    EXPECT_NEAR(account.getBalance(), 10100.0, 0.01);
    EXPECT_EQ(account.getTransactions().size(), 1);
    EXPECT_EQ(account.getTransactions()[0].comment, "ACCRUED_INTEREST");
}

TEST_F(AccountTest, ReserveAmountFixed) {
    auto fixedReserve = ReserveAmount::createFixReserve(5000.0);
    Account account("ReserveAccount", 3000.0, fixedReserve);
    
    EXPECT_DOUBLE_EQ(account.getTargetReserveAmount(), 5000.0);
    EXPECT_DOUBLE_EQ(account.getReserveGap(), 2000.0); // Need 2000 more
    EXPECT_DOUBLE_EQ(account.getReserveExcess(), 0.0); // No excess
    
    // Add funds to exceed reserve
    account.deposit(testDate, 3000.0);
    EXPECT_DOUBLE_EQ(account.getReserveGap(), 0.0); // No gap
    EXPECT_DOUBLE_EQ(account.getReserveExcess(), 1000.0); // 1000 excess
}

TEST_F(AccountTest, ReserveAmountMax) {
    auto reserve1 = std::make_shared<ReserveAmount>(ReserveAmount::createFixReserve(3000.0));
    auto reserve2 = std::make_shared<ReserveAmount>(ReserveAmount::createFixReserve(5000.0));
    auto reserve3 = std::make_shared<ReserveAmount>(ReserveAmount::createFixReserve(2000.0));
    
    std::vector<std::shared_ptr<ReserveAmount>> reserves = {reserve1, reserve2, reserve3};
    auto maxReserve = ReserveAmount::createMax(reserves);
    
    EXPECT_DOUBLE_EQ(maxReserve.evaluate(), 5000.0); // Should be the maximum
    
    Account account("MaxReserveAccount", 4000.0, maxReserve);
    EXPECT_DOUBLE_EQ(account.getTargetReserveAmount(), 5000.0);
    EXPECT_DOUBLE_EQ(account.getReserveGap(), 1000.0);
}

TEST_F(AccountTest, ReserveAmountMin) {
    auto reserve1 = std::make_shared<ReserveAmount>(ReserveAmount::createFixReserve(3000.0));
    auto reserve2 = std::make_shared<ReserveAmount>(ReserveAmount::createFixReserve(5000.0));
    auto reserve3 = std::make_shared<ReserveAmount>(ReserveAmount::createFixReserve(2000.0));
    
    std::vector<std::shared_ptr<ReserveAmount>> reserves = {reserve1, reserve2, reserve3};
    auto minReserve = ReserveAmount::createMin(reserves);
    
    EXPECT_DOUBLE_EQ(minReserve.evaluate(), 2000.0); // Should be the minimum
    
    Account account("MinReserveAccount", 4000.0, minReserve);
    EXPECT_DOUBLE_EQ(account.getTargetReserveAmount(), 2000.0);
    EXPECT_DOUBLE_EQ(account.getReserveExcess(), 2000.0);
}

TEST_F(AccountTest, AccountTransfer) {
    Account sourceAccount("SourceAccount", 1000.0);
    Account targetAccount("TargetAccount", 500.0);
    
    // Successful transfer
    bool success = Account::transfer(sourceAccount, targetAccount, testDate, 300.0, "Test transfer");
    EXPECT_TRUE(success);
    EXPECT_DOUBLE_EQ(sourceAccount.getBalance(), 700.0);
    EXPECT_DOUBLE_EQ(targetAccount.getBalance(), 800.0);
    
    // Check transaction records
    EXPECT_EQ(sourceAccount.getTransactions().size(), 1);
    EXPECT_EQ(targetAccount.getTransactions().size(), 1);
    EXPECT_EQ(sourceAccount.getTransactions()[0].comment, "Test transfer");
    EXPECT_EQ(targetAccount.getTransactions()[0].comment, "Test transfer");
    
    // Failed transfer (insufficient funds)
    bool failedTransfer = Account::transfer(sourceAccount, targetAccount, testDate, 1000.0);
    EXPECT_FALSE(failedTransfer);
    EXPECT_DOUBLE_EQ(sourceAccount.getBalance(), 700.0); // Unchanged
    EXPECT_DOUBLE_EQ(targetAccount.getBalance(), 800.0); // Unchanged
}

TEST_F(AccountTest, TransactionQueries) {
    Account account("QueryAccount", 1000.0);
    
    account.deposit(testDate, 200.0, "POOL_COLLECTION");
    account.withdraw(testDate, 50.0, "FEE_PAYMENT");
    account.deposit(testDate, 100.0, "POOL_COLLECTION");
    account.withdraw(testDate, 75.0, "BOND_PAYMENT");
    
    // Query by comment pattern
    auto poolTxns = account.queryTransactions("POOL");
    EXPECT_EQ(poolTxns.size(), 2);
    
    auto feeTxns = account.queryTransactions("FEE");
    EXPECT_EQ(feeTxns.size(), 1);
    EXPECT_DOUBLE_EQ(feeTxns[0].amount, -50.0);
    
    // Query by date range
    Date nextDay = testDate + 1;
    account.deposit(nextDay, 150.0, "NEXT_DAY_DEPOSIT");
    
    auto sameDayTxns = account.getTransactionsInRange(testDate, testDate);
    EXPECT_EQ(sameDayTxns.size(), 4); // All transactions on testDate
    
    auto allTxns = account.getTransactionsInRange(testDate, nextDay);
    EXPECT_EQ(allTxns.size(), 5); // All transactions in range
}

TEST_F(AccountTest, AverageBalanceCalculation) {
    Account account("AvgBalanceAccount", 1000.0);
    
    Date day1 = testDate;
    Date day2 = testDate + 1;
    Date day3 = testDate + 2;
    
    account.deposit(day1, 500.0); // Balance: 1500
    account.withdraw(day2, 200.0); // Balance: 1300
    account.deposit(day3, 300.0); // Balance: 1600
    
    Balance avgBalance = account.getAverageBalance(day1, day3);
    
    // Average of final balances: (1500 + 1300 + 1600) / 3 = 1466.67
    EXPECT_NEAR(avgBalance, 1466.67, 0.01);
}

TEST_F(AccountTest, ComplexAccountWithInterestAndReserve) {
    // Create account with both interest and reserve
    auto reserve = ReserveAmount::createFixReserve(1000.0);
    auto interest = InterestInfo::createBankAccount(0.05, testDate); // 5% annual
    
    Account account("ComplexAccount", 800.0, reserve, interest);
    
    // Check initial state
    EXPECT_DOUBLE_EQ(account.getBalance(), 800.0);
    EXPECT_DOUBLE_EQ(account.getTargetReserveAmount(), 1000.0);
    EXPECT_DOUBLE_EQ(account.getReserveGap(), 200.0);
    EXPECT_TRUE(account.getInterestInfo().has_value());
    
    // Accrue interest for 73 days (20% of year) = 5% * 20% * 800 = 8
    Date accrueDate = testDate + 73;
    Balance expectedInterest = 800.0 * 0.05 * 73.0 / 365.0;
    
    account.accrueInterest(accrueDate);
    EXPECT_NEAR(account.getBalance(), 800.0 + expectedInterest, 0.01);
    
    // The interest should help close the reserve gap
    Balance newGap = account.getReserveGap();
    EXPECT_LT(newGap, 200.0); // Gap should be smaller now
}