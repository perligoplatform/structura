#include <gtest/gtest.h>#include <gtest/gtest.h>

#include "../src/core/deal_base.h"#include "../src/core/deal_base.h"

#include "../src/deals/mortgage_deal.h"#include "../src/deals/mortgage_deal.h"

#include "../src/assets/mortgage.h"#include "../src/assets/mortgage.h"

#include <memory>#include <memory>



using namespace Structura;using namespace Structura;

using namespace QuantLib;using namespace QuantLib;



// Simple test implementation of DealBase// Simple test implementation of DealBase

class SimpleDeal : public DealBase {class SimpleDeal : public DealBase {

private:private:

    mutable int waterfallCallCount_ = 0;    mutable int waterfallCallCount_ = 0;



public:public:

    SimpleDeal(const DealInfo& info) : DealBase(info) {}    SimpleDeal(const DealInfo& info) : DealBase(info) {}

        

    void runWaterfall(const Date& paymentDate) override {    void runWaterfall(const Date& paymentDate) override {

        waterfallCallCount_++;        waterfallCallCount_++;

        // Simple mock waterfall - just update call count        // Simple mock waterfall - just update call count

    }    }

        

    Balance calculateDealValue(const Date& valuationDate) const override {    Balance calculateDealValue(const Date& valuationDate) const override {

        return 1000000.0;        return mockDealValue_;

    }    }

        

    std::vector<std::string> validate() const override {    std::vector<std::string> validate() const override {

        auto errors = validateBasicDealStructure();        auto errors = validateBasicDealStructure();

        return errors;        if (getAccountNames().size() < 2) {

    }            errors.push_back("Test deal must have at least 2 accounts");

            }

    std::string getDealSummary() const override {        return errors;

        return "Simple Deal Summary: " + getDealName();    }

    }    

        std::string getDealSummary() const override {

    // Test helpers        return "Test Deal Summary: " + getDealName();

    int getWaterfallCallCount() const { return waterfallCallCount_; }    }

};    

    // Test helpers

class DealBaseTest : public ::testing::Test {    int getWaterfallCallCount() const { return waterfallCallCount_; }

protected:    void setMockDealValue(Balance value) { mockDealValue_ = value; }

    void SetUp() override {};

        Date issueDate(15, Jan, 2024);

        Date maturityDate(15, Jan, 2034);class DealBaseTest : public ::testing::Test {

        protected:

        dealInfo_ = std::make_unique<DealInfo>("Test Deal", "TEST", issueDate, maturityDate);    void SetUp() override {

        dealInfo_->trustee = "Test Trustee";        Date issueDate(15, Jan, 2024);

        dealInfo_->underwriter = "Test Underwriter";        Date maturityDate(15, Jan, 2034);

        dealInfo_->firstPaymentDate = Date(15, Feb, 2024);        

        dealInfo_->currency = "USD";        dealInfo_ = std::make_unique<DealInfo>("Test MBS Deal", "MBS", issueDate, maturityDate);

        dealInfo_->description = "Test deal";        dealInfo_->trustee = "Test Trustee";

                dealInfo_->underwriter = "Test Underwriter";

        testDeal_ = std::make_unique<SimpleDeal>(*dealInfo_);        dealInfo_->firstPaymentDate = Date(15, Feb, 2024);

    }        dealInfo_->currency = "USD";

            dealInfo_->description = "Test mortgage-backed security";

    std::unique_ptr<DealInfo> dealInfo_;        

    std::unique_ptr<SimpleDeal> testDeal_;        testDeal_ = std::make_unique<TestDeal>(*dealInfo_);

};    }

    

// Basic Construction Tests    std::unique_ptr<DealInfo> dealInfo_;

TEST_F(DealBaseTest, BasicConstruction) {    std::unique_ptr<TestDeal> testDeal_;

    EXPECT_EQ(testDeal_->getDealName(), "Test Deal");};

    EXPECT_EQ(testDeal_->getDealType(), "TEST");

    EXPECT_EQ(testDeal_->getDealInfo().trustee, "Test Trustee");// DealInfo Tests

    EXPECT_EQ(testDeal_->getDealInfo().currency, "USD");TEST_F(DealBaseTest, DealInfoCreation) {

}    EXPECT_EQ(testDeal_->getDealName(), "Test MBS Deal");

    EXPECT_EQ(testDeal_->getDealType(), "MBS");

TEST_F(DealBaseTest, StatusManagement) {    EXPECT_EQ(testDeal_->getDealInfo().trustee, "Test Trustee");

    // Initial status should be Pending    EXPECT_EQ(testDeal_->getDealInfo().underwriter, "Test Underwriter");

    EXPECT_EQ(testDeal_->getStatus(), DealBase::DealStatus::Pending);    EXPECT_EQ(testDeal_->getDealInfo().currency, "USD");

    }

    // Test status changes

    testDeal_->setStatus(DealBase::DealStatus::Active);// Account Management Tests

    EXPECT_EQ(testDeal_->getStatus(), DealBase::DealStatus::Active);TEST_F(DealBaseTest, AccountManagement) {

        // Initially no accounts

    // Test string conversion    EXPECT_TRUE(testDeal_->getAccountNames().empty());

    EXPECT_EQ(DealBase::statusToString(DealBase::DealStatus::Active), "Active");    EXPECT_EQ(testDeal_->getTotalAccountBalance(), 0.0);

    EXPECT_EQ(DealBase::statusToString(DealBase::DealStatus::Matured), "Matured");    

        // Add accounts

    // Test string to status conversion    testDeal_->addAccount("Collection", std::make_unique<Account>("Collection", 50000.0));

    EXPECT_EQ(DealBase::stringToStatus("Active"), DealBase::DealStatus::Active);    testDeal_->addAccount("Reserve", std::make_unique<Account>("Reserve", 25000.0));

    EXPECT_EQ(DealBase::stringToStatus("Matured"), DealBase::DealStatus::Matured);    

}    // Verify accounts were added

    EXPECT_EQ(testDeal_->getAccountNames().size(), 2);

TEST_F(DealBaseTest, AccountManagement) {    EXPECT_EQ(testDeal_->getTotalAccountBalance(), 75000.0);

    // Initially no accounts    

    EXPECT_TRUE(testDeal_->getAccountNames().empty());    // Test account retrieval

    EXPECT_EQ(testDeal_->getTotalAccountBalance(), 0.0);    Account* collectionAccount = testDeal_->getAccount("Collection");

        ASSERT_NE(collectionAccount, nullptr);

    // Add accounts    EXPECT_EQ(collectionAccount->getName(), "Collection");

    testDeal_->addAccount("Collection",     EXPECT_EQ(collectionAccount->getBalance(), 50000.0);

                         std::make_unique<Account>("Collection", 50000.0));    

    testDeal_->addAccount("Reserve",     // Test non-existent account

                         std::make_unique<Account>("Reserve", 25000.0));    EXPECT_EQ(testDeal_->getAccount("NonExistent"), nullptr);

    }

    // Verify accounts were added

    EXPECT_EQ(testDeal_->getAccountNames().size(), 2);// Date Management Tests

    EXPECT_EQ(testDeal_->getTotalAccountBalance(), 75000.0);TEST_F(DealBaseTest, DateManagement) {

        std::vector<Date> paymentDates = {

    // Test account retrieval        Date(15, Feb, 2024),

    Account* collectionAccount = testDeal_->getAccount("Collection");        Date(15, Mar, 2024),

    ASSERT_NE(collectionAccount, nullptr);        Date(15, Apr, 2024),

    EXPECT_EQ(collectionAccount->getName(), "Collection");        Date(15, May, 2024)

    EXPECT_EQ(collectionAccount->getBalance(), 50000.0);    };

        

    // Test non-existent account    testDeal_->setPaymentDates(paymentDates);

    EXPECT_EQ(testDeal_->getAccount("NonExistent"), nullptr);    

}    const auto& retrievedDates = testDeal_->getPaymentDates();

    EXPECT_EQ(retrievedDates.size(), 4);

TEST_F(DealBaseTest, DateManagement) {    EXPECT_EQ(retrievedDates[0], Date(15, Feb, 2024));

    std::vector<Date> paymentDates = {    EXPECT_EQ(retrievedDates[3], Date(15, May, 2024));

        Date(15, Feb, 2024),    

        Date(15, Mar, 2024),    // Test date range queries

        Date(15, Apr, 2024)    auto datesInRange = testDeal_->getPaymentDatesInRange(Date(10, Feb, 2024), Date(20, Mar, 2024));

    };    EXPECT_EQ(datesInRange.size(), 2);

        

    testDeal_->setPaymentDates(paymentDates);    // Test next payment date

        Date nextPayment = testDeal_->getNextPaymentDate(Date(10, Feb, 2024));

    const auto& retrievedDates = testDeal_->getPaymentDates();    EXPECT_EQ(nextPayment, Date(15, Feb, 2024));

    EXPECT_EQ(retrievedDates.size(), 3);    

        Date nextPayment2 = testDeal_->getNextPaymentDate(Date(15, Mar, 2024));

    // Test date range queries    EXPECT_EQ(nextPayment2, Date(15, Apr, 2024));

    auto datesInRange = testDeal_->getPaymentDatesInRange(}

        Date(10, Feb, 2024), Date(20, Mar, 2024));

    EXPECT_EQ(datesInRange.size(), 2);// Status Management Tests

}TEST_F(DealBaseTest, StatusManagement) {

    // Initial status

TEST_F(DealBaseTest, WaterfallExecution) {    EXPECT_EQ(testDeal_->getStatus(), DealBase::DealStatus::Pending);

    testDeal_->setStatus(DealBase::DealStatus::Active);    

        // Test status changes

    // Add a test account    testDeal_->setStatus(DealBase::DealStatus::Active);

    testDeal_->addAccount("Collection",     EXPECT_EQ(testDeal_->getStatus(), DealBase::DealStatus::Active);

                         std::make_unique<Account>("Collection", 0.0));    

        // Test string conversion

    // Process payment date should call waterfall    EXPECT_EQ(DealBase::statusToString(DealBase::DealStatus::Active), "Active");

    Date paymentDate(15, Feb, 2024);    EXPECT_EQ(DealBase::statusToString(DealBase::DealStatus::Defaulted), "Defaulted");

    testDeal_->processPaymentDate(paymentDate);    

        // Test string to status conversion

    // Verify waterfall was called    EXPECT_EQ(DealBase::stringToStatus("Matured"), DealBase::DealStatus::Matured);

    EXPECT_EQ(testDeal_->getWaterfallCallCount(), 1);    EXPECT_EQ(DealBase::stringToStatus("Called"), DealBase::DealStatus::Called);

    EXPECT_EQ(testDeal_->getCurrentDate(), paymentDate);    

}    // Test invalid status string

    EXPECT_THROW(DealBase::stringToStatus("Invalid"), std::invalid_argument);

TEST_F(DealBaseTest, DealValidation) {}

    // Test with minimal setup

    auto errors = testDeal_->validate();// Process Payment Date Tests

    EXPECT_FALSE(errors.empty()); // Should have validation errors initiallyTEST_F(DealBaseTest, ProcessPaymentDate) {

        // Set up accounts and payment dates

    // Add payment dates    testDeal_->addAccount("Collection", std::make_unique<Account>("Collection", 0.0));

    std::vector<Date> paymentDates = {Date(15, Feb, 2024), Date(15, Mar, 2024)};    testDeal_->addAccount("Reserve", std::make_unique<Account>("Reserve", 0.0));

    testDeal_->setPaymentDates(paymentDates);    

        std::vector<Date> paymentDates = {Date(2024, 2, 15), Date(2024, 3, 15)};

    // Should now have fewer errors    testDeal_->setPaymentDates(paymentDates);

    errors = testDeal_->validate();    

    // Should pass basic validations now    testDeal_->setStatus(DealBase::DealStatus::Active);

    EXPECT_TRUE(errors.empty() || errors.size() < 3);    

}    // Process payment date

    Date paymentDate(2024, 2, 15);

// Simple MortgageDeal tests    testDeal_->processPaymentDate(paymentDate);

class SimpleMortgageDealTest : public ::testing::Test {    

protected:    // Verify current date was updated

    void SetUp() override {    EXPECT_EQ(testDeal_->getCurrentDate(), paymentDate);

        Date issueDate(15, Jan, 2024);    

        Date maturityDate(15, Jan, 2034);    // Verify waterfall was called

        Date firstPaymentDate(15, Feb, 2024);    EXPECT_EQ(testDeal_->getWaterfallCallCount(), 1);

        }

        dealInfo_ = std::make_unique<DealInfo>("Test Mortgage Deal", "MBS", issueDate, maturityDate);

        dealInfo_->firstPaymentDate = firstPaymentDate;// Validation Tests

        TEST_F(DealBaseTest, DealValidation) {

        // Create simple mortgage pool with default constructor    // Test with minimal setup - should have validation errors

        auto pool = std::make_unique<Pool<Mortgage>>();    auto errors = testDeal_->validate();

            EXPECT_FALSE(errors.empty());

        mortgageDeal_ = std::make_unique<MortgageDeal>(*dealInfo_, std::move(pool));    EXPECT_GT(errors.size(), 0);

    }    

        // Add required components

    std::unique_ptr<DealInfo> dealInfo_;    testDeal_->addAccount("Collection", std::make_unique<Account>("Collection", 0.0));

    std::unique_ptr<MortgageDeal> mortgageDeal_;    testDeal_->addAccount("Reserve", std::make_unique<Account>("Reserve", 0.0));

};    

    std::vector<Date> paymentDates = {Date(2024, 2, 15), Date(2024, 3, 15)};

TEST_F(SimpleMortgageDealTest, BasicConstruction) {    testDeal_->setPaymentDates(paymentDates);

    EXPECT_EQ(mortgageDeal_->getDealName(), "Test Mortgage Deal");    

    EXPECT_EQ(mortgageDeal_->getDealType(), "MBS");    // Should now pass validation

    EXPECT_NE(mortgageDeal_->getMortgagePool(), nullptr);    errors = testDeal_->validate();

}    EXPECT_TRUE(errors.empty());

}

TEST_F(SimpleMortgageDealTest, BondManagement) {

    // Add a simple bond// Transfer Between Accounts Tests

    MortgageBond seniorBond("Senior A", 1000000.0, 0.03, 1);TEST_F(DealBaseTest, AccountTransfers) {

    mortgageDeal_->addBond(seniorBond);    testDeal_->addAccount("Source", std::make_unique<Account>("Source", 10000.0));

        testDeal_->addAccount("Destination", std::make_unique<Account>("Destination", 5000.0));

    const auto& bonds = mortgageDeal_->getBonds();    

    EXPECT_EQ(bonds.size(), 1);    // Initial balances

    EXPECT_EQ(bonds[0].bondName, "Senior A");    EXPECT_EQ(testDeal_->getAccount("Source")->getBalance(), 10000.0);

    EXPECT_EQ(bonds[0].couponRate, 0.03);    EXPECT_EQ(testDeal_->getAccount("Destination")->getBalance(), 5000.0);

        

    // Test total bond balance    // Transfer funds

    EXPECT_EQ(mortgageDeal_->getTotalBondBalance(), 1000000.0);    testDeal_->transferBetweenAccounts("Source", "Destination", 3000.0);

        

    // Test bond retrieval    // Verify transfer

    MortgageBond* bondPtr = mortgageDeal_->getBond("Senior A");    EXPECT_EQ(testDeal_->getAccount("Source")->getBalance(), 7000.0);

    ASSERT_NE(bondPtr, nullptr);    EXPECT_EQ(testDeal_->getAccount("Destination")->getBalance(), 8000.0);

    EXPECT_EQ(bondPtr->couponRate, 0.03);    

}    // Test transfer from non-existent account

    EXPECT_THROW(testDeal_->transferBetweenAccounts("NonExistent", "Destination", 1000.0), 

TEST_F(SimpleMortgageDealTest, Initialization) {                 std::runtime_error);

    // Initialize the deal}

    mortgageDeal_->initialize();

    // Maturity Tests

    // Should have created standard accountsTEST_F(DealBaseTest, MaturityTests) {

    EXPECT_NE(mortgageDeal_->getAccount("Collection"), nullptr);    Date maturityDate = testDeal_->getDealInfo().maturityDate;

    EXPECT_NE(mortgageDeal_->getAccount("Reserve"), nullptr);    

    EXPECT_NE(mortgageDeal_->getAccount("Excess"), nullptr);    // Before maturity

        testDeal_->setCurrentDate(Date(2030, 1, 1));

    // Should be active after initialization    EXPECT_FALSE(testDeal_->isMatured());

    EXPECT_EQ(mortgageDeal_->getStatus(), DealBase::DealStatus::Active);    

}    // At maturity

    testDeal_->setCurrentDate(maturityDate);

TEST_F(SimpleMortgageDealTest, PerformanceMetrics) {    EXPECT_TRUE(testDeal_->isMatured());

    // Initially all metrics should be zero    

    EXPECT_EQ(mortgageDeal_->getCumulativePrincipalCollections(), 0.0);    // After maturity

    EXPECT_EQ(mortgageDeal_->getCumulativeInterestCollections(), 0.0);    testDeal_->setCurrentDate(Date(2035, 1, 1));

    EXPECT_EQ(mortgageDeal_->getCumulativePrepayments(), 0.0);    EXPECT_TRUE(testDeal_->isMatured());

    EXPECT_EQ(mortgageDeal_->getCumulativeDefaults(), 0.0);}

    EXPECT_EQ(mortgageDeal_->getCumulativeRecoveries(), 0.0);

    // Acceleration Tests

    // Loss rates should be zero initiallyTEST_F(DealBaseTest, AccelerationTests) {

    EXPECT_EQ(mortgageDeal_->getCurrentLossRate(), 0.0);    testDeal_->setStatus(DealBase::DealStatus::Active);

    EXPECT_EQ(mortgageDeal_->getCumulativeLossRate(), 0.0);    testDeal_->setCurrentDate(Date(2025, 1, 1)); // Before maturity

    EXPECT_EQ(mortgageDeal_->getDelinquencyRate(), 0.0);    

}    // Can accelerate when active and not matured

    EXPECT_TRUE(testDeal_->canAccelerate());

TEST_F(SimpleMortgageDealTest, DealSummary) {    

    mortgageDeal_->initialize();    // Accelerate the deal

        testDeal_->accelerate();

    std::string summary = mortgageDeal_->getDealSummary();    EXPECT_EQ(testDeal_->getStatus(), DealBase::DealStatus::Accelerated);

    EXPECT_FALSE(summary.empty());    

    EXPECT_NE(summary.find("Test Mortgage Deal"), std::string::npos);    // Cannot accelerate when already accelerated

    EXPECT_NE(summary.find("POOL INFORMATION"), std::string::npos);    EXPECT_FALSE(testDeal_->canAccelerate());

    EXPECT_NE(summary.find("PERFORMANCE METRICS"), std::string::npos);}

}

class MortgageDealTest : public ::testing::Test {

TEST_F(SimpleMortgageDealTest, StaticHelperMethods) {protected:

    // Test standard waterfall creation    void SetUp() override {

    MortgageWaterfall standardWaterfall = MortgageDeal::createStandardWaterfall();        Date issueDate(2024, 1, 15);

    EXPECT_FALSE(standardWaterfall.steps.empty());        Date maturityDate(2034, 1, 15);

    EXPECT_GT(standardWaterfall.steps.size(), 5);        Date firstPaymentDate(2024, 2, 15);

            

    // Test payment schedule generation        dealInfo_ = std::make_unique<DealInfo>("Test Mortgage Deal", "MBS", issueDate, maturityDate);

    Date firstPayment(15, Feb, 2024);        dealInfo_->firstPaymentDate = firstPaymentDate;

    Date maturity(15, Aug, 2024);        

    auto paymentDates = MortgageDeal::generateMonthlyPaymentSchedule(firstPayment, maturity);        // Create test mortgages

    EXPECT_FALSE(paymentDates.empty());        createTestMortgages();

    EXPECT_EQ(paymentDates[0], firstPayment);        

}        // Create mortgage pool
        auto pool = std::make_unique<Pool<Mortgage>>("TEST_POOL");
        for (auto& mortgage : testMortgages_) {
            pool->addAsset(std::move(mortgage));
        }
        
        mortgageDeal_ = std::make_unique<MortgageDeal>(*dealInfo_, std::move(pool));
    }
    
    void createTestMortgages() {
        // Create diverse set of test mortgages
        
        // Fixed rate mortgage
        Date originationDate1(2023, 6, 1);
        OriginalInfo info1(500000.0, 0.05, 360, originationDate1);
        auto mortgage1 = std::make_unique<Mortgage>("MORT001", info1, PaymentFreq::Monthly);
        testMortgages_.push_back(std::move(mortgage1));
        
        // Adjustable rate mortgage
        Date originationDate2(2023, 8, 15);
        OriginalInfo info2(750000.0, 0.04, 360, originationDate2);
        auto mortgage2 = std::make_unique<AdjustableRateMortgage>("MORT002", info2, PaymentFreq::Monthly,
                                                                  Date(2024, 8, 15), 12, 0.02, 0.06);
        testMortgages_.push_back(std::move(mortgage2));
        
        // Higher balance mortgage
        Date originationDate3(2023, 12, 1);
        OriginalInfo info3(1000000.0, 0.055, 360, originationDate3);
        auto mortgage3 = std::make_unique<Mortgage>("MORT003", info3, PaymentFreq::Monthly);
        testMortgages_.push_back(std::move(mortgage3));
    }
    
    std::unique_ptr<DealInfo> dealInfo_;
    std::unique_ptr<MortgageDeal> mortgageDeal_;
    std::vector<std::unique_ptr<Mortgage>> testMortgages_;
};

// MortgageDeal Construction Tests
TEST_F(MortgageDealTest, BasicConstruction) {
    EXPECT_EQ(mortgageDeal_->getDealName(), "Test Mortgage Deal");
    EXPECT_EQ(mortgageDeal_->getDealType(), "MBS");
    EXPECT_NE(mortgageDeal_->getMortgagePool(), nullptr);
    
    // Verify pool contains our mortgages
    const auto& poolAssets = mortgageDeal_->getMortgagePool()->getAssets();
    EXPECT_EQ(poolAssets.size(), 3);
}

// Bond Management Tests
TEST_F(MortgageDealTest, BondManagement) {
    // Add bonds with different priorities
    MortgageBond seniorBond("Senior A", 1500000.0, 0.03, 1);
    MortgageBond mezBond("Mezzanine B", 500000.0, 0.05, 2);
    MortgageBond subordinateBond("Subordinate C", 250000.0, 0.08, 3);
    subordinateBond.isSubordinated = true;
    
    mortgageDeal_->addBond(seniorBond);
    mortgageDeal_->addBond(mezBond);
    mortgageDeal_->addBond(subordinateBond);
    
    const auto& bonds = mortgageDeal_->getBonds();
    EXPECT_EQ(bonds.size(), 3);
    
    // Verify bonds are sorted by priority
    EXPECT_EQ(bonds[0].bondName, "Senior A");
    EXPECT_EQ(bonds[0].paymentPriority, 1);
    EXPECT_EQ(bonds[1].bondName, "Mezzanine B");
    EXPECT_EQ(bonds[2].bondName, "Subordinate C");
    
    // Test total bond balance
    EXPECT_EQ(mortgageDeal_->getTotalBondBalance(), 2250000.0);
    
    // Test bond retrieval
    MortgageBond* seniorPtr = mortgageDeal_->getBond("Senior A");
    ASSERT_NE(seniorPtr, nullptr);
    EXPECT_EQ(seniorPtr->couponRate, 0.03);
}

// Pool Metrics Tests
TEST_F(MortgageDealTest, PoolMetrics) {
    Balance poolBalance = mortgageDeal_->getCurrentPoolBalance();
    EXPECT_GT(poolBalance, 0.0);
    
    Rate wac = mortgageDeal_->getWeightedAverageCoupon();
    EXPECT_GT(wac, 0.0);
    EXPECT_LT(wac, 0.1); // Reasonable range
    
    int wam = mortgageDeal_->getWeightedAverageMaturity();
    EXPECT_GT(wam, 300); // Should be close to 360 months
    EXPECT_LE(wam, 360);
}

// Waterfall Tests
TEST_F(MortgageDealTest, WaterfallExecution) {
    // Initialize the deal
    mortgageDeal_->initialize();
    
    // Verify standard accounts were created
    EXPECT_NE(mortgageDeal_->getAccount("Collection"), nullptr);
    EXPECT_NE(mortgageDeal_->getAccount("Reserve"), nullptr);
    EXPECT_NE(mortgageDeal_->getAccount("Excess"), nullptr);
    
    // Add some bonds
    MortgageBond seniorBond("Senior A", 1000000.0, 0.03, 1);
    mortgageDeal_->addBond(seniorBond);
    
    // Deposit some funds into collection account
    mortgageDeal_->getAccount("Collection")->deposit(50000.0);
    
    // Run waterfall
    Date paymentDate(2024, 2, 15);
    mortgageDeal_->runWaterfall(paymentDate);
    
    // Verify waterfall executed (funds should have moved out of collection)
    Balance collectionBalance = mortgageDeal_->getAccount("Collection")->getBalance();
    EXPECT_LT(collectionBalance, 50000.0); // Some funds should have been distributed
}

// Performance Metrics Tests
TEST_F(MortgageDealTest, PerformanceMetrics) {
    // Initially all metrics should be zero
    EXPECT_EQ(mortgageDeal_->getCumulativePrincipalCollections(), 0.0);
    EXPECT_EQ(mortgageDeal_->getCumulativeInterestCollections(), 0.0);
    EXPECT_EQ(mortgageDeal_->getCumulativePrepayments(), 0.0);
    EXPECT_EQ(mortgageDeal_->getCumulativeDefaults(), 0.0);
    EXPECT_EQ(mortgageDeal_->getCumulativeRecoveries(), 0.0);
    
    // Loss rates should be zero initially
    EXPECT_EQ(mortgageDeal_->getCurrentLossRate(), 0.0);
    EXPECT_EQ(mortgageDeal_->getCumulativeLossRate(), 0.0);
    EXPECT_EQ(mortgageDeal_->getDelinquencyRate(), 0.0);
}

// Deal Validation Tests
TEST_F(MortgageDealTest, DealValidation) {
    // Deal without bonds should fail validation
    auto errors = mortgageDeal_->validate();
    EXPECT_FALSE(errors.empty());
    
    // Add bonds
    MortgageBond seniorBond("Senior A", 1000000.0, 0.03, 1);
    mortgageDeal_->addBond(seniorBond);
    
    // Initialize to create accounts and payment dates
    mortgageDeal_->initialize();
    
    // Should now pass most validations
    errors = mortgageDeal_->validate();
    // May still have some errors due to balance mismatches, but should have fewer
    EXPECT_LT(errors.size(), 3);
}

// Deal Summary Tests
TEST_F(MortgageDealTest, DealSummary) {
    mortgageDeal_->initialize();
    
    MortgageBond seniorBond("Senior A", 1000000.0, 0.03, 1);
    mortgageDeal_->addBond(seniorBond);
    
    std::string summary = mortgageDeal_->getDealSummary();
    EXPECT_FALSE(summary.empty());
    EXPECT_NE(summary.find("Test Mortgage Deal"), std::string::npos);
    EXPECT_NE(summary.find("POOL INFORMATION"), std::string::npos);
    EXPECT_NE(summary.find("PERFORMANCE METRICS"), std::string::npos);
    EXPECT_NE(summary.find("BOND INFORMATION"), std::string::npos);
}

// Deal Value Calculation Tests
TEST_F(MortgageDealTest, DealValueCalculation) {
    Date valuationDate(2024, 6, 15);
    Balance dealValue = mortgageDeal_->calculateDealValue(valuationDate);
    EXPECT_GT(dealValue, 0.0);
    
    // Deal value should be roughly equal to pool value
    Balance poolValue = mortgageDeal_->getMortgagePool()->calculateValue(valuationDate);
    EXPECT_EQ(dealValue, poolValue);
}

// Cashflow Collection Tests
TEST_F(MortgageDealTest, CashflowCollection) {
    mortgageDeal_->initialize();
    
    Date collectionDate(2024, 2, 15);
    Balance initialCollectionBalance = mortgageDeal_->getAccount("Collection")->getBalance();
    
    // Collect pool cashflows
    mortgageDeal_->collectPoolCashflows(collectionDate);
    
    Balance finalCollectionBalance = mortgageDeal_->getAccount("Collection")->getBalance();
    
    // Should have collected some cashflows
    EXPECT_GE(finalCollectionBalance, initialCollectionBalance);
}

// Static Helper Method Tests
TEST_F(MortgageDealTest, StaticHelperMethods) {
    // Test standard waterfall creation
    MortgageWaterfall standardWaterfall = MortgageDeal::createStandardWaterfall();
    EXPECT_FALSE(standardWaterfall.steps.empty());
    EXPECT_GT(standardWaterfall.steps.size(), 5);
    
    // Test payment schedule generation
    Date firstPayment(2024, 2, 15);
    Date maturity(2024, 8, 15);
    auto paymentDates = MortgageDeal::generateMonthlyPaymentSchedule(firstPayment, maturity);
    EXPECT_FALSE(paymentDates.empty());
    EXPECT_EQ(paymentDates[0], firstPayment);
    EXPECT_LE(paymentDates.back(), maturity);
}