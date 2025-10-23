#include <gtest/gtest.h>
#include "assets/auto_loan.h"
#include "core/financial_types.h"

using namespace Structura;

class AutoLoanTest : public ::testing::Test {
protected:
    void SetUp() override {
        // Create test vehicle
        test_vehicle = std::make_unique<Vehicle>("1HGBH41JXMN109186", "Honda", "Civic", 2022, 25000.0, 23000.0);
        
        // Create test borrower
        test_borrower = std::make_unique<Obligor>("BORROWER001");
        
        // Create test auto loan
        Date origination_date = Date(); // Would use actual date
        test_loan = std::make_unique<AutoLoan>(
            "LOAN001", 
            AutoLoanType::NEW_VEHICLE,
            *test_vehicle,
            *test_borrower,
            20000.0,  // Loan amount
            0.05,     // 5% interest rate
            60,       // 5-year term
            origination_date
        );
    }

    std::unique_ptr<Vehicle> test_vehicle;
    std::unique_ptr<Obligor> test_borrower;
    std::unique_ptr<AutoLoan> test_loan;
};

TEST_F(AutoLoanTest, ConstructorInitialization) {
    EXPECT_EQ(test_loan->getLoanId(), "LOAN001");
    EXPECT_EQ(test_loan->getLoanType(), AutoLoanType::NEW_VEHICLE);
    EXPECT_EQ(test_loan->getOriginalBalance(), 20000.0);
    EXPECT_EQ(test_loan->getCurrentBalance(), 20000.0);
    EXPECT_EQ(test_loan->getCurrentRate(), 0.05);
    EXPECT_EQ(test_loan->getOriginalTermMonths(), 60);
    EXPECT_EQ(test_loan->getRemainingTerms(), 60);
    EXPECT_EQ(test_loan->getStatus(), Status::Current);
}

TEST_F(AutoLoanTest, VehicleInformation) {
    const Vehicle& vehicle = test_loan->getVehicle();
    EXPECT_EQ(vehicle.vin, "1HGBH41JXMN109186");
    EXPECT_EQ(vehicle.make, "Honda");
    EXPECT_EQ(vehicle.model, "Civic");
    EXPECT_EQ(vehicle.year, 2022);
    EXPECT_EQ(vehicle.original_msrp, 25000.0);
    EXPECT_EQ(vehicle.purchase_price, 23000.0);
}

TEST_F(AutoLoanTest, MonthlyPaymentCalculation) {
    Amount monthly_payment = test_loan->getMonthlyPayment();
    
    // Expected payment for $20,000 at 5% for 60 months
    // Using formula: P * [r(1+r)^n] / [(1+r)^n - 1]
    double monthly_rate = 0.05 / 12.0;
    double factor = std::pow(1.0 + monthly_rate, 60);
    Amount expected_payment = 20000.0 * (monthly_rate * factor) / (factor - 1.0);
    
    EXPECT_NEAR(monthly_payment, expected_payment, 0.01);
    EXPECT_GT(monthly_payment, 350.0);
    EXPECT_LT(monthly_payment, 400.0);
}

TEST_F(AutoLoanTest, LTVCalculation) {
    // LTV should be 20000 / 23000 = ~0.87 (NEAR_PRIME category: 80% < LTV <= 90%)
    double ltv = test_loan->getCurrentLTV();
    EXPECT_NEAR(ltv, 20000.0 / 23000.0, 0.01);
    EXPECT_EQ(test_loan->getLTVCategory(), LTVCategory::NEAR_PRIME);
}

TEST_F(AutoLoanTest, PaymentProcessing) {
    Amount initial_balance = test_loan->getCurrentBalance();
    Amount monthly_payment = test_loan->getMonthlyPayment();
    Date payment_date = Date(); // Would use actual date
    
    // Make a payment
    test_loan->makePayment(300.0, 50.0, payment_date); // $300 principal, $50 interest
    
    EXPECT_LT(test_loan->getCurrentBalance(), initial_balance);
    EXPECT_EQ(test_loan->getPaymentsMade(), 1);
    EXPECT_EQ(test_loan->getTotalPrincipalPaid(), 300.0);
    EXPECT_EQ(test_loan->getTotalInterestPaid(), 50.0);
}

TEST_F(AutoLoanTest, VehicleDepreciation) {
    // Test straight-line depreciation
    Amount depreciation_12_months = test_vehicle->calculateStraightLineDepreciation(12);
    EXPECT_GT(depreciation_12_months, 0.0);
    
    // Test accelerated depreciation
    Amount accelerated_depreciation = test_vehicle->calculateAcceleratedDepreciation(12, 2.0);
    EXPECT_GT(accelerated_depreciation, depreciation_12_months);
}

TEST_F(AutoLoanTest, EquityCalculation) {
    // Initial equity = vehicle value - loan balance
    Amount initial_equity = test_loan->calculateEquityPosition();
    EXPECT_EQ(initial_equity, 23000.0 - 20000.0); // $3,000 equity
    
    // After depreciation, equity should change
    test_loan->updateVehicleValue(22000.0, Date());
    Amount updated_equity = test_loan->calculateEquityPosition();
    EXPECT_EQ(updated_equity, 22000.0 - 20000.0); // $2,000 equity
}

TEST_F(AutoLoanTest, DefaultHandling) {
    EXPECT_FALSE(test_loan->isInDefault());
    
    Date default_date = Date();
    test_loan->applyDefault(default_date);
    
    EXPECT_TRUE(test_loan->isInDefault());
    EXPECT_EQ(test_loan->getStatus(), Status::Defaulted);
}

TEST_F(AutoLoanTest, PrepaymentHandling) {
    Amount initial_balance = test_loan->getCurrentBalance();
    
    // Partial prepayment
    test_loan->applyPrepayment(5000.0, Date());
    EXPECT_EQ(test_loan->getCurrentBalance(), initial_balance - 5000.0);
    EXPECT_EQ(test_loan->getStatus(), Status::Current);
    
    // Full prepayment
    test_loan->applyPrepayment(test_loan->getCurrentBalance(), Date());
    EXPECT_EQ(test_loan->getCurrentBalance(), 0.0);
    EXPECT_EQ(test_loan->getStatus(), Status::PaidOff);
}

TEST_F(AutoLoanTest, RiskAssessment) {
    // Initial risk assessment
    double initial_pd = test_loan->calculateProbabilityOfDefault();
    EXPECT_GT(initial_pd, 0.0);
    EXPECT_LT(initial_pd, 1.0);
    
    double risk_score = test_loan->getRiskScore();
    EXPECT_GE(risk_score, 0.0);
    EXPECT_LE(risk_score, 1000.0);
    
    bool is_high_risk = test_loan->isHighRisk();
    EXPECT_FALSE(is_high_risk); // Should not be high risk initially
}

TEST_F(AutoLoanTest, PaymentSchedule) {
    std::vector<Date> schedule = test_loan->getPaymentSchedule(12);
    EXPECT_EQ(schedule.size(), 12);
    
    Date next_payment = test_loan->getNextPaymentDate();
    EXPECT_NE(next_payment, Date()); // Check that it's not a default-constructed date
}

TEST_F(AutoLoanTest, RecoveryCalculations) {
    Amount recovery_value = test_loan->estimateRecoveryValue();
    EXPECT_GT(recovery_value, 0.0);
    EXPECT_LT(recovery_value, test_vehicle->current_value);
    
    Amount expected_loss = test_loan->calculateExpectedLoss();
    EXPECT_GE(expected_loss, 0.0);
}

TEST_F(AutoLoanTest, MileageUpdates) {
    // Update mileage and verify value impact
    int initial_mileage = test_vehicle->mileage;
    test_loan->updateMileage(50000, Date()); // High mileage
    
    const Vehicle& updated_vehicle = test_loan->getVehicle();
    EXPECT_EQ(updated_vehicle.mileage, 50000);
    EXPECT_LT(updated_vehicle.current_value, test_vehicle->current_value); // Value should decrease
}

TEST_F(AutoLoanTest, AutoLoanTypeConversion) {
    EXPECT_EQ(autoLoanTypeToString(AutoLoanType::NEW_VEHICLE), "New Vehicle");
    EXPECT_EQ(autoLoanTypeToString(AutoLoanType::USED_VEHICLE), "Used Vehicle");
    EXPECT_EQ(autoLoanTypeToString(AutoLoanType::REFINANCE), "Refinance");
    EXPECT_EQ(autoLoanTypeToString(AutoLoanType::LEASE_BUYOUT), "Lease Buyout");
    EXPECT_EQ(autoLoanTypeToString(AutoLoanType::PRIVATE_PARTY), "Private Party");
}

TEST_F(AutoLoanTest, LTVCategoryCalculation) {
    EXPECT_EQ(calculateLTVCategory(14000.0, 20000.0), LTVCategory::SUPER_PRIME); // 70% LTV
    EXPECT_EQ(calculateLTVCategory(16000.0, 20000.0), LTVCategory::PRIME);       // 80% LTV
    EXPECT_EQ(calculateLTVCategory(18000.0, 20000.0), LTVCategory::NEAR_PRIME);  // 90% LTV
    EXPECT_EQ(calculateLTVCategory(20000.0, 20000.0), LTVCategory::SUB_PRIME);   // 100% LTV
    EXPECT_EQ(calculateLTVCategory(22000.0, 20000.0), LTVCategory::HIGH_LTV);    // 110% LTV
}

TEST_F(AutoLoanTest, ResetToOriginal) {
    // Modify the loan
    test_loan->makePayment(100.0, 50.0, Date());
    test_loan->applyDefault(Date());
    
    EXPECT_NE(test_loan->getCurrentBalance(), test_loan->getOriginalBalance());
    EXPECT_EQ(test_loan->getStatus(), Status::Defaulted);
    
    // Reset to original
    test_loan->resetToOriginal();
    
    EXPECT_EQ(test_loan->getCurrentBalance(), test_loan->getOriginalBalance());
    EXPECT_EQ(test_loan->getStatus(), Status::Current);
    EXPECT_EQ(test_loan->getPaymentsMade(), 0);
    EXPECT_EQ(test_loan->getTotalPrincipalPaid(), 0.0);
    EXPECT_EQ(test_loan->getTotalInterestPaid(), 0.0);
    EXPECT_FALSE(test_loan->isInDefault());
}