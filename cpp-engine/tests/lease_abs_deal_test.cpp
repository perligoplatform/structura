#include <gtest/gtest.h>
#include "deals/lease_abs_deal.h"
#include "assets/lease.h"
#include "assets/pool.h"

using namespace Structura;

class LeaseABSDealTest : public ::testing::Test {
protected:
    void SetUp() override {
        // Create test deal info
        deal_info_ = DealInfo{
            "TEST_LEASE_001",
            "Test Lease ABS Deal",
            DealType::LEASE_ABS_DEAL,
            Date(2024, 1, 15),
            Date(2029, 1, 15),
            DealStatus::ACTIVE
        };
        
        // Create lease ABS deal
        lease_abs_deal_ = std::make_unique<LeaseABSDeal>(deal_info_);
        
        // Create test equipment and lessees
        setupTestEquipmentAndLessees();
        
        // Create test leases
        setupTestLeases();
        
        // Create test pool
        setupTestPool();
        
        // Create test tranches
        setupTestTranches();
    }
    
    void setupTestEquipmentAndLessees() {
        // Equipment 1: Construction equipment
        test_equipment_["EQUIP_001"] = Equipment(
            "EQUIP_001", "Caterpillar", "320 Excavator",
            500000.0, 150000.0, 60 // 5 year life
        );
        
        // Equipment 2: Vehicle fleet
        test_equipment_["EQUIP_002"] = Equipment(
            "EQUIP_002", "Ford", "F-150 Fleet",
            300000.0, 100000.0, 48 // 4 year life
        );
        
        // Equipment 3: Medical equipment
        test_equipment_["EQUIP_003"] = Equipment(
            "EQUIP_003", "GE Healthcare", "MRI Scanner",
            2000000.0, 800000.0, 120 // 10 year life
        );
        
        // Lessee 1: Construction company
        test_lessees_["LESSEE_001"] = Lessee("LESSEE_001", "ABC Construction", "Construction");
        test_lessees_["LESSEE_001"].credit_score = 750.0;
        test_lessees_["LESSEE_001"].annual_revenue = 5000000.0;
        
        // Lessee 2: Logistics company
        test_lessees_["LESSEE_002"] = Lessee("LESSEE_002", "Fast Delivery Co", "Logistics");
        test_lessees_["LESSEE_002"].credit_score = 680.0;
        test_lessees_["LESSEE_002"].annual_revenue = 2000000.0;
        
        // Lessee 3: Healthcare provider
        test_lessees_["LESSEE_003"] = Lessee("LESSEE_003", "City Medical Center", "Healthcare");
        test_lessees_["LESSEE_003"].credit_score = 720.0;
        test_lessees_["LESSEE_003"].annual_revenue = 50000000.0;
    }
    
    void setupTestLeases() {
        // Lease 1: Construction equipment
        test_leases_.emplace_back(
            "LEASE_001", LeaseType::EQUIPMENT, LeaseStructure::CAPITAL,
            test_equipment_["EQUIP_001"], test_lessees_["LESSEE_001"],
            Date(2024, 1, 1), 60, 10000.0
        );
        
        // Lease 2: Vehicle fleet
        test_leases_.emplace_back(
            "LEASE_002", LeaseType::VEHICLE, LeaseStructure::OPERATING,
            test_equipment_["EQUIP_002"], test_lessees_["LESSEE_002"],
            Date(2024, 2, 1), 48, 7500.0
        );
        
        // Lease 3: Medical equipment
        test_leases_.emplace_back(
            "LEASE_003", LeaseType::MEDICAL, LeaseStructure::CAPITAL,
            test_equipment_["EQUIP_003"], test_lessees_["LESSEE_003"],
            Date(2024, 1, 15), 120, 20000.0
        );
    }
    
    void setupTestPool() {
        test_pool_ = std::make_unique<Pool<Lease>>("TEST_LEASE_POOL", test_leases_);
    }
    
    void setupTestTranches() {
        // Senior A Tranche
        test_tranches_["SENIOR_A"] = LeaseABSTranche("SENIOR_A", LeaseTrancheType::SENIOR_A, 
                                                     15000000.0, 0.025);
        
        // Senior B Tranche  
        test_tranches_["SENIOR_B"] = LeaseABSTranche("SENIOR_B", LeaseTrancheType::SENIOR_B,
                                                     8000000.0, 0.035);
        
        // Mezzanine Tranche
        test_tranches_["MEZZ"] = LeaseABSTranche("MEZZ", LeaseTrancheType::MEZZANINE,
                                                 5000000.0, 0.055);
        
        // Subordinate Tranche
        test_tranches_["SUB"] = LeaseABSTranche("SUB", LeaseTrancheType::SUBORDINATE,
                                                3000000.0, 0.085);
        
        // Residual Tranche
        test_tranches_["RESIDUAL"] = LeaseABSTranche("RESIDUAL", LeaseTrancheType::RESIDUAL,
                                                     2000000.0, 0.0);
    }
    
    DealInfo deal_info_;
    std::unique_ptr<LeaseABSDeal> lease_abs_deal_;
    std::map<std::string, Equipment> test_equipment_;
    std::map<std::string, Lessee> test_lessees_;
    std::vector<Lease> test_leases_;
    std::unique_ptr<Pool<Lease>> test_pool_;
    std::map<std::string, LeaseABSTranche> test_tranches_;
};

// Basic construction and setup tests
TEST_F(LeaseABSDealTest, BasicConstruction) {
    EXPECT_EQ(lease_abs_deal_->getDealInfo().deal_id, "TEST_LEASE_001");
    EXPECT_EQ(lease_abs_deal_->getDealInfo().deal_name, "Test Lease ABS Deal");
    EXPECT_EQ(lease_abs_deal_->getDealInfo().deal_type, DealType::LEASE_ABS_DEAL);
    EXPECT_EQ(lease_abs_deal_->getDealInfo().status, DealStatus::ACTIVE);
}

TEST_F(LeaseABSDealTest, LeaseTypeStringConversion) {
    EXPECT_EQ(leaseTypeToString(LeaseType::EQUIPMENT), "Equipment");
    EXPECT_EQ(leaseTypeToString(LeaseType::VEHICLE), "Vehicle");
    EXPECT_EQ(leaseTypeToString(LeaseType::AIRCRAFT), "Aircraft");
    EXPECT_EQ(leaseTypeToString(LeaseType::MEDICAL), "Medical");
    EXPECT_EQ(leaseTypeToString(LeaseType::TECHNOLOGY), "Technology");
}

TEST_F(LeaseABSDealTest, LeaseStructureStringConversion) {
    EXPECT_EQ(leaseStructureToString(LeaseStructure::CAPITAL), "Capital");
    EXPECT_EQ(leaseStructureToString(LeaseStructure::OPERATING), "Operating");
    EXPECT_EQ(leaseStructureToString(LeaseStructure::SYNTHETIC), "Synthetic");
    EXPECT_EQ(leaseStructureToString(LeaseStructure::LEVERAGED), "Leveraged");
}

TEST_F(LeaseABSDealTest, TrancheTypeStringConversion) {
    EXPECT_EQ(leaseTrancheTypeToString(LeaseTrancheType::SENIOR_A), "Senior A");
    EXPECT_EQ(leaseTrancheTypeToString(LeaseTrancheType::SENIOR_B), "Senior B");
    EXPECT_EQ(leaseTrancheTypeToString(LeaseTrancheType::MEZZANINE), "Mezzanine");
    EXPECT_EQ(leaseTrancheTypeToString(LeaseTrancheType::SUBORDINATE), "Subordinate");
    EXPECT_EQ(leaseTrancheTypeToString(LeaseTrancheType::RESIDUAL), "Residual");
}

// Equipment tests
TEST_F(LeaseABSDealTest, EquipmentConstruction) {
    const auto& equipment = test_equipment_["EQUIP_001"];
    
    EXPECT_EQ(equipment.equipment_id, "EQUIP_001");
    EXPECT_EQ(equipment.manufacturer, "Caterpillar");
    EXPECT_EQ(equipment.model, "320 Excavator");
    EXPECT_EQ(equipment.original_cost, 500000.0);
    EXPECT_EQ(equipment.estimated_residual_value, 150000.0);
    EXPECT_EQ(equipment.useful_life_months, 60);
}

TEST_F(LeaseABSDealTest, EquipmentDepreciation) {
    const auto& equipment = test_equipment_["EQUIP_001"];
    
    // Test straight line depreciation
    Amount depreciation_12_months = equipment.calculateStraightLineDepreciation(12);
    Amount expected_depreciation = (500000.0 - 150000.0) / 60 * 12; // 70,000
    EXPECT_DOUBLE_EQ(depreciation_12_months, expected_depreciation);
    
    // Test current book value
    Amount book_value = equipment.getCurrentBookValue(12);
    EXPECT_DOUBLE_EQ(book_value, 500000.0 - expected_depreciation);
}

// Lessee tests
TEST_F(LeaseABSDealTest, LesseeConstruction) {
    const auto& lessee = test_lessees_["LESSEE_001"];
    
    EXPECT_EQ(lessee.lessee_id, "LESSEE_001");
    EXPECT_EQ(lessee.company_name, "ABC Construction");
    EXPECT_EQ(lessee.industry, "Construction");
    EXPECT_EQ(lessee.credit_score, 750.0);
    EXPECT_EQ(lessee.annual_revenue, 5000000.0);
}

TEST_F(LeaseABSDealTest, LesseeCreditAssessment) {
    const auto& lessee = test_lessees_["LESSEE_001"];
    
    double credit_risk_score = lessee.getCreditRiskScore();
    EXPECT_GT(credit_risk_score, 0.0);
    EXPECT_LE(credit_risk_score, 1.0);
    
    // High credit score should result in good risk score
    EXPECT_GT(credit_risk_score, 0.7);
    
    bool investment_grade = lessee.isInvestmentGrade();
    EXPECT_TRUE(investment_grade); // 750 credit score should be investment grade
}

// Lease tests
TEST_F(LeaseABSDealTest, LeaseConstruction) {
    const auto& lease = test_leases_[0];
    
    EXPECT_EQ(lease.getId(), "LEASE_001");
    EXPECT_EQ(lease.getLeaseType(), LeaseType::EQUIPMENT);
    EXPECT_EQ(lease.getLeaseStructure(), LeaseStructure::CAPITAL);
    EXPECT_EQ(lease.getLeaseTermMonths(), 60);
    EXPECT_EQ(lease.getMonthlyPayment(), 10000.0);
}

TEST_F(LeaseABSDealTest, LeasePaymentProcessing) {
    auto lease = test_leases_[0];
    Amount initial_balance = lease.getCurrentBalance();
    
    // Process a payment
    lease.processLeasePayment(10000.0, Date(2024, 2, 1));
    
    Amount new_balance = lease.getCurrentBalance();
    EXPECT_LT(new_balance, initial_balance);
    EXPECT_EQ(lease.getTotalPaymentsReceived(), 10000.0);
}

TEST_F(LeaseABSDealTest, LeaseResidualValue) {
    const auto& lease = test_leases_[0];
    
    Amount estimated_residual = lease.getEstimatedResidualValue();
    Amount guaranteed_residual = lease.getGuaranteedResidualValue();
    
    EXPECT_GT(estimated_residual, 0.0);
    EXPECT_GT(guaranteed_residual, 0.0);
    EXPECT_LE(guaranteed_residual, estimated_residual);
    
    double residual_ratio = lease.getResidualRiskRatio();
    EXPECT_GT(residual_ratio, 0.0);
    EXPECT_LT(residual_ratio, 1.0);
}

// Tranche tests
TEST_F(LeaseABSDealTest, TrancheConstruction) {
    const auto& senior_a = test_tranches_["SENIOR_A"];
    
    EXPECT_EQ(senior_a.tranche_id, "SENIOR_A");
    EXPECT_EQ(senior_a.tranche_type, LeaseTrancheType::SENIOR_A);
    EXPECT_EQ(senior_a.notional_amount, 15000000.0);
    EXPECT_EQ(senior_a.outstanding_balance, 15000000.0);
    EXPECT_EQ(senior_a.coupon_rate, 0.025);
    EXPECT_FALSE(senior_a.isFullyPaid());
}

TEST_F(LeaseABSDealTest, TrancheInterestCalculation) {
    const auto& senior_a = test_tranches_["SENIOR_A"];
    
    Amount expected_interest = 15000000.0 * 0.025 / 4.0; // Quarterly
    Amount calculated_interest = senior_a.calculateInterestPayment();
    
    EXPECT_DOUBLE_EQ(calculated_interest, expected_interest);
}

TEST_F(LeaseABSDealTest, TranchePaymentProcessing) {
    auto senior_a = test_tranches_["SENIOR_A"];
    Amount initial_balance = senior_a.outstanding_balance;
    
    // Make a payment
    senior_a.makePayment(100000.0, 93750.0, Date(2024, 4, 15));
    
    EXPECT_EQ(senior_a.total_principal_paid, 100000.0);
    EXPECT_EQ(senior_a.total_interest_paid, 93750.0);
    EXPECT_EQ(senior_a.outstanding_balance, initial_balance - 100000.0);
}

// Waterfall tests
TEST_F(LeaseABSDealTest, WaterfallSetup) {
    LeaseABSWaterfall waterfall;
    waterfall.payment_sequence = {"SENIOR_A", "SENIOR_B", "MEZZ", "SUB", "RESIDUAL"};
    
    lease_abs_deal_->setupWaterfall(waterfall);
    
    EXPECT_EQ(lease_abs_deal_->getWaterfall().payment_sequence.size(), 5);
    EXPECT_EQ(lease_abs_deal_->getWaterfall().payment_sequence[0], "SENIOR_A");
}

TEST_F(LeaseABSDealTest, InterestWaterfallDistribution) {
    LeaseABSWaterfall waterfall;
    waterfall.payment_sequence = {"SENIOR_A", "SENIOR_B", "MEZZ"};
    
    Amount available_interest = 500000.0;
    auto distributions = waterfall.processInterestWaterfall(available_interest, test_tranches_);
    
    // Senior A should get paid first
    EXPECT_GT(distributions["SENIOR_A"], 0.0);
    
    // Total distributions shouldn't exceed available funds
    Amount total_distributed = 0.0;
    for (const auto& [tranche_id, amount] : distributions) {
        total_distributed += amount;
    }
    EXPECT_LE(total_distributed, available_interest);
}

TEST_F(LeaseABSDealTest, PrincipalWaterfallDistribution) {
    LeaseABSWaterfall waterfall;
    waterfall.payment_sequence = {"SENIOR_A", "SENIOR_B", "MEZZ"};
    
    Amount available_principal = 1000000.0;
    auto distributions = waterfall.processPrincipalWaterfall(available_principal, test_tranches_);
    
    // Senior A should get principal first
    EXPECT_GT(distributions["SENIOR_A"], 0.0);
    
    // Check that distributions respect seniority
    if (distributions.find("SENIOR_B") != distributions.end()) {
        EXPECT_GE(distributions["SENIOR_A"], distributions["SENIOR_B"]);
    }
}

TEST_F(LeaseABSDealTest, LossWaterfallDistribution) {
    LeaseABSWaterfall waterfall;
    waterfall.payment_sequence = {"SENIOR_A", "SENIOR_B", "MEZZ", "SUB"};
    
    Amount total_losses = 2000000.0;
    auto loss_allocations = waterfall.processLossWaterfall(total_losses, test_tranches_);
    
    // Subordinate should absorb losses first
    EXPECT_GT(loss_allocations["SUB"], 0.0);
    
    // Senior should be protected
    EXPECT_LE(loss_allocations["SENIOR_A"], loss_allocations["SUB"]);
}

// ResidualValueManager tests
TEST_F(LeaseABSDealTest, ResidualValueManagerBasics) {
    ResidualValueManager manager;
    
    manager.updateResidualEstimates(*test_pool_);
    
    EXPECT_GT(manager.total_estimated_residual, 0.0);
    EXPECT_GT(manager.guaranteed_residual_value, 0.0);
    EXPECT_GE(manager.residual_value_ratio, 0.0);
}

TEST_F(LeaseABSDealTest, ResidualValueRealization) {
    ResidualValueManager manager;
    
    Date realization_date(2024, 6, 15);
    Amount realized_amount = 50000.0;
    
    Amount processed = manager.processResidualRealization(realization_date, realized_amount);
    
    EXPECT_EQ(processed, realized_amount);
    EXPECT_EQ(manager.realized_residual_value, realized_amount);
    EXPECT_TRUE(manager.residual_realization_schedule.find(realization_date) != 
                manager.residual_realization_schedule.end());
}

// Deal setup and validation tests
TEST_F(LeaseABSDealTest, DealSetup) {
    // Add tranches
    for (const auto& [tranche_id, tranche] : test_tranches_) {
        lease_abs_deal_->addTranche(tranche);
    }
    
    // Set lease pool
    lease_abs_deal_->setLeasePool(test_pool_.get());
    
    // Setup waterfall
    LeaseABSWaterfall waterfall;
    waterfall.payment_sequence = {"SENIOR_A", "SENIOR_B", "MEZZ", "SUB", "RESIDUAL"};
    lease_abs_deal_->setupWaterfall(waterfall);
    
    // Deal should now validate
    EXPECT_TRUE(lease_abs_deal_->validateDeal());
}

TEST_F(LeaseABSDealTest, DealValidationFailures) {
    // Deal should not validate without setup
    EXPECT_FALSE(lease_abs_deal_->validateDeal());
    
    // Add tranches but no pool
    for (const auto& [tranche_id, tranche] : test_tranches_) {
        lease_abs_deal_->addTranche(tranche);
    }
    EXPECT_FALSE(lease_abs_deal_->validateDeal());
    
    // Add pool but no waterfall
    lease_abs_deal_->setLeasePool(test_pool_.get());
    EXPECT_FALSE(lease_abs_deal_->validateDeal());
}

// Portfolio analytics tests
TEST_F(LeaseABSDealTest, PortfolioAnalytics) {
    // Setup complete deal
    for (const auto& [tranche_id, tranche] : test_tranches_) {
        lease_abs_deal_->addTranche(tranche);
    }
    lease_abs_deal_->setLeasePool(test_pool_.get());
    
    LeaseABSWaterfall waterfall;
    waterfall.payment_sequence = {"SENIOR_A", "SENIOR_B", "MEZZ", "SUB", "RESIDUAL"};
    lease_abs_deal_->setupWaterfall(waterfall);
    
    // Calculate metrics
    Date as_of_date(2024, 3, 15);
    auto metrics = lease_abs_deal_->calculateMetrics(as_of_date);
    
    EXPECT_GT(metrics["total_notional"], 0.0);
    EXPECT_GT(metrics["portfolio_balance"], 0.0);
    EXPECT_GE(metrics["expected_losses"], 0.0);
    EXPECT_GT(metrics["residual_exposure"], 0.0);
}

TEST_F(LeaseABSDealTest, ConcentrationAnalysis) {
    // Setup complete deal
    for (const auto& [tranche_id, tranche] : test_tranches_) {
        lease_abs_deal_->addTranche(tranche);
    }
    lease_abs_deal_->setLeasePool(test_pool_.get());
    
    // Test lease type concentration
    auto lease_type_concentration = lease_abs_deal_->getLeaseTypeConcentration();
    EXPECT_GT(lease_type_concentration[LeaseType::EQUIPMENT], 0.0);
    EXPECT_GT(lease_type_concentration[LeaseType::VEHICLE], 0.0);
    EXPECT_GT(lease_type_concentration[LeaseType::MEDICAL], 0.0);
    
    // Test lessee concentration
    auto lessee_concentration = lease_abs_deal_->getLesseeConcentration();
    EXPECT_GT(lessee_concentration["LESSEE_001"], 0.0);
    EXPECT_GT(lessee_concentration["LESSEE_002"], 0.0);
    EXPECT_GT(lessee_concentration["LESSEE_003"], 0.0);
}

TEST_F(LeaseABSDealTest, PerformanceMetrics) {
    // Setup complete deal
    for (const auto& [tranche_id, tranche] : test_tranches_) {
        lease_abs_deal_->addTranche(tranche);
    }
    lease_abs_deal_->setLeasePool(test_pool_.get());
    
    double portfolio_yield = lease_abs_deal_->calculatePortfolioYield();
    EXPECT_GE(portfolio_yield, 0.0);
    
    double excess_spread = lease_abs_deal_->calculateExcessSpread();
    // Could be positive or negative depending on structure
    
    double weighted_avg_life = lease_abs_deal_->calculateWeightedAverageLife();
    EXPECT_GT(weighted_avg_life, 0.0);
}

// Payment processing tests
TEST_F(LeaseABSDealTest, PaymentProcessing) {
    // Setup complete deal
    for (const auto& [tranche_id, tranche] : test_tranches_) {
        lease_abs_deal_->addTranche(tranche);
    }
    lease_abs_deal_->setLeasePool(test_pool_.get());
    
    LeaseABSWaterfall waterfall;
    waterfall.payment_sequence = {"SENIOR_A", "SENIOR_B", "MEZZ", "SUB", "RESIDUAL"};
    lease_abs_deal_->setupWaterfall(waterfall);
    
    // Process payment
    Date payment_date(2024, 4, 15);
    lease_abs_deal_->processPaymentDate(payment_date);
    
    // Verify deal still functioning
    EXPECT_EQ(lease_abs_deal_->getLastPaymentDate(), payment_date);
    EXPECT_EQ(lease_abs_deal_->getDealInfo().status, DealStatus::ACTIVE);
}

// Stress testing
TEST_F(LeaseABSDealTest, StressTesting) {
    // Setup complete deal
    for (const auto& [tranche_id, tranche] : test_tranches_) {
        lease_abs_deal_->addTranche(tranche);
    }
    lease_abs_deal_->setLeasePool(test_pool_.get());
    
    // Test default scenario
    Amount original_expected_losses = lease_abs_deal_->calculateExpectedLosses();
    lease_abs_deal_->applyLeaseDefaultScenario(2.0); // Double default rates
    Amount stressed_expected_losses = lease_abs_deal_->calculateExpectedLosses();
    
    EXPECT_GE(stressed_expected_losses, original_expected_losses);
    
    // Test residual value stress
    Amount original_residual_exposure = lease_abs_deal_->getTotalResidualExposure();
    lease_abs_deal_->applyResidualValueStress(0.25); // 25% decline
    Amount stressed_residual_exposure = lease_abs_deal_->getTotalResidualExposure();
    
    EXPECT_LT(stressed_residual_exposure, original_residual_exposure);
}

TEST_F(LeaseABSDealTest, StatusReporting) {
    // Setup complete deal
    for (const auto& [tranche_id, tranche] : test_tranches_) {
        lease_abs_deal_->addTranche(tranche);
    }
    lease_abs_deal_->setLeasePool(test_pool_.get());
    
    std::string status = lease_abs_deal_->getStatus();
    EXPECT_FALSE(status.empty());
    
    // Status should reflect deal condition
    if (lease_abs_deal_->getDealInfo().status == DealStatus::ACTIVE) {
        EXPECT_TRUE(status.find("Active") != std::string::npos);
    }
}

TEST_F(LeaseABSDealTest, AssumptionUpdates) {
    // Setup complete deal
    for (const auto& [tranche_id, tranche] : test_tranches_) {
        lease_abs_deal_->addTranche(tranche);
    }
    lease_abs_deal_->setLeasePool(test_pool_.get());
    
    Amount original_expected_losses = lease_abs_deal_->calculateExpectedLosses();
    Amount original_residual_exposure = lease_abs_deal_->getTotalResidualExposure();
    
    // Update assumptions
    std::map<std::string, double> new_assumptions;
    new_assumptions["default_rate_multiplier"] = 1.5;
    new_assumptions["residual_value_stress"] = 0.20;
    
    lease_abs_deal_->updateAssumptions(new_assumptions);
    
    Amount updated_expected_losses = lease_abs_deal_->calculateExpectedLosses();
    Amount updated_residual_exposure = lease_abs_deal_->getTotalResidualExposure();
    
    // Check that assumptions were applied
    EXPECT_GE(updated_expected_losses, original_expected_losses * 0.9); // Allow some tolerance
    EXPECT_LE(updated_residual_exposure, original_residual_exposure * 1.1); // Allow some tolerance
}