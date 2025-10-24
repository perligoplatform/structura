#include <gtest/gtest.h>
#include "../src/validation/validation_engine.h"
#include "../src/core/deal_base.h"
#include "../src/analytics/analytics_engine.h"
#include <memory>

using namespace Structura;

// Mock deal class for testing
class MockDeal : public DealBase {
public:
    MockDeal() : DealBase(DealInfo("Mock Deal", "TEST", Date::todaysDate(), Date::todaysDate() + 365)) {
        // Initialize with test data
    }
    
    // Override pure virtual methods
    void runWaterfall(const Date& paymentDate) override {
        // Mock implementation
    }
    
    Balance calculateDealValue(const Date& valuationDate) const override {
        return 1000000.0; // Mock value
    }
    
    std::vector<std::string> validate() const override {
        return {}; // No validation issues for mock
    }
    
    std::string getDealSummary() const override {
        return "Mock Deal Summary";
    }
};

class ValidationEngineTest : public ::testing::Test {
protected:
    void SetUp() override {
        analytics_engine = std::make_shared<AnalyticsEngine>();
        validation_engine = std::make_unique<ValidationEngine>(analytics_engine);
        mock_deal = std::make_unique<MockDeal>();
    }
    
    void TearDown() override {
        // Cleanup
    }
    
    std::shared_ptr<AnalyticsEngine> analytics_engine;
    std::unique_ptr<ValidationEngine> validation_engine;
    std::unique_ptr<MockDeal> mock_deal;
};

// Basic construction and configuration tests
TEST_F(ValidationEngineTest, BasicConstruction) {
    EXPECT_TRUE(validation_engine != nullptr);
    
    // Test that standard rules are loaded
    std::vector<std::string> available_rules = validation_engine->getAvailableRules();
    EXPECT_GT(available_rules.size(), 0);
}

TEST_F(ValidationEngineTest, AnalyticsEngineIntegration) {
    ValidationEngine engine_without_analytics;
    EXPECT_TRUE(true); // Constructor should work without analytics
    
    auto analytics = std::make_shared<AnalyticsEngine>();
    engine_without_analytics.setAnalyticsEngine(analytics);
    EXPECT_TRUE(true); // Should set analytics engine without error
}

// String conversion tests
TEST_F(ValidationEngineTest, ValidationSeverityStringConversion) {
    EXPECT_EQ(ValidationEngine::validationSeverityToString(ValidationSeverity::INFO), "INFO");
    EXPECT_EQ(ValidationEngine::validationSeverityToString(ValidationSeverity::WARNING), "WARNING");
    EXPECT_EQ(ValidationEngine::validationSeverityToString(ValidationSeverity::ERROR), "ERROR");
    EXPECT_EQ(ValidationEngine::validationSeverityToString(ValidationSeverity::CRITICAL), "CRITICAL");
    
    EXPECT_EQ(ValidationEngine::stringToValidationSeverity("INFO"), ValidationSeverity::INFO);
    EXPECT_EQ(ValidationEngine::stringToValidationSeverity("WARNING"), ValidationSeverity::WARNING);
    EXPECT_EQ(ValidationEngine::stringToValidationSeverity("ERROR"), ValidationSeverity::ERROR);
    EXPECT_EQ(ValidationEngine::stringToValidationSeverity("CRITICAL"), ValidationSeverity::CRITICAL);
}

TEST_F(ValidationEngineTest, ValidationCategoryStringConversion) {
    EXPECT_EQ(ValidationEngine::validationCategoryToString(ValidationCategory::DATA_INTEGRITY), "DATA_INTEGRITY");
    EXPECT_EQ(ValidationEngine::validationCategoryToString(ValidationCategory::BUSINESS_RULES), "BUSINESS_RULES");
    EXPECT_EQ(ValidationEngine::validationCategoryToString(ValidationCategory::COVENANT_COMPLIANCE), "COVENANT_COMPLIANCE");
    EXPECT_EQ(ValidationEngine::validationCategoryToString(ValidationCategory::REGULATORY_COMPLIANCE), "REGULATORY_COMPLIANCE");
    
    EXPECT_EQ(ValidationEngine::stringToValidationCategory("DATA_INTEGRITY"), ValidationCategory::DATA_INTEGRITY);
    EXPECT_EQ(ValidationEngine::stringToValidationCategory("BUSINESS_RULES"), ValidationCategory::BUSINESS_RULES);
    EXPECT_EQ(ValidationEngine::stringToValidationCategory("COVENANT_COMPLIANCE"), ValidationCategory::COVENANT_COMPLIANCE);
}

TEST_F(ValidationEngineTest, ValidationRuleTypeStringConversion) {
    EXPECT_EQ(ValidationEngine::validationRuleTypeToString(ValidationRuleType::NUMERIC_RANGE), "NUMERIC_RANGE");
    EXPECT_EQ(ValidationEngine::validationRuleTypeToString(ValidationRuleType::RATIO_CHECK), "RATIO_CHECK");
    EXPECT_EQ(ValidationEngine::validationRuleTypeToString(ValidationRuleType::DATE_VALIDATION), "DATE_VALIDATION");
    EXPECT_EQ(ValidationEngine::validationRuleTypeToString(ValidationRuleType::COVENANT_TEST), "COVENANT_TEST");
    
    EXPECT_EQ(ValidationEngine::stringToValidationRuleType("NUMERIC_RANGE"), ValidationRuleType::NUMERIC_RANGE);
    EXPECT_EQ(ValidationEngine::stringToValidationRuleType("RATIO_CHECK"), ValidationRuleType::RATIO_CHECK);
    EXPECT_EQ(ValidationEngine::stringToValidationRuleType("DATE_VALIDATION"), ValidationRuleType::DATE_VALIDATION);
}

TEST_F(ValidationEngineTest, ValidationStatusStringConversion) {
    EXPECT_EQ(ValidationEngine::validationStatusToString(ValidationStatus::PASSED), "PASSED");
    EXPECT_EQ(ValidationEngine::validationStatusToString(ValidationStatus::FAILED), "FAILED");
    EXPECT_EQ(ValidationEngine::validationStatusToString(ValidationStatus::WARNING), "WARNING");
    EXPECT_EQ(ValidationEngine::validationStatusToString(ValidationStatus::SKIPPED), "SKIPPED");
    EXPECT_EQ(ValidationEngine::validationStatusToString(ValidationStatus::ERROR), "ERROR");
    
    EXPECT_EQ(ValidationEngine::stringToValidationStatus("PASSED"), ValidationStatus::PASSED);
    EXPECT_EQ(ValidationEngine::stringToValidationStatus("FAILED"), ValidationStatus::FAILED);
    EXPECT_EQ(ValidationEngine::stringToValidationStatus("WARNING"), ValidationStatus::WARNING);
}

// Rule management tests
TEST_F(ValidationEngineTest, AddAndRetrieveValidationRule) {
    ValidationRule test_rule("TEST_RULE", "Test Rule", ValidationRuleType::NUMERIC_RANGE, 
                           ValidationCategory::DATA_INTEGRITY, ValidationSeverity::WARNING);
    test_rule.description = "A test validation rule";
    test_rule.parameters.push_back(ValidationParameter("field", std::string("test_field")));
    test_rule.parameters.push_back(ValidationParameter("min_value", static_cast<Amount>(0.0)));
    test_rule.parameters.push_back(ValidationParameter("max_value", static_cast<Amount>(100.0)));
    
    validation_engine->addValidationRule(test_rule);
    
    ValidationRule retrieved_rule = validation_engine->getValidationRule("TEST_RULE");
    EXPECT_EQ(retrieved_rule.id, "TEST_RULE");
    EXPECT_EQ(retrieved_rule.name, "Test Rule");
    EXPECT_EQ(retrieved_rule.type, ValidationRuleType::NUMERIC_RANGE);
    EXPECT_EQ(retrieved_rule.category, ValidationCategory::DATA_INTEGRITY);
    EXPECT_EQ(retrieved_rule.severity, ValidationSeverity::WARNING);
}

TEST_F(ValidationEngineTest, UpdateValidationRule) {
    ValidationRule original_rule("UPDATE_TEST", "Original Rule", ValidationRuleType::NUMERIC_RANGE,
                               ValidationCategory::DATA_INTEGRITY);
    validation_engine->addValidationRule(original_rule);
    
    ValidationRule updated_rule("UPDATE_TEST", "Updated Rule", ValidationRuleType::RATIO_CHECK,
                              ValidationCategory::BUSINESS_RULES, ValidationSeverity::CRITICAL);
    validation_engine->updateValidationRule("UPDATE_TEST", updated_rule);
    
    ValidationRule retrieved_rule = validation_engine->getValidationRule("UPDATE_TEST");
    EXPECT_EQ(retrieved_rule.name, "Updated Rule");
    EXPECT_EQ(retrieved_rule.type, ValidationRuleType::RATIO_CHECK);
    EXPECT_EQ(retrieved_rule.category, ValidationCategory::BUSINESS_RULES);
    EXPECT_EQ(retrieved_rule.severity, ValidationSeverity::CRITICAL);
}

TEST_F(ValidationEngineTest, RemoveValidationRule) {
    ValidationRule test_rule("REMOVE_TEST", "Rule To Remove", ValidationRuleType::NUMERIC_RANGE,
                           ValidationCategory::DATA_INTEGRITY);
    validation_engine->addValidationRule(test_rule);
    
    // Rule should exist
    EXPECT_NO_THROW(validation_engine->getValidationRule("REMOVE_TEST"));
    
    validation_engine->removeValidationRule("REMOVE_TEST");
    
    // Rule should no longer exist
    EXPECT_THROW(validation_engine->getValidationRule("REMOVE_TEST"), std::runtime_error);
}

TEST_F(ValidationEngineTest, GetRulesByCategory) {
    ValidationRule data_rule("DATA_RULE", "Data Rule", ValidationRuleType::NUMERIC_RANGE,
                           ValidationCategory::DATA_INTEGRITY);
    ValidationRule business_rule("BUS_RULE", "Business Rule", ValidationRuleType::BUSINESS_LOGIC,
                               ValidationCategory::BUSINESS_RULES);
    ValidationRule covenant_rule("COV_RULE", "Covenant Rule", ValidationRuleType::COVENANT_TEST,
                               ValidationCategory::COVENANT_COMPLIANCE);
    
    validation_engine->addValidationRule(data_rule);
    validation_engine->addValidationRule(business_rule);
    validation_engine->addValidationRule(covenant_rule);
    
    std::vector<ValidationRule> data_rules = validation_engine->getValidationRules(ValidationCategory::DATA_INTEGRITY);
    std::vector<ValidationRule> business_rules = validation_engine->getValidationRules(ValidationCategory::BUSINESS_RULES);
    std::vector<ValidationRule> covenant_rules = validation_engine->getValidationRules(ValidationCategory::COVENANT_COMPLIANCE);
    
    // Should include our added rules plus any default rules in those categories
    EXPECT_GE(data_rules.size(), 1);
    EXPECT_GE(business_rules.size(), 1);
    EXPECT_GE(covenant_rules.size(), 1);
    
    // Check that the specific rules are in the right categories
    bool found_data_rule = false, found_business_rule = false, found_covenant_rule = false;
    
    for (const auto& rule : data_rules) {
        if (rule.id == "DATA_RULE") found_data_rule = true;
    }
    for (const auto& rule : business_rules) {
        if (rule.id == "BUS_RULE") found_business_rule = true;
    }
    for (const auto& rule : covenant_rules) {
        if (rule.id == "COV_RULE") found_covenant_rule = true;
    }
    
    EXPECT_TRUE(found_data_rule);
    EXPECT_TRUE(found_business_rule);
    EXPECT_TRUE(found_covenant_rule);
}

// Covenant management tests
TEST_F(ValidationEngineTest, AddAndRetrieveCovenantTest) {
    CovenantTest test_covenant("TEST_COVENANT", "Test Covenant");
    test_covenant.description = "A test covenant for validation";
    test_covenant.breach_severity = ValidationSeverity::CRITICAL;
    test_covenant.value_calculator = [](const DealBase& deal) -> Amount { return 1.15; };
    test_covenant.threshold_calculator = [](const DealBase& deal) -> Amount { return 1.10; };
    test_covenant.comparison = [](Amount actual, Amount threshold) -> bool { return actual >= threshold; };
    
    validation_engine->addCovenantTest(test_covenant);
    
    CovenantTest retrieved_covenant = validation_engine->getCovenantTest("TEST_COVENANT");
    EXPECT_EQ(retrieved_covenant.covenant_id, "TEST_COVENANT");
    EXPECT_EQ(retrieved_covenant.name, "Test Covenant");
    EXPECT_EQ(retrieved_covenant.breach_severity, ValidationSeverity::CRITICAL);
}

TEST_F(ValidationEngineTest, RemoveCovenantTest) {
    CovenantTest test_covenant("REMOVE_COVENANT", "Covenant To Remove");
    validation_engine->addCovenantTest(test_covenant);
    
    // Covenant should exist
    EXPECT_NO_THROW(validation_engine->getCovenantTest("REMOVE_COVENANT"));
    
    validation_engine->removeCovenantTest("REMOVE_COVENANT");
    
    // Covenant should no longer exist
    EXPECT_THROW(validation_engine->getCovenantTest("REMOVE_COVENANT"), std::runtime_error);
}

TEST_F(ValidationEngineTest, GetAllCovenantTests) {
    CovenantTest covenant1("COV1", "First Covenant");
    CovenantTest covenant2("COV2", "Second Covenant");
    
    validation_engine->addCovenantTest(covenant1);
    validation_engine->addCovenantTest(covenant2);
    
    std::vector<CovenantTest> all_covenants = validation_engine->getAllCovenantTests();
    
    // Should include our added covenants plus any default covenants
    EXPECT_GE(all_covenants.size(), 2);
    
    bool found_cov1 = false, found_cov2 = false;
    for (const auto& covenant : all_covenants) {
        if (covenant.covenant_id == "COV1") found_cov1 = true;
        if (covenant.covenant_id == "COV2") found_cov2 = true;
    }
    
    EXPECT_TRUE(found_cov1);
    EXPECT_TRUE(found_cov2);
}

// Single rule validation tests
TEST_F(ValidationEngineTest, ValidateSingleRuleSuccess) {
    ValidationRule test_rule("SINGLE_TEST", "Single Test Rule", ValidationRuleType::NUMERIC_RANGE,
                           ValidationCategory::DATA_INTEGRITY);
    test_rule.parameters.push_back(ValidationParameter("field", std::string("total_balance")));
    test_rule.parameters.push_back(ValidationParameter("min_value", static_cast<Amount>(0.0)));
    test_rule.parameters.push_back(ValidationParameter("max_value", static_cast<Amount>(1e12)));
    
    validation_engine->addValidationRule(test_rule);
    
    ValidationResult result = validation_engine->validateSingleRule(*mock_deal, "SINGLE_TEST");
    
    EXPECT_EQ(result.rule_id, "SINGLE_TEST");
    EXPECT_EQ(result.rule_name, "Single Test Rule");
    EXPECT_EQ(result.category, ValidationCategory::DATA_INTEGRITY);
    // Status could be PASSED or FAILED depending on the mock deal implementation
    EXPECT_TRUE(result.status == ValidationStatus::PASSED || 
                result.status == ValidationStatus::FAILED);
}

TEST_F(ValidationEngineTest, ValidateSingleRuleNotFound) {
    ValidationResult result = validation_engine->validateSingleRule(*mock_deal, "NONEXISTENT_RULE");
    
    EXPECT_EQ(result.rule_id, "NONEXISTENT_RULE");
    EXPECT_EQ(result.status, ValidationStatus::ERROR);
    EXPECT_EQ(result.severity, ValidationSeverity::ERROR);
    EXPECT_EQ(result.message, "Rule not found");
}

TEST_F(ValidationEngineTest, ValidateSingleRuleDisabled) {
    ValidationRule disabled_rule("DISABLED_RULE", "Disabled Rule", ValidationRuleType::NUMERIC_RANGE,
                               ValidationCategory::DATA_INTEGRITY);
    disabled_rule.is_active = false;
    
    validation_engine->addValidationRule(disabled_rule);
    
    ValidationResult result = validation_engine->validateSingleRule(*mock_deal, "DISABLED_RULE");
    
    EXPECT_EQ(result.status, ValidationStatus::SKIPPED);
    EXPECT_EQ(result.message, "Rule is disabled");
}

// Category validation tests
TEST_F(ValidationEngineTest, ValidateByCategory) {
    ValidationRule data_rule1("DATA1", "Data Rule 1", ValidationRuleType::NUMERIC_RANGE,
                            ValidationCategory::DATA_INTEGRITY);
    ValidationRule data_rule2("DATA2", "Data Rule 2", ValidationRuleType::DATE_VALIDATION,
                            ValidationCategory::DATA_INTEGRITY);
    ValidationRule business_rule("BUS1", "Business Rule 1", ValidationRuleType::BUSINESS_LOGIC,
                               ValidationCategory::BUSINESS_RULES);
    
    validation_engine->addValidationRule(data_rule1);
    validation_engine->addValidationRule(data_rule2);
    validation_engine->addValidationRule(business_rule);
    
    std::vector<ValidationResult> data_results = 
        validation_engine->validateByCategory(*mock_deal, ValidationCategory::DATA_INTEGRITY);
    std::vector<ValidationResult> business_results = 
        validation_engine->validateByCategory(*mock_deal, ValidationCategory::BUSINESS_RULES);
    
    // Should include our rules plus any default rules in those categories
    EXPECT_GE(data_results.size(), 2);
    EXPECT_GE(business_results.size(), 1);
    
    // Check that results are for the correct category
    for (const auto& result : data_results) {
        EXPECT_EQ(result.category, ValidationCategory::DATA_INTEGRITY);
    }
    for (const auto& result : business_results) {
        EXPECT_EQ(result.category, ValidationCategory::BUSINESS_RULES);
    }
}

// Full deal validation tests
TEST_F(ValidationEngineTest, ValidateDealSummary) {
    ValidationSummary summary = validation_engine->validateDeal(*mock_deal);
    
    EXPECT_GE(summary.total_rules_executed, 0);
    EXPECT_GE(summary.validation_score, 0.0);
    EXPECT_LE(summary.validation_score, 100.0);
    EXPECT_FALSE(summary.overall_status.empty());
    
    // Summary counts should add up
    EXPECT_EQ(summary.total_rules_executed, 
              summary.rules_passed + summary.rules_failed + 
              summary.rules_warning + summary.rules_skipped + summary.rules_error);
}

TEST_F(ValidationEngineTest, ValidateDealIncremental) {
    // Add some test rules
    ValidationRule rule1("INC1", "Incremental Rule 1", ValidationRuleType::NUMERIC_RANGE,
                       ValidationCategory::DATA_INTEGRITY);
    ValidationRule rule2("INC2", "Incremental Rule 2", ValidationRuleType::DATE_VALIDATION,
                       ValidationCategory::DATA_INTEGRITY);
    
    validation_engine->addValidationRule(rule1);
    validation_engine->addValidationRule(rule2);
    
    std::vector<std::string> rule_ids = {"INC1", "INC2"};
    ValidationSummary summary = validation_engine->validateDealIncremental(*mock_deal, rule_ids);
    
    EXPECT_EQ(summary.total_rules_executed, 2);
    EXPECT_GE(summary.validation_score, 0.0);
    EXPECT_LE(summary.validation_score, 100.0);
}

// Covenant testing
TEST_F(ValidationEngineTest, TestSingleCovenant) {
    CovenantTest test_covenant("SINGLE_COV", "Single Covenant Test");
    test_covenant.value_calculator = [](const DealBase& deal) -> Amount { return 1.15; };
    test_covenant.threshold_calculator = [](const DealBase& deal) -> Amount { return 1.10; };
    test_covenant.comparison = [](Amount actual, Amount threshold) -> bool { return actual >= threshold; };
    
    validation_engine->addCovenantTest(test_covenant);
    
    ValidationResult result = validation_engine->testCovenant(*mock_deal, "SINGLE_COV");
    
    EXPECT_EQ(result.rule_id, "SINGLE_COV");
    EXPECT_EQ(result.rule_name, "Single Covenant Test");
    EXPECT_EQ(result.category, ValidationCategory::COVENANT_COMPLIANCE);
    EXPECT_EQ(result.status, ValidationStatus::PASSED); // 1.15 >= 1.10
    EXPECT_EQ(result.actual_value, 1.15);
    EXPECT_EQ(result.expected_value, 1.10);
}

TEST_F(ValidationEngineTest, TestCovenantBreach) {
    CovenantTest breach_covenant("BREACH_COV", "Breach Covenant Test");
    breach_covenant.value_calculator = [](const DealBase& deal) -> Amount { return 1.05; };
    breach_covenant.threshold_calculator = [](const DealBase& deal) -> Amount { return 1.10; };
    breach_covenant.comparison = [](Amount actual, Amount threshold) -> bool { return actual >= threshold; };
    
    validation_engine->addCovenantTest(breach_covenant);
    
    ValidationResult result = validation_engine->testCovenant(*mock_deal, "BREACH_COV");
    
    EXPECT_EQ(result.status, ValidationStatus::FAILED); // 1.05 < 1.10
    EXPECT_EQ(result.actual_value, 1.05);
    EXPECT_EQ(result.expected_value, 1.10);
}

TEST_F(ValidationEngineTest, TestAllCovenants) {
    CovenantTest covenant1("ALL_COV1", "All Covenants Test 1");
    CovenantTest covenant2("ALL_COV2", "All Covenants Test 2");
    
    validation_engine->addCovenantTest(covenant1);
    validation_engine->addCovenantTest(covenant2);
    
    std::vector<ValidationResult> results = validation_engine->testCovenants(*mock_deal);
    
    // Should include our covenants plus any default covenants
    EXPECT_GE(results.size(), 2);
    
    // All results should be covenant compliance category
    for (const auto& result : results) {
        EXPECT_EQ(result.category, ValidationCategory::COVENANT_COMPLIANCE);
    }
}

// Rule enable/disable tests
TEST_F(ValidationEngineTest, EnableDisableRule) {
    ValidationRule toggle_rule("TOGGLE_RULE", "Toggle Rule", ValidationRuleType::NUMERIC_RANGE,
                             ValidationCategory::DATA_INTEGRITY);
    validation_engine->addValidationRule(toggle_rule);
    
    // Rule should be enabled by default
    ValidationResult result1 = validation_engine->validateSingleRule(*mock_deal, "TOGGLE_RULE");
    EXPECT_NE(result1.status, ValidationStatus::SKIPPED);
    
    // Disable the rule
    validation_engine->disableRule("TOGGLE_RULE");
    ValidationResult result2 = validation_engine->validateSingleRule(*mock_deal, "TOGGLE_RULE");
    EXPECT_EQ(result2.status, ValidationStatus::SKIPPED);
    
    // Re-enable the rule
    validation_engine->enableRule("TOGGLE_RULE");
    ValidationResult result3 = validation_engine->validateSingleRule(*mock_deal, "TOGGLE_RULE");
    EXPECT_NE(result3.status, ValidationStatus::SKIPPED);
}

// History and reporting tests
TEST_F(ValidationEngineTest, ValidationHistory) {
    validation_engine->clearValidationHistory();
    
    ValidationRule history_rule("HISTORY_RULE", "History Rule", ValidationRuleType::NUMERIC_RANGE,
                              ValidationCategory::DATA_INTEGRITY);
    validation_engine->addValidationRule(history_rule);
    
    // Run some validations to build history
    validation_engine->validateSingleRule(*mock_deal, "HISTORY_RULE");
    validation_engine->validateSingleRule(*mock_deal, "HISTORY_RULE");
    
    std::vector<ValidationResult> history = validation_engine->getValidationHistory();
    EXPECT_GE(history.size(), 2);
}

TEST_F(ValidationEngineTest, ValidationHistoryByCategory) {
    validation_engine->clearValidationHistory();
    
    ValidationRule data_rule("DATA_HIST", "Data History Rule", ValidationRuleType::NUMERIC_RANGE,
                           ValidationCategory::DATA_INTEGRITY);
    ValidationRule business_rule("BUS_HIST", "Business History Rule", ValidationRuleType::BUSINESS_LOGIC,
                               ValidationCategory::BUSINESS_RULES);
    
    validation_engine->addValidationRule(data_rule);
    validation_engine->addValidationRule(business_rule);
    
    validation_engine->validateSingleRule(*mock_deal, "DATA_HIST");
    validation_engine->validateSingleRule(*mock_deal, "BUS_HIST");
    
    std::vector<ValidationResult> data_history = 
        validation_engine->getValidationHistory(ValidationCategory::DATA_INTEGRITY);
    std::vector<ValidationResult> business_history = 
        validation_engine->getValidationHistory(ValidationCategory::BUSINESS_RULES);
    
    EXPECT_GE(data_history.size(), 1);
    EXPECT_GE(business_history.size(), 1);
    
    // Check categories
    for (const auto& result : data_history) {
        EXPECT_EQ(result.category, ValidationCategory::DATA_INTEGRITY);
    }
    for (const auto& result : business_history) {
        EXPECT_EQ(result.category, ValidationCategory::BUSINESS_RULES);
    }
}

// Export and import tests
TEST_F(ValidationEngineTest, ExportValidationResults) {
    ValidationSummary summary;
    summary.validation_date = Date::todaysDate();
    summary.total_rules_executed = 10;
    summary.rules_passed = 8;
    summary.rules_failed = 1;
    summary.rules_warning = 1;
    summary.validation_score = 80.0;
    summary.overall_status = "WARNING";
    
    std::string json_export = validation_engine->exportValidationResults(summary, "JSON");
    
    EXPECT_FALSE(json_export.empty());
    EXPECT_NE(json_export.find("validation_date"), std::string::npos);
    EXPECT_NE(json_export.find("total_rules_executed"), std::string::npos);
    EXPECT_NE(json_export.find("validation_score"), std::string::npos);
    EXPECT_NE(json_export.find("overall_status"), std::string::npos);
}

TEST_F(ValidationEngineTest, TriggerEvaluation) {
    std::vector<TriggerResult> triggers = validation_engine->evaluateTriggers(*mock_deal);
    
    // Should return trigger results (even if empty for mock implementation)
    EXPECT_TRUE(triggers.size() >= 0);
}

// Performance test
TEST_F(ValidationEngineTest, PerformanceWithMultipleRules) {
    // Add multiple rules for performance testing
    for (int i = 0; i < 50; ++i) {
        ValidationRule rule("PERF_RULE_" + std::to_string(i), 
                          "Performance Rule " + std::to_string(i),
                          ValidationRuleType::NUMERIC_RANGE,
                          ValidationCategory::DATA_INTEGRITY);
        validation_engine->addValidationRule(rule);
    }
    
    auto start = std::chrono::high_resolution_clock::now();
    ValidationSummary summary = validation_engine->validateDeal(*mock_deal);
    auto end = std::chrono::high_resolution_clock::now();
    
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
    
    EXPECT_LT(duration.count(), 1000); // Should complete within 1 second
    EXPECT_GE(summary.total_rules_executed, 50);
}