#pragma once

#include <string>
#include <vector>
#include <map>
#include <memory>
#include <functional>
#include <optional>
#include <chrono>
#include <variant>

#include "../core/types.h"
#include "../core/date_utils_enhanced.h"
#include "../core/deal_base.h"
#include "../analytics/analytics_engine.h"

namespace Structura {

// Forward declarations
class DealBase;
class AnalyticsEngine;

// Validation severity levels
enum class ValidationSeverity {
    INFO,
    WARNING,
    ERROR,
    CRITICAL
};

// Validation categories
enum class ValidationCategory {
    DATA_INTEGRITY,
    BUSINESS_RULES,
    COVENANT_COMPLIANCE,
    REGULATORY_COMPLIANCE,
    OPERATIONAL_RULES,
    PERFORMANCE_METRICS,
    CASH_FLOW_VALIDATION,
    COLLATERAL_VALIDATION,
    TRIGGER_EVALUATION,
    WATERFALL_VALIDATION
};

// Validation rule types
enum class ValidationRuleType {
    NUMERIC_RANGE,
    RATIO_CHECK,
    DATE_VALIDATION,
    COVENANT_TEST,
    BUSINESS_LOGIC,
    AGGREGATE_CHECK,
    CROSS_REFERENCE,
    COMPLIANCE_CHECK,
    CUSTOM_RULE
};

// Validation result status
enum class ValidationStatus {
    PASSED,
    FAILED,
    WARNING,
    SKIPPED,
    ERROR
};

// Data types for validation parameters
using ValidationValue = std::variant<
    Amount,
    int,
    std::string,
    Date,
    bool,
    std::vector<Amount>,
    std::vector<Date>
>;

// Validation parameter structure
struct ValidationParameter {
    std::string name;
    ValidationValue value;
    std::string description;
    
    ValidationParameter(const std::string& n, ValidationValue v, const std::string& desc = "")
        : name(n), value(v), description(desc) {}
};

// Validation rule configuration
struct ValidationRule {
    std::string id;
    std::string name;
    std::string description;
    ValidationRuleType type;
    ValidationCategory category;
    ValidationSeverity severity;
    std::vector<ValidationParameter> parameters;
    std::function<bool(const DealBase&, const ValidationRule&)> validator;
    bool is_active;
    
    ValidationRule() = default;
    
    ValidationRule(const std::string& rule_id, const std::string& rule_name,
                  ValidationRuleType rule_type, ValidationCategory cat,
                  ValidationSeverity sev = ValidationSeverity::ERROR)
        : id(rule_id), name(rule_name), type(rule_type), category(cat), 
          severity(sev), is_active(true) {}
};

// Validation result
struct ValidationResult {
    std::string rule_id;
    std::string rule_name;
    ValidationStatus status;
    ValidationSeverity severity;
    ValidationCategory category;
    std::string message;
    std::string details;
    Date validation_date;
    Amount actual_value;
    Amount expected_value;
    std::vector<std::string> affected_components;
    std::map<std::string, ValidationValue> context_data;
    
    ValidationResult(const std::string& id, const std::string& name, 
                    ValidationStatus stat, ValidationSeverity sev,
                    ValidationCategory cat, const std::string& msg)
        : rule_id(id), rule_name(name), status(stat), severity(sev),
          category(cat), message(msg), validation_date(Date::todaysDate()),
          actual_value(0.0), expected_value(0.0) {}
};

// Covenant test configuration
struct CovenantTest {
    std::string covenant_id;
    std::string name;
    std::string description;
    ValidationSeverity breach_severity;
    std::function<Amount(const DealBase&)> value_calculator;
    std::function<Amount(const DealBase&)> threshold_calculator;
    std::function<bool(Amount, Amount)> comparison; // actual, threshold
    bool is_active;
    std::vector<Date> test_dates;
    
    CovenantTest() = default;
    
    CovenantTest(const std::string& id, const std::string& covenant_name)
        : covenant_id(id), name(covenant_name), breach_severity(ValidationSeverity::ERROR),
          is_active(true) {}
};

// Trigger evaluation result
struct TriggerResult {
    std::string trigger_id;
    std::string name;
    bool is_triggered;
    Amount trigger_value;
    Amount threshold_value;
    Date evaluation_date;
    std::string description;
    ValidationSeverity severity;
    std::vector<std::string> consequences;
    
    TriggerResult(const std::string& id, const std::string& trigger_name)
        : trigger_id(id), name(trigger_name), is_triggered(false),
          trigger_value(0.0), threshold_value(0.0), 
          evaluation_date(Date::todaysDate()),
          severity(ValidationSeverity::WARNING) {}
};

// Validation summary
struct ValidationSummary {
    Date validation_date;
    int total_rules_executed;
    int rules_passed;
    int rules_failed;
    int rules_warning;
    int rules_skipped;
    int rules_error;
    std::vector<ValidationResult> critical_issues;
    std::vector<ValidationResult> errors;
    std::vector<ValidationResult> warnings;
    std::vector<TriggerResult> triggered_events;
    std::map<ValidationCategory, int> issues_by_category;
    double validation_score; // 0-100 percentage
    std::string overall_status; // PASS, FAIL, WARNING
    
    ValidationSummary() 
        : validation_date(Date::todaysDate()), total_rules_executed(0),
          rules_passed(0), rules_failed(0), rules_warning(0), 
          rules_skipped(0), rules_error(0), validation_score(0.0),
          overall_status("UNKNOWN") {}
};

// Main validation engine class
class ValidationEngine {
private:
    std::map<std::string, ValidationRule> validation_rules_;
    std::map<std::string, CovenantTest> covenant_tests_;
    std::vector<ValidationResult> validation_history_;
    std::shared_ptr<AnalyticsEngine> analytics_engine_;
    
    // Built-in validation functions
    bool validateNumericRange(const DealBase& deal, const ValidationRule& rule);
    bool validateRatioCheck(const DealBase& deal, const ValidationRule& rule);
    bool validateDateValidation(const DealBase& deal, const ValidationRule& rule);
    bool validateCovenantTest(const DealBase& deal, const ValidationRule& rule);
    bool validateBusinessLogic(const DealBase& deal, const ValidationRule& rule);
    bool validateAggregateCheck(const DealBase& deal, const ValidationRule& rule);
    bool validateCrossReference(const DealBase& deal, const ValidationRule& rule);
    bool validateComplianceCheck(const DealBase& deal, const ValidationRule& rule);
    
    // Helper methods
    ValidationValue extractValue(const DealBase& deal, const std::string& field_name);
    Amount calculateMetric(const DealBase& deal, const std::string& metric_name);
    bool compareValues(ValidationValue actual, ValidationValue expected, const std::string& operator_type);
    std::string formatValidationMessage(const ValidationRule& rule, const ValidationResult& result);

public:
    ValidationEngine();
    explicit ValidationEngine(std::shared_ptr<AnalyticsEngine> analytics);
    ~ValidationEngine() = default;
    
    // Rule management
    void addValidationRule(const ValidationRule& rule);
    void removeValidationRule(const std::string& rule_id);
    void updateValidationRule(const std::string& rule_id, const ValidationRule& rule);
    ValidationRule getValidationRule(const std::string& rule_id) const;
    std::vector<ValidationRule> getValidationRules(ValidationCategory category = ValidationCategory::DATA_INTEGRITY) const;
    std::vector<std::string> getAvailableRules() const;
    
    // Covenant management  
    void addCovenantTest(const CovenantTest& covenant);
    void removeCovenantTest(const std::string& covenant_id);
    CovenantTest getCovenantTest(const std::string& covenant_id) const;
    std::vector<CovenantTest> getAllCovenantTests() const;
    
    // Validation execution
    ValidationResult validateSingleRule(const DealBase& deal, const std::string& rule_id);
    std::vector<ValidationResult> validateByCategory(const DealBase& deal, ValidationCategory category);
    ValidationSummary validateDeal(const DealBase& deal);
    ValidationSummary validateDealIncremental(const DealBase& deal, const std::vector<std::string>& rule_ids);
    
    // Covenant testing
    std::vector<ValidationResult> testCovenants(const DealBase& deal);
    ValidationResult testCovenant(const DealBase& deal, const std::string& covenant_id);
    std::vector<TriggerResult> evaluateTriggers(const DealBase& deal);
    
    // Built-in rule sets
    void loadStandardValidationRules();
    void loadRegulatoryComplianceRules();
    void loadBusinessLogicRules();
    void loadCovenantTestRules();
    void loadDataIntegrityRules();
    
    // Configuration and utilities
    void setAnalyticsEngine(std::shared_ptr<AnalyticsEngine> analytics);
    void enableRule(const std::string& rule_id);
    void disableRule(const std::string& rule_id);
    void setSeverityLevel(ValidationSeverity min_severity);
    
    // History and reporting
    std::vector<ValidationResult> getValidationHistory() const;
    std::vector<ValidationResult> getValidationHistory(ValidationCategory category) const;
    ValidationSummary getLastValidationSummary() const;
    void clearValidationHistory();
    
    // Export and import
    std::string exportValidationResults(const ValidationSummary& summary, const std::string& format = "JSON") const;
    void saveValidationReport(const ValidationSummary& summary, const std::string& filename) const;
    
    // String conversion utilities
    static std::string validationSeverityToString(ValidationSeverity severity);
    static std::string validationCategoryToString(ValidationCategory category);
    static std::string validationRuleTypeToString(ValidationRuleType type);
    static std::string validationStatusToString(ValidationStatus status);
    static ValidationSeverity stringToValidationSeverity(const std::string& str);
    static ValidationCategory stringToValidationCategory(const std::string& str);
    static ValidationRuleType stringToValidationRuleType(const std::string& str);
    static ValidationStatus stringToValidationStatus(const std::string& str);
};

} // namespace Structura