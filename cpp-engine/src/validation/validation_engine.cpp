#include "validation_engine.h"
#include <algorithm>
#include <numeric>
#include <sstream>
#include <iomanip>
#include <fstream>
#include <cmath>

namespace Structura {

ValidationEngine::ValidationEngine() 
    : analytics_engine_(nullptr) {
    loadStandardValidationRules();
}

ValidationEngine::ValidationEngine(std::shared_ptr<AnalyticsEngine> analytics)
    : analytics_engine_(analytics) {
    loadStandardValidationRules();
}

// Rule management
void ValidationEngine::addValidationRule(const ValidationRule& rule) {
    validation_rules_[rule.id] = rule;
}

void ValidationEngine::removeValidationRule(const std::string& rule_id) {
    validation_rules_.erase(rule_id);
}

void ValidationEngine::updateValidationRule(const std::string& rule_id, const ValidationRule& rule) {
    if (validation_rules_.find(rule_id) != validation_rules_.end()) {
        validation_rules_[rule_id] = rule;
    }
}

ValidationRule ValidationEngine::getValidationRule(const std::string& rule_id) const {
    auto it = validation_rules_.find(rule_id);
    if (it != validation_rules_.end()) {
        return it->second;
    }
    throw std::runtime_error("Validation rule not found: " + rule_id);
}

std::vector<ValidationRule> ValidationEngine::getValidationRules(ValidationCategory category) const {
    std::vector<ValidationRule> rules;
    for (const auto& pair : validation_rules_) {
        if (pair.second.category == category) {
            rules.push_back(pair.second);
        }
    }
    return rules;
}

std::vector<std::string> ValidationEngine::getAvailableRules() const {
    std::vector<std::string> rule_ids;
    for (const auto& pair : validation_rules_) {
        rule_ids.push_back(pair.first);
    }
    return rule_ids;
}

// Covenant management
void ValidationEngine::addCovenantTest(const CovenantTest& covenant) {
    covenant_tests_[covenant.covenant_id] = covenant;
}

void ValidationEngine::removeCovenantTest(const std::string& covenant_id) {
    covenant_tests_.erase(covenant_id);
}

CovenantTest ValidationEngine::getCovenantTest(const std::string& covenant_id) const {
    auto it = covenant_tests_.find(covenant_id);
    if (it != covenant_tests_.end()) {
        return it->second;
    }
    throw std::runtime_error("Covenant test not found: " + covenant_id);
}

std::vector<CovenantTest> ValidationEngine::getAllCovenantTests() const {
    std::vector<CovenantTest> covenants;
    for (const auto& pair : covenant_tests_) {
        covenants.push_back(pair.second);
    }
    return covenants;
}

// Validation execution
ValidationResult ValidationEngine::validateSingleRule(const DealBase& deal, const std::string& rule_id) {
    auto it = validation_rules_.find(rule_id);
    if (it == validation_rules_.end()) {
        return ValidationResult(rule_id, "Unknown Rule", ValidationStatus::ERROR, 
                              ValidationSeverity::ERROR, ValidationCategory::DATA_INTEGRITY,
                              "Rule not found");
    }
    
    const ValidationRule& rule = it->second;
    if (!rule.is_active) {
        return ValidationResult(rule_id, rule.name, ValidationStatus::SKIPPED,
                              rule.severity, rule.category, "Rule is disabled");
    }
    
    try {
        ValidationResult result(rule_id, rule.name, ValidationStatus::PASSED,
                              rule.severity, rule.category, "Validation passed");
        
        bool validation_passed = false;
        
        // Execute appropriate validation based on rule type
        switch (rule.type) {
            case ValidationRuleType::NUMERIC_RANGE:
                validation_passed = validateNumericRange(deal, rule);
                break;
            case ValidationRuleType::RATIO_CHECK:
                validation_passed = validateRatioCheck(deal, rule);
                break;
            case ValidationRuleType::DATE_VALIDATION:
                validation_passed = validateDateValidation(deal, rule);
                break;
            case ValidationRuleType::COVENANT_TEST:
                validation_passed = validateCovenantTest(deal, rule);
                break;
            case ValidationRuleType::BUSINESS_LOGIC:
                validation_passed = validateBusinessLogic(deal, rule);
                break;
            case ValidationRuleType::AGGREGATE_CHECK:
                validation_passed = validateAggregateCheck(deal, rule);
                break;
            case ValidationRuleType::CROSS_REFERENCE:
                validation_passed = validateCrossReference(deal, rule);
                break;
            case ValidationRuleType::COMPLIANCE_CHECK:
                validation_passed = validateComplianceCheck(deal, rule);
                break;
            case ValidationRuleType::CUSTOM_RULE:
                if (rule.validator) {
                    validation_passed = rule.validator(deal, rule);
                } else {
                    result.status = ValidationStatus::ERROR;
                    result.message = "Custom validator not provided";
                    return result;
                }
                break;
            default:
                result.status = ValidationStatus::ERROR;
                result.message = "Unknown validation rule type";
                return result;
        }
        
        if (!validation_passed) {
            result.status = (rule.severity == ValidationSeverity::WARNING) ? 
                           ValidationStatus::WARNING : ValidationStatus::FAILED;
            result.message = formatValidationMessage(rule, result);
        }
        
        // Add to history
        validation_history_.push_back(result);
        
        return result;
        
    } catch (const std::exception& e) {
        ValidationResult error_result(rule_id, rule.name, ValidationStatus::ERROR,
                                    ValidationSeverity::ERROR, rule.category,
                                    "Validation error: " + std::string(e.what()));
        validation_history_.push_back(error_result);
        return error_result;
    }
}

std::vector<ValidationResult> ValidationEngine::validateByCategory(const DealBase& deal, ValidationCategory category) {
    std::vector<ValidationResult> results;
    
    for (const auto& pair : validation_rules_) {
        if (pair.second.category == category) {
            results.push_back(validateSingleRule(deal, pair.first));
        }
    }
    
    return results;
}

ValidationSummary ValidationEngine::validateDeal(const DealBase& deal) {
    ValidationSummary summary;
    std::vector<ValidationResult> all_results;
    
    // Run all validation rules
    for (const auto& pair : validation_rules_) {
        if (pair.second.is_active) {
            ValidationResult result = validateSingleRule(deal, pair.first);
            all_results.push_back(result);
            validation_history_.push_back(result);
            
            // Update counters
            summary.total_rules_executed++;
            switch (result.status) {
                case ValidationStatus::PASSED:
                    summary.rules_passed++;
                    break;
                case ValidationStatus::FAILED:
                    summary.rules_failed++;
                    if (result.severity == ValidationSeverity::CRITICAL) {
                        summary.critical_issues.push_back(result);
                    } else {
                        summary.errors.push_back(result);
                    }
                    break;
                case ValidationStatus::WARNING:
                    summary.rules_warning++;
                    summary.warnings.push_back(result);
                    break;
                case ValidationStatus::SKIPPED:
                    summary.rules_skipped++;
                    break;
                case ValidationStatus::ERROR:
                    summary.rules_error++;
                    summary.errors.push_back(result);
                    break;
            }
            
            // Update category counts
            summary.issues_by_category[result.category]++;
        }
    }
    
    // Test covenants
    std::vector<ValidationResult> covenant_results = testCovenants(deal);
    all_results.insert(all_results.end(), covenant_results.begin(), covenant_results.end());
    
    // Evaluate triggers
    summary.triggered_events = evaluateTriggers(deal);
    
    // Calculate validation score
    if (summary.total_rules_executed > 0) {
        summary.validation_score = (static_cast<double>(summary.rules_passed) / 
                                  summary.total_rules_executed) * 100.0;
    }
    
    // Determine overall status
    if (summary.critical_issues.size() > 0) {
        summary.overall_status = "CRITICAL";
    } else if (summary.rules_failed > 0) {
        summary.overall_status = "FAIL";
    } else if (summary.rules_warning > 0) {
        summary.overall_status = "WARNING";
    } else {
        summary.overall_status = "PASS";
    }
    
    return summary;
}

ValidationSummary ValidationEngine::validateDealIncremental(const DealBase& deal, 
                                                           const std::vector<std::string>& rule_ids) {
    ValidationSummary summary;
    
    for (const std::string& rule_id : rule_ids) {
        ValidationResult result = validateSingleRule(deal, rule_id);
        validation_history_.push_back(result);
        
        summary.total_rules_executed++;
        switch (result.status) {
            case ValidationStatus::PASSED:
                summary.rules_passed++;
                break;
            case ValidationStatus::FAILED:
                summary.rules_failed++;
                if (result.severity == ValidationSeverity::CRITICAL) {
                    summary.critical_issues.push_back(result);
                } else {
                    summary.errors.push_back(result);
                }
                break;
            case ValidationStatus::WARNING:
                summary.rules_warning++;
                summary.warnings.push_back(result);
                break;
            case ValidationStatus::SKIPPED:
                summary.rules_skipped++;
                break;
            case ValidationStatus::ERROR:
                summary.rules_error++;
                summary.errors.push_back(result);
                break;
        }
    }
    
    // Calculate validation score
    if (summary.total_rules_executed > 0) {
        summary.validation_score = (static_cast<double>(summary.rules_passed) / 
                                  summary.total_rules_executed) * 100.0;
    }
    
    return summary;
}

// Covenant testing
std::vector<ValidationResult> ValidationEngine::testCovenants(const DealBase& deal) {
    std::vector<ValidationResult> results;
    
    for (const auto& pair : covenant_tests_) {
        results.push_back(testCovenant(deal, pair.first));
    }
    
    return results;
}

ValidationResult ValidationEngine::testCovenant(const DealBase& deal, const std::string& covenant_id) {
    auto it = covenant_tests_.find(covenant_id);
    if (it == covenant_tests_.end()) {
        return ValidationResult(covenant_id, "Unknown Covenant", ValidationStatus::ERROR,
                              ValidationSeverity::ERROR, ValidationCategory::COVENANT_COMPLIANCE,
                              "Covenant test not found");
    }
    
    const CovenantTest& covenant = it->second;
    if (!covenant.is_active) {
        return ValidationResult(covenant_id, covenant.name, ValidationStatus::SKIPPED,
                              covenant.breach_severity, ValidationCategory::COVENANT_COMPLIANCE,
                              "Covenant test is disabled");
    }
    
    try {
        Amount actual_value = covenant.value_calculator(deal);
        Amount threshold_value = covenant.threshold_calculator(deal);
        bool covenant_passed = covenant.comparison(actual_value, threshold_value);
        
        ValidationResult result(covenant_id, covenant.name, 
                              covenant_passed ? ValidationStatus::PASSED : ValidationStatus::FAILED,
                              covenant.breach_severity, ValidationCategory::COVENANT_COMPLIANCE,
                              covenant_passed ? "Covenant test passed" : "Covenant breach detected");
        
        result.actual_value = actual_value;
        result.expected_value = threshold_value;
        result.details = covenant.description;
        
        return result;
        
    } catch (const std::exception& e) {
        return ValidationResult(covenant_id, covenant.name, ValidationStatus::ERROR,
                              ValidationSeverity::ERROR, ValidationCategory::COVENANT_COMPLIANCE,
                              "Covenant test error: " + std::string(e.what()));
    }
}

std::vector<TriggerResult> ValidationEngine::evaluateTriggers(const DealBase& deal) {
    std::vector<TriggerResult> triggers;
    
    // Example trigger evaluations - would be customized based on deal structure
    TriggerResult credit_enhancement_trigger("CE_TRIGGER", "Credit Enhancement Trigger");
    TriggerResult performance_trigger("PERF_TRIGGER", "Performance Trigger");
    TriggerResult liquidity_trigger("LIQ_TRIGGER", "Liquidity Trigger");
    
    // These would contain actual trigger logic based on deal specifics
    // For now, returning empty triggers as placeholders
    
    return triggers;
}

// Built-in validation functions
bool ValidationEngine::validateNumericRange(const DealBase& deal, const ValidationRule& rule) {
    if (rule.parameters.size() < 3) return false;
    
    // Expected parameters: field_name, min_value, max_value
    std::string field_name = std::get<std::string>(rule.parameters[0].value);
    Amount min_value = std::get<Amount>(rule.parameters[1].value);
    Amount max_value = std::get<Amount>(rule.parameters[2].value);
    
    ValidationValue actual = extractValue(deal, field_name);
    if (std::holds_alternative<Amount>(actual)) {
        Amount value = std::get<Amount>(actual);
        return value >= min_value && value <= max_value;
    }
    
    return false;
}

bool ValidationEngine::validateRatioCheck(const DealBase& deal, const ValidationRule& rule) {
    if (rule.parameters.size() < 4) return false;
    
    // Expected parameters: numerator_field, denominator_field, operator, threshold
    std::string numerator_field = std::get<std::string>(rule.parameters[0].value);
    std::string denominator_field = std::get<std::string>(rule.parameters[1].value);
    std::string operator_type = std::get<std::string>(rule.parameters[2].value);
    Amount threshold = std::get<Amount>(rule.parameters[3].value);
    
    ValidationValue num_val = extractValue(deal, numerator_field);
    ValidationValue den_val = extractValue(deal, denominator_field);
    
    if (std::holds_alternative<Amount>(num_val) && std::holds_alternative<Amount>(den_val)) {
        Amount numerator = std::get<Amount>(num_val);
        Amount denominator = std::get<Amount>(den_val);
        
        if (denominator == 0.0) return false;
        
        Amount ratio = numerator / denominator;
        return compareValues(ValidationValue(ratio), ValidationValue(threshold), operator_type);
    }
    
    return false;
}

bool ValidationEngine::validateDateValidation(const DealBase& deal, const ValidationRule& rule) {
    if (rule.parameters.size() < 2) return false;
    
    // Expected parameters: date_field, validation_type
    std::string date_field = std::get<std::string>(rule.parameters[0].value);
    std::string validation_type = std::get<std::string>(rule.parameters[1].value);
    
    ValidationValue date_val = extractValue(deal, date_field);
    if (std::holds_alternative<Date>(date_val)) {
        Date date = std::get<Date>(date_val);
        Date today = Date::todaysDate();
        
        if (validation_type == "future") {
            return date > today;
        } else if (validation_type == "past") {
            return date < today;
        } else if (validation_type == "valid") {
            return true; // If we got a Date object, it's valid
        }
    }
    
    return false;
}

bool ValidationEngine::validateCovenantTest(const DealBase& deal, const ValidationRule& rule) {
    // This would delegate to the covenant testing system
    if (rule.parameters.size() < 1) return false;
    
    std::string covenant_id = std::get<std::string>(rule.parameters[0].value);
    ValidationResult result = testCovenant(deal, covenant_id);
    
    return result.status == ValidationStatus::PASSED;
}

bool ValidationEngine::validateBusinessLogic(const DealBase& deal, const ValidationRule& rule) {
    // Custom business logic validation - would be implemented based on specific requirements
    return true; // Placeholder
}

bool ValidationEngine::validateAggregateCheck(const DealBase& deal, const ValidationRule& rule) {
    // Aggregate checks across multiple fields or assets
    return true; // Placeholder
}

bool ValidationEngine::validateCrossReference(const DealBase& deal, const ValidationRule& rule) {
    // Cross-reference validation between different data elements
    return true; // Placeholder
}

bool ValidationEngine::validateComplianceCheck(const DealBase& deal, const ValidationRule& rule) {
    // Regulatory compliance validation
    return true; // Placeholder
}

// Helper methods
ValidationValue ValidationEngine::extractValue(const DealBase& deal, const std::string& field_name) {
    // This would extract values from the deal based on field name
    // For now, returning placeholder values
    if (field_name == "total_balance") {
        return Amount(1000000.0);
    } else if (field_name == "weighted_average_rate") {
        return Amount(0.05);
    } else if (field_name == "closing_date") {
        return Date::todaysDate();
    }
    
    return Amount(0.0);
}

Amount ValidationEngine::calculateMetric(const DealBase& deal, const std::string& metric_name) {
    if (analytics_engine_) {
        // Use analytics engine to calculate metrics
        return 0.0; // Placeholder
    }
    return 0.0;
}

bool ValidationEngine::compareValues(ValidationValue actual, ValidationValue expected, const std::string& operator_type) {
    if (std::holds_alternative<Amount>(actual) && std::holds_alternative<Amount>(expected)) {
        Amount act_val = std::get<Amount>(actual);
        Amount exp_val = std::get<Amount>(expected);
        
        if (operator_type == ">=") return act_val >= exp_val;
        if (operator_type == "<=") return act_val <= exp_val;
        if (operator_type == ">") return act_val > exp_val;
        if (operator_type == "<") return act_val < exp_val;
        if (operator_type == "==") return std::abs(act_val - exp_val) < 1e-6;
        if (operator_type == "!=") return std::abs(act_val - exp_val) >= 1e-6;
    }
    
    return false;
}

std::string ValidationEngine::formatValidationMessage(const ValidationRule& rule, const ValidationResult& result) {
    std::ostringstream oss;
    oss << "Validation failed for rule: " << rule.name;
    if (!result.details.empty()) {
        oss << " - " << result.details;
    }
    return oss.str();
}

// Built-in rule sets
void ValidationEngine::loadStandardValidationRules() {
    // Data integrity rules
    ValidationRule balance_positive("BAL_POS", "Balance Must Be Positive", 
                                  ValidationRuleType::NUMERIC_RANGE, 
                                  ValidationCategory::DATA_INTEGRITY);
    balance_positive.parameters.push_back(ValidationParameter("field", std::string("total_balance")));
    balance_positive.parameters.push_back(ValidationParameter("min_value", Amount(0.0)));
    balance_positive.parameters.push_back(ValidationParameter("max_value", Amount(1e12)));
    addValidationRule(balance_positive);
    
    // Rate validation
    ValidationRule rate_range("RATE_RANGE", "Interest Rate Range Check",
                            ValidationRuleType::NUMERIC_RANGE,
                            ValidationCategory::DATA_INTEGRITY);
    rate_range.parameters.push_back(ValidationParameter("field", std::string("weighted_average_rate")));
    rate_range.parameters.push_back(ValidationParameter("min_value", Amount(0.0)));
    rate_range.parameters.push_back(ValidationParameter("max_value", Amount(0.5)));
    addValidationRule(rate_range);
    
    // Date validation
    ValidationRule closing_date("CLOSING_DATE", "Closing Date Validation",
                              ValidationRuleType::DATE_VALIDATION,
                              ValidationCategory::DATA_INTEGRITY);
    closing_date.parameters.push_back(ValidationParameter("field", std::string("closing_date")));
    closing_date.parameters.push_back(ValidationParameter("validation_type", std::string("past")));
    addValidationRule(closing_date);
}

void ValidationEngine::loadRegulatoryComplianceRules() {
    // Regulatory compliance rules would be loaded here
}

void ValidationEngine::loadBusinessLogicRules() {
    // Business logic rules would be loaded here
}

void ValidationEngine::loadCovenantTestRules() {
    // Standard covenant tests
    CovenantTest overcollateralization("OC_TEST", "Overcollateralization Test");
    overcollateralization.description = "Test that collateral value exceeds bond balance by required ratio";
    overcollateralization.value_calculator = [](const DealBase& deal) -> Amount {
        // Calculate OC ratio
        return 1.15; // Placeholder
    };
    overcollateralization.threshold_calculator = [](const DealBase& deal) -> Amount {
        return 1.10; // Required minimum OC ratio
    };
    overcollateralization.comparison = [](Amount actual, Amount threshold) -> bool {
        return actual >= threshold;
    };
    addCovenantTest(overcollateralization);
}

void ValidationEngine::loadDataIntegrityRules() {
    // Data integrity rules would be loaded here
}

// Configuration and utilities
void ValidationEngine::setAnalyticsEngine(std::shared_ptr<AnalyticsEngine> analytics) {
    analytics_engine_ = analytics;
}

void ValidationEngine::enableRule(const std::string& rule_id) {
    auto it = validation_rules_.find(rule_id);
    if (it != validation_rules_.end()) {
        it->second.is_active = true;
    }
}

void ValidationEngine::disableRule(const std::string& rule_id) {
    auto it = validation_rules_.find(rule_id);
    if (it != validation_rules_.end()) {
        it->second.is_active = false;
    }
}

void ValidationEngine::setSeverityLevel(ValidationSeverity min_severity) {
    // Implementation would filter rules based on severity
}

// History and reporting
std::vector<ValidationResult> ValidationEngine::getValidationHistory() const {
    return validation_history_;
}

std::vector<ValidationResult> ValidationEngine::getValidationHistory(ValidationCategory category) const {
    std::vector<ValidationResult> filtered_history;
    for (const auto& result : validation_history_) {
        if (result.category == category) {
            filtered_history.push_back(result);
        }
    }
    return filtered_history;
}

ValidationSummary ValidationEngine::getLastValidationSummary() const {
    // Would return the most recent validation summary
    return ValidationSummary();
}

void ValidationEngine::clearValidationHistory() {
    validation_history_.clear();
}

// Export and import
std::string ValidationEngine::exportValidationResults(const ValidationSummary& summary, const std::string& format) const {
    if (format == "JSON") {
        std::ostringstream json;
        json << "{\n";
        json << "  \"validation_date\": \"" << summary.validation_date << "\",\n";
        json << "  \"total_rules_executed\": " << summary.total_rules_executed << ",\n";
        json << "  \"rules_passed\": " << summary.rules_passed << ",\n";
        json << "  \"rules_failed\": " << summary.rules_failed << ",\n";
        json << "  \"rules_warning\": " << summary.rules_warning << ",\n";
        json << "  \"validation_score\": " << std::fixed << std::setprecision(2) << summary.validation_score << ",\n";
        json << "  \"overall_status\": \"" << summary.overall_status << "\"\n";
        json << "}";
        return json.str();
    }
    
    return "Unsupported format";
}

void ValidationEngine::saveValidationReport(const ValidationSummary& summary, const std::string& filename) const {
    std::ofstream file(filename);
    if (file.is_open()) {
        file << exportValidationResults(summary, "JSON");
        file.close();
    }
}

// String conversion utilities
std::string ValidationEngine::validationSeverityToString(ValidationSeverity severity) {
    switch (severity) {
        case ValidationSeverity::INFO: return "INFO";
        case ValidationSeverity::WARNING: return "WARNING";
        case ValidationSeverity::ERROR: return "ERROR";
        case ValidationSeverity::CRITICAL: return "CRITICAL";
        default: return "UNKNOWN";
    }
}

std::string ValidationEngine::validationCategoryToString(ValidationCategory category) {
    switch (category) {
        case ValidationCategory::DATA_INTEGRITY: return "DATA_INTEGRITY";
        case ValidationCategory::BUSINESS_RULES: return "BUSINESS_RULES";
        case ValidationCategory::COVENANT_COMPLIANCE: return "COVENANT_COMPLIANCE";
        case ValidationCategory::REGULATORY_COMPLIANCE: return "REGULATORY_COMPLIANCE";
        case ValidationCategory::OPERATIONAL_RULES: return "OPERATIONAL_RULES";
        case ValidationCategory::PERFORMANCE_METRICS: return "PERFORMANCE_METRICS";
        case ValidationCategory::CASH_FLOW_VALIDATION: return "CASH_FLOW_VALIDATION";
        case ValidationCategory::COLLATERAL_VALIDATION: return "COLLATERAL_VALIDATION";
        case ValidationCategory::TRIGGER_EVALUATION: return "TRIGGER_EVALUATION";
        case ValidationCategory::WATERFALL_VALIDATION: return "WATERFALL_VALIDATION";
        default: return "UNKNOWN";
    }
}

std::string ValidationEngine::validationRuleTypeToString(ValidationRuleType type) {
    switch (type) {
        case ValidationRuleType::NUMERIC_RANGE: return "NUMERIC_RANGE";
        case ValidationRuleType::RATIO_CHECK: return "RATIO_CHECK";
        case ValidationRuleType::DATE_VALIDATION: return "DATE_VALIDATION";
        case ValidationRuleType::COVENANT_TEST: return "COVENANT_TEST";
        case ValidationRuleType::BUSINESS_LOGIC: return "BUSINESS_LOGIC";
        case ValidationRuleType::AGGREGATE_CHECK: return "AGGREGATE_CHECK";
        case ValidationRuleType::CROSS_REFERENCE: return "CROSS_REFERENCE";
        case ValidationRuleType::COMPLIANCE_CHECK: return "COMPLIANCE_CHECK";
        case ValidationRuleType::CUSTOM_RULE: return "CUSTOM_RULE";
        default: return "UNKNOWN";
    }
}

std::string ValidationEngine::validationStatusToString(ValidationStatus status) {
    switch (status) {
        case ValidationStatus::PASSED: return "PASSED";
        case ValidationStatus::FAILED: return "FAILED";
        case ValidationStatus::WARNING: return "WARNING";
        case ValidationStatus::SKIPPED: return "SKIPPED";
        case ValidationStatus::ERROR: return "ERROR";
        default: return "UNKNOWN";
    }
}

ValidationSeverity ValidationEngine::stringToValidationSeverity(const std::string& str) {
    if (str == "INFO") return ValidationSeverity::INFO;
    if (str == "WARNING") return ValidationSeverity::WARNING;
    if (str == "ERROR") return ValidationSeverity::ERROR;
    if (str == "CRITICAL") return ValidationSeverity::CRITICAL;
    return ValidationSeverity::ERROR;
}

ValidationCategory ValidationEngine::stringToValidationCategory(const std::string& str) {
    if (str == "DATA_INTEGRITY") return ValidationCategory::DATA_INTEGRITY;
    if (str == "BUSINESS_RULES") return ValidationCategory::BUSINESS_RULES;
    if (str == "COVENANT_COMPLIANCE") return ValidationCategory::COVENANT_COMPLIANCE;
    if (str == "REGULATORY_COMPLIANCE") return ValidationCategory::REGULATORY_COMPLIANCE;
    if (str == "OPERATIONAL_RULES") return ValidationCategory::OPERATIONAL_RULES;
    if (str == "PERFORMANCE_METRICS") return ValidationCategory::PERFORMANCE_METRICS;
    if (str == "CASH_FLOW_VALIDATION") return ValidationCategory::CASH_FLOW_VALIDATION;
    if (str == "COLLATERAL_VALIDATION") return ValidationCategory::COLLATERAL_VALIDATION;
    if (str == "TRIGGER_EVALUATION") return ValidationCategory::TRIGGER_EVALUATION;
    if (str == "WATERFALL_VALIDATION") return ValidationCategory::WATERFALL_VALIDATION;
    return ValidationCategory::DATA_INTEGRITY;
}

ValidationRuleType ValidationEngine::stringToValidationRuleType(const std::string& str) {
    if (str == "NUMERIC_RANGE") return ValidationRuleType::NUMERIC_RANGE;
    if (str == "RATIO_CHECK") return ValidationRuleType::RATIO_CHECK;
    if (str == "DATE_VALIDATION") return ValidationRuleType::DATE_VALIDATION;
    if (str == "COVENANT_TEST") return ValidationRuleType::COVENANT_TEST;
    if (str == "BUSINESS_LOGIC") return ValidationRuleType::BUSINESS_LOGIC;
    if (str == "AGGREGATE_CHECK") return ValidationRuleType::AGGREGATE_CHECK;
    if (str == "CROSS_REFERENCE") return ValidationRuleType::CROSS_REFERENCE;
    if (str == "COMPLIANCE_CHECK") return ValidationRuleType::COMPLIANCE_CHECK;
    if (str == "CUSTOM_RULE") return ValidationRuleType::CUSTOM_RULE;
    return ValidationRuleType::NUMERIC_RANGE;
}

ValidationStatus ValidationEngine::stringToValidationStatus(const std::string& str) {
    if (str == "PASSED") return ValidationStatus::PASSED;
    if (str == "FAILED") return ValidationStatus::FAILED;
    if (str == "WARNING") return ValidationStatus::WARNING;
    if (str == "SKIPPED") return ValidationStatus::SKIPPED;
    if (str == "ERROR") return ValidationStatus::ERROR;
    return ValidationStatus::ERROR;
}

} // namespace Structura