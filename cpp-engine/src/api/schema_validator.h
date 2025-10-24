#pragma once

#include <nlohmann/json.hpp>
#include <string>
#include <vector>
#include <unordered_map>
#include <functional>
#include <optional>
#include <regex>
#include <memory>

using json = nlohmann::json;

namespace structura {
namespace api {
namespace schema {

/**
 * @brief Validation result with detailed error information
 */
struct ValidationError {
    std::string path;         // JSON path to the error (e.g., "deal.structure.tranches[0].balance")
    std::string message;      // Human-readable error message
    std::string code;         // Error code for programmatic handling
    json received;            // The actual value that failed validation
    json expected;            // Expected value/type information
};

struct ValidationResult {
    bool valid = true;
    std::vector<ValidationError> errors;
    json sanitized_data;      // Cleaned/transformed data if validation passes
    
    void addError(const std::string& path, const std::string& message, 
                  const std::string& code = "validation_error",
                  const json& received = json::value_t::null,
                  const json& expected = json::value_t::null) {
        valid = false;
        errors.push_back({path, message, code, received, expected});
    }
    
    std::string getFormattedErrors() const {
        if (valid) return "";
        
        std::string result = "Validation failed with " + std::to_string(errors.size()) + " error(s):\n";
        for (const auto& error : errors) {
            result += "  • " + error.path + ": " + error.message + "\n";
        }
        return result;
    }
};

/**
 * @brief Base schema validator class
 */
class Schema {
public:
    virtual ~Schema() = default;
    virtual ValidationResult validate(const json& data, const std::string& path = "") const = 0;
    virtual json getSchemaDefinition() const = 0;
    virtual std::string getTypeName() const = 0;
    
    // Fluent interface for chaining validations
    std::shared_ptr<Schema> optional() const;
    std::shared_ptr<Schema> defaultValue(const json& value) const;
    std::shared_ptr<Schema> description(const std::string& desc) const;
};

/**
 * @brief Type-specific validators
 */
class StringSchema : public Schema {
private:
    size_t min_length_ = 0;
    size_t max_length_ = SIZE_MAX;
    std::optional<std::regex> pattern_;
    std::vector<std::string> enum_values_;
    std::optional<std::string> description_;
    bool optional_ = false;
    std::optional<json> default_value_;

public:
    StringSchema& min(size_t length) { min_length_ = length; return *this; }
    StringSchema& max(size_t length) { max_length_ = length; return *this; }
    StringSchema& regex(const std::string& pattern) { 
        pattern_ = std::regex(pattern); 
        return *this; 
    }
    StringSchema& oneOf(const std::vector<std::string>& values) { 
        enum_values_ = values; 
        return *this; 
    }
    StringSchema& email() { 
        pattern_ = std::regex(R"([a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,})"); 
        return *this; 
    }
    StringSchema& uuid() { 
        pattern_ = std::regex(R"([0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12})"); 
        return *this; 
    }
    
    ValidationResult validate(const json& data, const std::string& path = "") const override;
    json getSchemaDefinition() const override;
    std::string getTypeName() const override { return "string"; }
};

class NumberSchema : public Schema {
private:
    std::optional<double> min_;
    std::optional<double> max_;
    bool integer_only_ = false;
    std::optional<std::string> description_;
    bool optional_ = false;
    std::optional<json> default_value_;

public:
    NumberSchema& min(double value) { min_ = value; return *this; }
    NumberSchema& max(double value) { max_ = value; return *this; }
    NumberSchema& positive() { min_ = 0.0; return *this; }
    NumberSchema& negative() { max_ = 0.0; return *this; }
    NumberSchema& integer() { integer_only_ = true; return *this; }
    
    ValidationResult validate(const json& data, const std::string& path = "") const override;
    json getSchemaDefinition() const override;
    std::string getTypeName() const override { return integer_only_ ? "integer" : "number"; }
};

class BooleanSchema : public Schema {
private:
    std::optional<std::string> description_;
    bool optional_ = false;
    std::optional<json> default_value_;

public:
    ValidationResult validate(const json& data, const std::string& path = "") const override;
    json getSchemaDefinition() const override;
    std::string getTypeName() const override { return "boolean"; }
};

class ArraySchema : public Schema {
private:
    std::shared_ptr<Schema> item_schema_;
    size_t min_items_ = 0;
    size_t max_items_ = SIZE_MAX;
    std::optional<std::string> description_;
    bool optional_ = false;
    std::optional<json> default_value_;

public:
    ArraySchema(std::shared_ptr<Schema> itemSchema) : item_schema_(itemSchema) {}
    
    ArraySchema& min(size_t count) { min_items_ = count; return *this; }
    ArraySchema& max(size_t count) { max_items_ = count; return *this; }
    ArraySchema& nonEmpty() { min_items_ = 1; return *this; }
    
    ValidationResult validate(const json& data, const std::string& path = "") const override;
    json getSchemaDefinition() const override;
    std::string getTypeName() const override { return "array"; }
};

class ObjectSchema : public Schema {
private:
    std::unordered_map<std::string, std::shared_ptr<Schema>> properties_;
    std::vector<std::string> required_fields_;
    bool allow_additional_ = false;
    std::optional<std::string> description_;
    bool optional_ = false;
    std::optional<json> default_value_;

public:
    ObjectSchema& property(const std::string& name, std::shared_ptr<Schema> schema, bool required = true) {
        properties_[name] = schema;
        if (required) {
            required_fields_.push_back(name);
        }
        return *this;
    }
    
    ObjectSchema& additionalProperties(bool allow = true) { 
        allow_additional_ = allow; 
        return *this; 
    }
    
    ValidationResult validate(const json& data, const std::string& path = "") const override;
    json getSchemaDefinition() const override;
    std::string getTypeName() const override { return "object"; }
};

class UnionSchema : public Schema {
private:
    std::vector<std::shared_ptr<Schema>> schemas_;
    std::optional<std::string> description_;
    bool optional_ = false;

public:
    UnionSchema(std::vector<std::shared_ptr<Schema>> schemas) : schemas_(schemas) {}
    
    ValidationResult validate(const json& data, const std::string& path = "") const override;
    json getSchemaDefinition() const override;
    std::string getTypeName() const override { return "union"; }
};

/**
 * @brief Zod-like factory functions for creating schemas
 */
namespace z {
    inline std::shared_ptr<StringSchema> string() { 
        return std::make_shared<StringSchema>(); 
    }
    
    inline std::shared_ptr<NumberSchema> number() { 
        return std::make_shared<NumberSchema>(); 
    }
    
    inline std::shared_ptr<BooleanSchema> boolean() { 
        return std::make_shared<BooleanSchema>(); 
    }
    
    inline std::shared_ptr<ArraySchema> array(std::shared_ptr<Schema> itemSchema) { 
        return std::make_shared<ArraySchema>(itemSchema); 
    }
    
    inline std::shared_ptr<ObjectSchema> object() { 
        return std::make_shared<ObjectSchema>(); 
    }
    
    inline std::shared_ptr<UnionSchema> oneOf(std::vector<std::shared_ptr<Schema>> schemas) { 
        return std::make_shared<UnionSchema>(schemas); 
    }
    
    // Utility schemas
    inline std::shared_ptr<StringSchema> dealId() {
        auto schema = string();
        schema->regex(R"(DEAL_\d{6})");
        return schema;
    }
    
    inline std::shared_ptr<StringSchema> timestamp() {
        auto schema = string();
        schema->regex(R"(\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z)");
        return schema;
    }
    
    inline std::shared_ptr<NumberSchema> positiveNumber() {
        auto schema = number();
        schema->positive();
        return schema;
    }
    
    inline std::shared_ptr<NumberSchema> percentage() {
        auto schema = number();
        schema->min(0.0);
        schema->max(1.0);
        return schema;
    }
    
    inline std::shared_ptr<StringSchema> rating() {
        auto schema = string();
        schema->oneOf({"AAA", "AA+", "AA", "AA-", "A+", "A", "A-", 
                      "BBB+", "BBB", "BBB-", "BB+", "BB", "BB-", 
                      "B+", "B", "B-", "CCC+", "CCC", "CCC-", 
                      "CC", "C", "D", "NR"});
        return schema;
    }
}

/**
 * @brief Pre-defined schemas for common API objects
 */
class CommonSchemas {
public:
    // Deal tranche schema
    static std::shared_ptr<ObjectSchema> tranche() {
        auto nameSchema = z::string();
        nameSchema->min(1);
        
        auto rateSchema = z::number();
        rateSchema->min(0.0);
        rateSchema->max(1.0);
        
        auto schema = z::object();
        schema->property("name", nameSchema, true);
        schema->property("balance", z::positiveNumber(), true);
        schema->property("rate", rateSchema, true);
        schema->property("rating", z::rating(), false);
        schema->property("subordination", z::percentage(), false);
        schema->property("creditEnhancement", z::percentage(), false);
        return schema;
    }
    
    // Deal structure schema
    static std::shared_ptr<ObjectSchema> dealStructure() {
        auto tranchesArray = z::array(tranche());
        tranchesArray->min(1);
        
        auto schema = z::object();
        schema->property("tranches", tranchesArray, true);
        schema->property("totalBalance", z::positiveNumber(), false);
        schema->property("seniorSubordination", z::percentage(), false);
        schema->property("overcollateralization", z::percentage(), false);
        return schema;
    }
    
    // Asset pool schema
    static std::shared_ptr<ObjectSchema> assetPool() {
        auto assetCountSchema = z::number();
        assetCountSchema->integer();
        assetCountSchema->min(1);
        
        auto schema = z::object();
        schema->property("total_balance", z::positiveNumber(), true);
        schema->property("asset_count", assetCountSchema, true);
        schema->property("weighted_average_rate", z::percentage(), true);
        schema->property("weighted_average_life", z::positiveNumber(), false);
        schema->property("default_rate", z::percentage(), false);
        schema->property("recovery_rate", z::percentage(), false);
        return schema;
    }
    
    // Vector parameter schema (132-period support)
    static std::shared_ptr<ObjectSchema> vectorParameter() {
        return z::object()
            ->property("type", z::string()->oneOf({"vector", "constant", "seasonal", "ramp"}), true)
            ->property("baseValue", z::number(), false)
            ->property("values", z::array(z::number())->max(132), false)
            ->property("description", z::string(), false)
            ->property("startDate", z::timestamp(), false);
    }
    
    // Performance assumptions schema
    static std::shared_ptr<ObjectSchema> performanceAssumptions() {
        return z::object()
            ->property("default_rate", z::oneOf({vectorParameter(), z::percentage()}), false)
            ->property("prepayment_rate", z::oneOf({vectorParameter(), z::percentage()}), false)
            ->property("recovery_rate", z::oneOf({vectorParameter(), z::percentage()}), false)
            ->property("interest_rates", z::oneOf({vectorParameter(), z::percentage()}), false)
            ->property("servicing_fee", z::oneOf({vectorParameter(), z::percentage()}), false);
    }
    
    // Main deal schema
    static std::shared_ptr<ObjectSchema> deal() {
        return z::object()
            ->property("name", z::string()->min(1)->max(255), true)
            ->property("type", z::string()->oneOf({"consumer_abs", "clo", "commercial_mortgage", 
                                                 "auto_loan", "credit_card", "student_loan"}), true)
            ->property("structure", dealStructure(), false)
            ->property("assets", assetPool(), false)
            ->property("assumptions", performanceAssumptions(), false)
            ->property("status", z::string()->oneOf({"active", "inactive", "closed", "defaulted"}), false)
            ->property("description", z::string()->max(1000), false)
            ->property("tags", z::array(z::string()), false);
    }
    
    // Scenario schema
    static std::shared_ptr<ObjectSchema> scenario() {
        return z::object()
            ->property("name", z::string()->min(1), true)
            ->property("description", z::string(), false)
            ->property("assumptions", performanceAssumptions(), true)
            ->property("shocks", z::array(z::object()
                ->property("parameter", z::string(), true)
                ->property("type", z::string()->oneOf({"absolute", "relative", "shock"}), true)
                ->property("value", z::number(), true)
                ->property("period", z::number()->integer()->min(1)->max(132), false)
            ), false);
    }
};

} // namespace schema
} // namespace api
} // namespace structura