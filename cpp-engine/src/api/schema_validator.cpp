#include "schema_validator.h"
#include <sstream>
#include <algorithm>

namespace structura {
namespace api {
namespace schema {

// StringSchema implementation
ValidationResult StringSchema::validate(const json& data, const std::string& path) const {
    ValidationResult result;
    result.sanitized_data = data;
    
    if (data.is_null() && optional_) {
        if (default_value_.has_value()) {
            result.sanitized_data = default_value_.value();
        }
        return result;
    }
    
    if (!data.is_string()) {
        result.addError(path, "Expected string, got " + std::string(data.type_name()), 
                       "invalid_type", data, "string");
        return result;
    }
    
    std::string value = data.get<std::string>();
    
    // Length validation
    if (value.length() < min_length_) {
        result.addError(path, "String too short. Expected at least " + std::to_string(min_length_) + 
                       " characters, got " + std::to_string(value.length()), 
                       "too_small", data, min_length_);
        return result;
    }
    
    if (value.length() > max_length_) {
        result.addError(path, "String too long. Expected at most " + std::to_string(max_length_) + 
                       " characters, got " + std::to_string(value.length()), 
                       "too_big", data, max_length_);
        return result;
    }
    
    // Pattern validation
    if (pattern_.has_value() && !std::regex_match(value, pattern_.value())) {
        result.addError(path, "String does not match required pattern", 
                       "invalid_string", data);
        return result;
    }
    
    // Enum validation
    if (!enum_values_.empty()) {
        auto it = std::find(enum_values_.begin(), enum_values_.end(), value);
        if (it == enum_values_.end()) {
            json expected_array = json::array();
            for (const auto& val : enum_values_) {
                expected_array.push_back(val);
            }
            result.addError(path, "Invalid enum value. Expected one of: " + expected_array.dump(), 
                           "invalid_enum_value", data, expected_array);
            return result;
        }
    }
    
    return result;
}

json StringSchema::getSchemaDefinition() const {
    json schema = {
        {"type", "string"}
    };
    
    if (min_length_ > 0) schema["minLength"] = min_length_;
    if (max_length_ < SIZE_MAX) schema["maxLength"] = max_length_;
    if (pattern_.has_value()) schema["pattern"] = "regex pattern";
    if (!enum_values_.empty()) schema["enum"] = enum_values_;
    if (description_.has_value()) schema["description"] = description_.value();
    if (default_value_.has_value()) schema["default"] = default_value_.value();
    
    return schema;
}

// NumberSchema implementation
ValidationResult NumberSchema::validate(const json& data, const std::string& path) const {
    ValidationResult result;
    result.sanitized_data = data;
    
    if (data.is_null() && optional_) {
        if (default_value_.has_value()) {
            result.sanitized_data = default_value_.value();
        }
        return result;
    }
    
    if (!data.is_number()) {
        result.addError(path, "Expected number, got " + std::string(data.type_name()), 
                       "invalid_type", data, "number");
        return result;
    }
    
    double value = data.get<double>();
    
    // Integer validation
    if (integer_only_ && value != std::floor(value)) {
        result.addError(path, "Expected integer, got decimal number", 
                       "invalid_type", data, "integer");
        return result;
    }
    
    // Range validation
    if (min_.has_value() && value < min_.value()) {
        result.addError(path, "Number too small. Expected at least " + std::to_string(min_.value()) + 
                       ", got " + std::to_string(value), 
                       "too_small", data, min_.value());
        return result;
    }
    
    if (max_.has_value() && value > max_.value()) {
        result.addError(path, "Number too big. Expected at most " + std::to_string(max_.value()) + 
                       ", got " + std::to_string(value), 
                       "too_big", data, max_.value());
        return result;
    }
    
    return result;
}

json NumberSchema::getSchemaDefinition() const {
    json schema = {
        {"type", integer_only_ ? "integer" : "number"}
    };
    
    if (min_.has_value()) schema["minimum"] = min_.value();
    if (max_.has_value()) schema["maximum"] = max_.value();
    if (description_.has_value()) schema["description"] = description_.value();
    if (default_value_.has_value()) schema["default"] = default_value_.value();
    
    return schema;
}

// BooleanSchema implementation
ValidationResult BooleanSchema::validate(const json& data, const std::string& path) const {
    ValidationResult result;
    result.sanitized_data = data;
    
    if (data.is_null() && optional_) {
        if (default_value_.has_value()) {
            result.sanitized_data = default_value_.value();
        }
        return result;
    }
    
    if (!data.is_boolean()) {
        result.addError(path, "Expected boolean, got " + std::string(data.type_name()), 
                       "invalid_type", data, "boolean");
        return result;
    }
    
    return result;
}

json BooleanSchema::getSchemaDefinition() const {
    json schema = {
        {"type", "boolean"}
    };
    
    if (description_.has_value()) schema["description"] = description_.value();
    if (default_value_.has_value()) schema["default"] = default_value_.value();
    
    return schema;
}

// ArraySchema implementation
ValidationResult ArraySchema::validate(const json& data, const std::string& path) const {
    ValidationResult result;
    result.sanitized_data = json::array();
    
    if (data.is_null() && optional_) {
        if (default_value_.has_value()) {
            result.sanitized_data = default_value_.value();
        }
        return result;
    }
    
    if (!data.is_array()) {
        result.addError(path, "Expected array, got " + std::string(data.type_name()), 
                       "invalid_type", data, "array");
        return result;
    }
    
    const auto& array = data.get<json::array_t>();
    
    // Size validation
    if (array.size() < min_items_) {
        result.addError(path, "Array too small. Expected at least " + std::to_string(min_items_) + 
                       " items, got " + std::to_string(array.size()), 
                       "too_small", data, min_items_);
        return result;
    }
    
    if (array.size() > max_items_) {
        result.addError(path, "Array too big. Expected at most " + std::to_string(max_items_) + 
                       " items, got " + std::to_string(array.size()), 
                       "too_big", data, max_items_);
        return result;
    }
    
    // Validate each item
    json::array_t sanitized_array;
    for (size_t i = 0; i < array.size(); ++i) {
        std::string item_path = path + "[" + std::to_string(i) + "]";
        auto item_result = item_schema_->validate(array[i], item_path);
        
        if (!item_result.valid) {
            result.valid = false;
            result.errors.insert(result.errors.end(), 
                               item_result.errors.begin(), 
                               item_result.errors.end());
        } else {
            sanitized_array.push_back(item_result.sanitized_data);
        }
    }
    
    if (result.valid) {
        result.sanitized_data = sanitized_array;
    }
    
    return result;
}

json ArraySchema::getSchemaDefinition() const {
    json schema = {
        {"type", "array"},
        {"items", item_schema_->getSchemaDefinition()}
    };
    
    if (min_items_ > 0) schema["minItems"] = min_items_;
    if (max_items_ < SIZE_MAX) schema["maxItems"] = max_items_;
    if (description_.has_value()) schema["description"] = description_.value();
    
    return schema;
}

// ObjectSchema implementation
ValidationResult ObjectSchema::validate(const json& data, const std::string& path) const {
    ValidationResult result;
    result.sanitized_data = json::object();
    
    if (data.is_null() && optional_) {
        if (default_value_.has_value()) {
            result.sanitized_data = default_value_.value();
        }
        return result;
    }
    
    if (!data.is_object()) {
        result.addError(path, "Expected object, got " + std::string(data.type_name()), 
                       "invalid_type", data, "object");
        return result;
    }
    
    const auto& object = data.get<json::object_t>();
    json::object_t sanitized_object;
    
    // Check required fields
    for (const auto& required_field : required_fields_) {
        if (object.find(required_field) == object.end()) {
            result.addError(path.empty() ? required_field : path + "." + required_field, 
                           "Required field '" + required_field + "' is missing", 
                           "missing_field");
        }
    }
    
    // Validate known properties
    for (const auto& [key, value] : object) {
        std::string field_path = path.empty() ? key : path + "." + key;
        
        auto prop_it = properties_.find(key);
        if (prop_it != properties_.end()) {
            // Known property - validate it
            auto field_result = prop_it->second->validate(value, field_path);
            
            if (!field_result.valid) {
                result.valid = false;
                result.errors.insert(result.errors.end(), 
                                   field_result.errors.begin(), 
                                   field_result.errors.end());
            } else {
                sanitized_object[key] = field_result.sanitized_data;
            }
        } else if (!allow_additional_) {
            // Unknown property and additional properties not allowed
            result.addError(field_path, "Unknown property '" + key + "'", 
                           "unrecognized_keys", value);
        } else {
            // Unknown property but additional properties allowed
            sanitized_object[key] = value;
        }
    }
    
    if (result.valid) {
        result.sanitized_data = sanitized_object;
    }
    
    return result;
}

json ObjectSchema::getSchemaDefinition() const {
    json schema = {
        {"type", "object"},
        {"properties", json::object()},
        {"required", required_fields_},
        {"additionalProperties", allow_additional_}
    };
    
    for (const auto& [name, prop_schema] : properties_) {
        schema["properties"][name] = prop_schema->getSchemaDefinition();
    }
    
    if (description_.has_value()) schema["description"] = description_.value();
    
    return schema;
}

// UnionSchema implementation
ValidationResult UnionSchema::validate(const json& data, const std::string& path) const {
    ValidationResult result;
    std::vector<ValidationResult> all_results;
    
    // Try each schema until one succeeds
    for (const auto& schema : schemas_) {
        auto attempt_result = schema->validate(data, path);
        if (attempt_result.valid) {
            return attempt_result; // First successful validation wins
        }
        all_results.push_back(std::move(attempt_result));
    }
    
    // None succeeded - combine all error messages
    result.valid = false;
    result.addError(path, "Value does not match any of the expected types", "invalid_union");
    
    // Add details from each failed attempt
    for (size_t i = 0; i < all_results.size(); ++i) {
        for (const auto& error : all_results[i].errors) {
            result.addError(error.path + " (option " + std::to_string(i + 1) + ")", 
                           error.message, error.code, error.received, error.expected);
        }
    }
    
    return result;
}

json UnionSchema::getSchemaDefinition() const {
    json schema = {
        {"anyOf", json::array()}
    };
    
    for (const auto& sub_schema : schemas_) {
        schema["anyOf"].push_back(sub_schema->getSchemaDefinition());
    }
    
    if (description_.has_value()) schema["description"] = description_.value();
    
    return schema;
}

} // namespace schema
} // namespace api
} // namespace structura