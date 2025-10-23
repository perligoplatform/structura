#pragma once

#include <string>
#include <chrono>
#include <memory>
#include <vector>
#include <unordered_map>
#include <ql/time/date.hpp>

namespace Structura {

// Import QuantLib types
#include <ql/types.hpp>
using Balance = QuantLib::Real;  // Consistent with financial_types.h

// Use QuantLib Date directly - clean, no conditionals  
using Date = QuantLib::Date;

// Helper functions for QuantLib Date
namespace DateUtils {
    Date makeDate(int year, int month, int day);
    std::string toString(const Date& date);
    int year(const Date& date);
    int month(const Date& date);
    int dayOfMonth(const Date& date);
}

// Forward declarations
class Deal;
class Bond;
class Account;
class Fee;

// Smart pointer aliases
template<typename T>
using Ptr = std::shared_ptr<T>;

template<typename T>
using EntityMap = std::unordered_map<std::string, Ptr<T>>;

// Entity base class
class Entity {
protected:
    std::string id_;
    std::string name_;
    
public:
    Entity(std::string id, std::string name)
        : id_(std::move(id)), name_(std::move(name)) {}
    
    virtual ~Entity() = default;
    
    const std::string& getId() const { return id_; }
    const std::string& getName() const { return name_; }
    
    virtual Balance getCurrentBalance() const = 0;
};

} // namespace Structura
