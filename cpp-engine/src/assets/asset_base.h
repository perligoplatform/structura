#pragma once

#include "core/financial_types.h"
#include <string>
#include <vector>
#include <map>
#include <optional>
#include <variant>

namespace Structura {

// Forward declarations
class InterestRate;

// Basic asset status enumeration
enum class Status {
    Current,
    Defaulted,
    PaidOff
};

// Amortization plan types - foundation for payment calculations
enum class AmortPlan {
    Level,        // Fixed payment (French system)
    Even,         // Even principal distribution
    I_P,          // Interest only, principal at end
    F_P,          // Fee based
    Balloon       // Balloon payment
};

// Amortization rules for asset depreciation
enum class AmortRule {
    DecliningBalance,
    StraightLine
};

// Prepayment penalty types
enum class PrepayPenaltyType {
    ByTerm,       // Different rates based on term
    FixAmount,    // Fixed penalty amount
    FixPct,       // Fixed percentage penalty
    Sliding,      // Sliding scale penalty
    StepDown      // Step-down penalty schedule
};

// Obligor information - borrower/debtor details
class Obligor {
private:
    std::string id_;
    std::vector<std::string> tags_;
    std::map<std::string, std::variant<std::string, double>> fields_;

public:
    Obligor(std::string id) : id_(std::move(id)) {}
    
    const std::string& getId() const { return id_; }
    const std::vector<std::string>& getTags() const { return tags_; }
    
    void addTag(const std::string& tag) { tags_.push_back(tag); }
    void setField(const std::string& key, const std::string& value) { fields_[key] = value; }
    void setField(const std::string& key, double value) { fields_[key] = value; }
    
    std::optional<std::string> getStringField(const std::string& key) const {
        auto it = fields_.find(key);
        if (it != fields_.end() && std::holds_alternative<std::string>(it->second)) {
            return std::get<std::string>(it->second);
        }
        return std::nullopt;
    }
    
    std::optional<double> getDoubleField(const std::string& key) const {
        auto it = fields_.find(key);
        if (it != fields_.end() && std::holds_alternative<double>(it->second)) {
            return std::get<double>(it->second);
        }
        return std::nullopt;
    }
};

// Original asset information - fundamental asset characteristics
class OriginalInfo {
private:
    Balance originBalance_;
    Rate originRate_;
    int originTerm_;
    Date startDate_;
    Period period_;
    AmortPlan prinType_;
    std::optional<Obligor> obligor_;

public:
    OriginalInfo(Balance balance, Rate rate, int term, Date startDate, 
                Period period, AmortPlan prinType)
        : originBalance_(balance), originRate_(rate), originTerm_(term)
        , startDate_(startDate), period_(period), prinType_(prinType) {}
        
    // Getters
    Balance getOriginBalance() const { return originBalance_; }
    Rate getOriginRate() const { return originRate_; }
    int getOriginTerm() const { return originTerm_; }
    Date getStartDate() const { return startDate_; }
    Period getPeriod() const { return period_; }
    AmortPlan getPrinType() const { return prinType_; }
    
    // Obligor management
    void setObligor(const Obligor& obligor) { obligor_ = obligor; }
    const std::optional<Obligor>& getObligor() const { return obligor_; }
    bool hasObligor() const { return obligor_.has_value(); }
};

// Forward declarations for asset types
class Mortgage;
class Loan;
class Installment;
class Lease;
class FixedAsset;
class Receivable;
class ProjectedCashFlow;

// Asset union type - can hold any asset type
using AssetUnion = std::variant<
    Mortgage, 
    Loan, 
    Installment, 
    Lease, 
    FixedAsset, 
    Receivable, 
    ProjectedCashFlow
>;

// Utility functions for string conversion
std::string toString(Status status);
std::string toString(AmortPlan plan);
std::string toString(AmortRule rule);

// Financial calculation functions
Amount calculatePayment(Balance balance, Rate rate, int periods);
std::pair<Balance, Balance> calculatePrincipalInterest(
    AmortPlan plan, Balance balance, Rate rate, 
    int originalTerm, int remainingTerm, 
    const std::pair<Balance, int>& amortInfo);

} // namespace Structura