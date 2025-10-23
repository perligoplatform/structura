#pragma once

#include "core/financial_types.h"
#include "core/account.h"
#include "entities/bond.h"
#include "entities/fee.h"
#include <nlohmann/json.hpp>

namespace Structura {

class Deal {
private:
    std::string id_;
    std::string name_;
    Date closing_date_;
    Date maturity_date_;
    
    EntityMap<Bond> bonds_;
    EntityMap<Account> accounts_;  // Updated to use new Account system
    EntityMap<Fee> fees_;
    
public:
    Deal(std::string id, std::string name, Date closing, Date maturity)
        : id_(std::move(id)), name_(std::move(name))
        , closing_date_(closing), maturity_date_(maturity) {}
    
    // Accessors
    const std::string& getId() const { return id_; }
    const std::string& getName() const { return name_; }
    Date getClosingDate() const { return closing_date_; }
    Date getMaturityDate() const { return maturity_date_; }
    
    // Entity management
    void addBond(Ptr<Bond> bond) { bonds_[bond->getId()] = bond; }
    void addAccount(Ptr<Account> account) { accounts_[account->getName()] = account; }  // Updated to use Account name
    void addFee(Ptr<Fee> fee) { fees_[fee->getId()] = fee; }
    
    Ptr<Bond> getBond(const std::string& id) const {
        auto it = bonds_.find(id);
        return it != bonds_.end() ? it->second : nullptr;
    }
    
    Ptr<Account> getAccount(const std::string& name) const {
        auto it = accounts_.find(name);
        return it != accounts_.end() ? it->second : nullptr;
    }
    
    Ptr<Fee> getFee(const std::string& id) const {
        auto it = fees_.find(id);
        return it != fees_.end() ? it->second : nullptr;
    }
    
    const EntityMap<Bond>& getBonds() const { return bonds_; }
    const EntityMap<Account>& getAccounts() const { return accounts_; }
    const EntityMap<Fee>& getFees() const { return fees_; }
    
    // Utility methods
    Balance getTotalBondBalance() const {
        Balance total = 0.0;
        for (const auto& [id, bond] : bonds_) {
            total += bond->getCurrentBalance();
        }
        return total;
    }
    
    Balance getTotalAccountBalance() const {
        Balance total = 0.0;
        for (const auto& [name, account] : accounts_) {
            total += account->getBalance();  // Updated to use getBalance()
        }
        return total;
    }
    
    // JSON support
    static std::unique_ptr<Deal> fromJsonFile(const std::string& filename);
    static std::unique_ptr<Deal> fromJsonString(const std::string& json_str);
    nlohmann::json toJson() const;
};

} // namespace Structura