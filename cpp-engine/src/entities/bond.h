#pragma once

#include "core/financial_types.h"
#include "core/types.h"  // For Entity base class

namespace Structura {

class Bond : public Entity {
private:
    Balance original_balance_;
    Balance current_balance_;
    Rate current_rate_;
    Date maturity_date_;
    
public:
    Bond(std::string id, std::string name, Balance original_balance, 
         Rate rate, Date maturity)
        : Entity(std::move(id), std::move(name))
        , original_balance_(original_balance)
        , current_balance_(original_balance)
        , current_rate_(rate)
        , maturity_date_(maturity) {}
    
    Balance getCurrentBalance() const override { return current_balance_; }
    Balance getOriginalBalance() const { return original_balance_; }
    Rate getCurrentRate() const { return current_rate_; }
    Date getMaturityDate() const { return maturity_date_; }
    
    // Basic operations
    void payPrincipal(Amount amount) {
        current_balance_ = std::max(Balance(0.0), current_balance_ - amount);
    }
    
    Balance calculateInterest(Date from_date, Date to_date) const {
        int days = to_date - from_date;
        return current_balance_ * current_rate_ * days / 365.0L;
    }
};

} // namespace Structura