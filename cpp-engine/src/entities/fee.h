#pragma once

#include "core/financial_types.h"
#include "core/types.h"  // For Entity base class

namespace Structura {

class Fee : public Entity {
private:
    Balance due_amount_;
    Rate fee_rate_;
    
public:
    Fee(std::string id, std::string name, Rate rate = 0.0)
        : Entity(std::move(id), std::move(name))
        , due_amount_(0.0)
        , fee_rate_(rate) {}
    
    Balance getCurrentBalance() const override { return due_amount_; }
    Balance getDueAmount() const { return due_amount_; }
    Rate getFeeRate() const { return fee_rate_; }
    
    void calculateFee(Balance base_amount) {
        due_amount_ += base_amount * fee_rate_;
    }
    
    Amount payFee(Amount amount) {
        Amount actual_payment = std::min(amount, due_amount_);
        due_amount_ -= actual_payment;
        return actual_payment;
    }
};

} // namespace Structura