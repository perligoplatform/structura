#include "assets/asset_base.h"
#include <cmath>
#include <stdexcept>

namespace Structura {

// String conversion utilities
std::string toString(Status status) {
    switch (status) {
        case Status::Current: return "Current";
        case Status::Defaulted: return "Defaulted";
        default: return "Unknown";
    }
}

std::string toString(AmortPlan plan) {
    switch (plan) {
        case AmortPlan::Level: return "Level";
        case AmortPlan::Even: return "Even";
        case AmortPlan::I_P: return "I_P";
        case AmortPlan::F_P: return "F_P";
        case AmortPlan::Balloon: return "Balloon";
        default: return "Unknown";
    }
}

std::string toString(AmortRule rule) {
    switch (rule) {
        case AmortRule::DecliningBalance: return "DecliningBalance";
        case AmortRule::StraightLine: return "StraightLine";
        default: return "Unknown";
    }
}

// Core financial calculation: calculate payment amount (Annuity/Level mortgage)
Amount calculatePayment(Balance balance, Rate rate, int periods) {
    if (rate == 0.0) {
        return balance / periods;
    }
    
    // Standard annuity payment formula: PMT = PV * r * (1+r)^n / ((1+r)^n - 1)
    double r = static_cast<double>(rate);
    double n = static_cast<double>(periods);
    double power = std::pow(1.0 + r, n);
    
    return static_cast<Balance>(balance) * (r * power) / (power - 1.0);
}

// Core calculation: determine principal and interest portions of payment
std::pair<Balance, Balance> calculatePrincipalInterest(
    AmortPlan plan, Balance balance, Rate rate, 
    int originalTerm, int remainingTerm, 
    const std::pair<Balance, int>& amortInfo) {
    
    Balance interestAccrued = balance * rate;
    Amount payment = calculatePayment(balance, rate, remainingTerm);
    int periodsPassed = originalTerm - remainingTerm;
    
    switch (plan) {
        case AmortPlan::Level: {
            Balance principalPayment = payment - interestAccrued;
            return {interestAccrued, principalPayment};
        }
        
        case AmortPlan::Even: {
            Balance principalPayment = balance / remainingTerm;
            return {interestAccrued, principalPayment};
        }
        
        case AmortPlan::I_P: {
            if (remainingTerm == 1) {
                // Last payment - pay all principal
                return {interestAccrued, balance};
            } else {
                // Interest only
                return {interestAccrued, 0.0};
            }
        }
        
        case AmortPlan::Balloon: {
            if (remainingTerm == 1) {
                // Final balloon payment
                return {interestAccrued, balance};
            } else {
                // Regular payment based on amortization schedule
                Balance balloonPayment = calculatePayment(balance, rate, amortInfo.second - periodsPassed);
                Balance principalPayment = balloonPayment - interestAccrued;
                return {interestAccrued, principalPayment};
            }
        }
        
        default:
            throw std::runtime_error("Unsupported amortization plan: " + toString(plan));
    }
}

} // namespace Structura