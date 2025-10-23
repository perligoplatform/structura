#include "bond_deal.h"
#include <algorithm>
#include <cmath>
#include <stdexcept>
#include <sstream>

namespace Structura {

// BondType conversion
std::string bondTypeToString(BondType type) {
    switch (type) {
        case BondType::CORPORATE: return "Corporate";
        case BondType::GOVERNMENT: return "Government";
        case BondType::MUNICIPAL: return "Municipal";
        case BondType::HIGH_YIELD: return "High Yield";
        case BondType::INVESTMENT_GRADE: return "Investment Grade";
        default: return "Unknown";
    }
}

// CreditRating conversion and utilities
std::string creditRatingToString(CreditRating rating) {
    switch (rating) {
        case CreditRating::AAA: return "AAA";
        case CreditRating::AA_PLUS: return "AA+";
        case CreditRating::AA: return "AA";
        case CreditRating::AA_MINUS: return "AA-";
        case CreditRating::A_PLUS: return "A+";
        case CreditRating::A: return "A";
        case CreditRating::A_MINUS: return "A-";
        case CreditRating::BBB_PLUS: return "BBB+";
        case CreditRating::BBB: return "BBB";
        case CreditRating::BBB_MINUS: return "BBB-";
        case CreditRating::BB_PLUS: return "BB+";
        case CreditRating::BB: return "BB";
        case CreditRating::BB_MINUS: return "BB-";
        case CreditRating::B_PLUS: return "B+";
        case CreditRating::B: return "B";
        case CreditRating::B_MINUS: return "B-";
        case CreditRating::CCC_PLUS: return "CCC+";
        case CreditRating::CCC: return "CCC";
        case CreditRating::CCC_MINUS: return "CCC-";
        case CreditRating::CC: return "CC";
        case CreditRating::C: return "C";
        case CreditRating::D: return "D";
        case CreditRating::NOT_RATED: return "NR";
        default: return "Unknown";
    }
}

bool isInvestmentGrade(CreditRating rating) {
    return rating <= CreditRating::BBB_MINUS;
}

// StructuredBond implementation
StructuredBond::StructuredBond(const std::string& id, BondType type, CreditRating rating,
                               Amount notional, Rate coupon, Date maturity, PaymentFrequency freq)
    : bond_id(id), bond_type(type), rating(rating), notional(notional), 
      coupon_rate(coupon), maturity_date(maturity), payment_freq(freq),
      outstanding_balance(notional), payments_missed(0), is_defaulted(false) {
    
    // Set default credit parameters based on rating
    switch (rating) {
        case CreditRating::AAA:
            probability_of_default = 0.0001;
            loss_given_default = 0.20;
            break;
        case CreditRating::AA:
        case CreditRating::AA_PLUS:
        case CreditRating::AA_MINUS:
            probability_of_default = 0.0005;
            loss_given_default = 0.25;
            break;
        case CreditRating::A:
        case CreditRating::A_PLUS:
        case CreditRating::A_MINUS:
            probability_of_default = 0.002;
            loss_given_default = 0.30;
            break;
        case CreditRating::BBB:
        case CreditRating::BBB_PLUS:
        case CreditRating::BBB_MINUS:
            probability_of_default = 0.005;
            loss_given_default = 0.35;
            break;
        case CreditRating::BB:
        case CreditRating::BB_PLUS:
        case CreditRating::BB_MINUS:
            probability_of_default = 0.015;
            loss_given_default = 0.40;
            break;
        case CreditRating::B:
        case CreditRating::B_PLUS:
        case CreditRating::B_MINUS:
            probability_of_default = 0.040;
            loss_given_default = 0.50;
            break;
        default:
            probability_of_default = 0.100;
            loss_given_default = 0.60;
            break;
    }
    
    recovery_rate = 1.0 - loss_given_default;
}

Amount StructuredBond::calculateCouponPayment() const {
    if (is_defaulted) return 0.0;
    
    double periods_per_year = static_cast<double>(payment_freq);
    return outstanding_balance * (coupon_rate / periods_per_year);
}

Amount StructuredBond::calculateAccruedInterest(Date as_of_date) const {
    if (is_defaulted || as_of_date <= last_payment_date) return 0.0;
    
    // Simplified: assume 30/360 day count
    int days_since_payment = (as_of_date - last_payment_date);
    double periods_per_year = static_cast<double>(payment_freq);
    int days_per_period = 360 / static_cast<int>(periods_per_year);
    
    double accrual_factor = static_cast<double>(days_since_payment) / days_per_period;
    return outstanding_balance * coupon_rate * accrual_factor;
}

double StructuredBond::calculateYieldToMaturity(Amount market_price) const {
    if (market_price <= 0 || outstanding_balance <= 0) return 0.0;
    
    // Simplified YTM calculation - would use Newton-Raphson in practice
    double periods_per_year = static_cast<double>(payment_freq);
    double coupon_payment = calculateCouponPayment();
    double price_ratio = market_price / outstanding_balance;
    
    if (price_ratio > 1.0) {
        return coupon_rate * (1.0 - (price_ratio - 1.0) * 0.5);
    } else {
        return coupon_rate * (1.0 + (1.0 - price_ratio) * 0.5);
    }
}

double StructuredBond::calculateModifiedDuration(Rate yield) const {
    if (yield <= 0) return 0.0;
    
    // Simplified duration calculation
    double periods_per_year = static_cast<double>(payment_freq);
    double years_to_maturity = 5.0; // Simplified
    
    return years_to_maturity / (1.0 + yield / periods_per_year);
}

void StructuredBond::updateCreditRating(CreditRating new_rating) {
    rating = new_rating;
    
    // Update credit parameters
    if (new_rating >= CreditRating::BB_PLUS) {
        probability_of_default *= 1.5; // Downgrade increases PD
    }
}

void StructuredBond::markDefault() {
    is_defaulted = true;
    probability_of_default = 1.0;
}

Amount StructuredBond::calculateExpectedLoss() const {
    return outstanding_balance * probability_of_default * loss_given_default;
}

// BondWaterfall implementation
std::map<std::string, Amount> BondWaterfall::distributeFunds(
    Amount available_funds, const std::map<std::string, StructuredBond>& bonds) const {
    
    std::map<std::string, Amount> distributions;
    Amount remaining_funds = available_funds;
    
    // Distribute in order of seniority
    for (const auto& bond_id : payment_sequence) {
        if (remaining_funds <= 0) break;
        
        auto bond_it = bonds.find(bond_id);
        if (bond_it == bonds.end()) continue;
        
        const auto& bond = bond_it->second;
        Amount required_payment = bond.calculateCouponPayment();
        Amount payment = std::min(required_payment, remaining_funds);
        
        distributions[bond_id] = payment;
        remaining_funds -= payment;
    }
    
    return distributions;
}

Amount BondWaterfall::calculateRequiredReserves(const std::map<std::string, StructuredBond>& bonds) const {
    Amount total_reserves = 0.0;
    
    for (const auto& [bond_id, bond] : bonds) {
        auto reserve_it = reserves_required.find(bond_id);
        if (reserve_it != reserves_required.end()) {
            total_reserves += reserve_it->second;
        } else {
            // Default reserve calculation
            total_reserves += bond.outstanding_balance * 0.02; // 2% reserve
        }
    }
    
    return total_reserves;
}

bool BondWaterfall::checkCoverageRatios(Amount pool_cashflow, Amount bond_payments) const {
    if (bond_payments <= 0) return true;
    
    double coverage_ratio = pool_cashflow / bond_payments;
    return coverage_ratio >= 1.10; // 110% coverage requirement
}

// BondDeal implementation
BondDeal::BondDeal(const DealInfo& deal_info) 
    : DealBase(deal_info), underlying_pool_(nullptr), weighted_average_rating_(0.0), portfolio_duration_(0.0) {
    
    // Create deal-specific accounts
    reserve_account_id_ = "reserve_" + deal_info.deal_id;
    excess_spread_account_id_ = "excess_spread_" + deal_info.deal_id;
    trustee_account_id_ = "trustee_" + deal_info.deal_id;
    
    // Add accounts to the base class account map
    accounts_[reserve_account_id_] = Account(reserve_account_id_, AccountType::RESERVE);
    accounts_[excess_spread_account_id_] = Account(excess_spread_account_id_, AccountType::CASH);
    accounts_[trustee_account_id_] = Account(trustee_account_id_, AccountType::CASH);
}

void BondDeal::setUnderlyingPool(Pool<Bond>* pool) {
    underlying_pool_ = pool;
}

void BondDeal::addBond(const StructuredBond& bond) {
    bonds_[bond.bond_id] = bond;
    updatePortfolioMetrics();
}

void BondDeal::setupWaterfall(const BondWaterfall& waterfall) {
    waterfall_ = waterfall;
}

void BondDeal::setupCreditEnhancement(Amount reserve_target, Rate excess_spread_target) {
    if (accounts_.find(reserve_account_id_) != accounts_.end()) {
        accounts_[reserve_account_id_].deposit(reserve_target);
    }
    
    // Store target rates for ongoing monitoring
    // In practice, would store these in member variables
}

bool BondDeal::validateDeal() const {
    // Check that we have bonds
    if (bonds_.empty()) return false;
    
    // Check that waterfall is configured
    if (waterfall_.payment_sequence.empty()) return false;
    
    // Check that all bonds in waterfall exist
    for (const auto& bond_id : waterfall_.payment_sequence) {
        if (bonds_.find(bond_id) == bonds_.end()) return false;
    }
    
    // Check that reserve accounts exist
    if (accounts_.find(reserve_account_id_) == accounts_.end()) return false;
    
    return true;
}

void BondDeal::processPaymentDate(Date payment_date) {
    if (status_ != DealStatus::ACTIVE) return;
    
    // Collect cashflow from underlying pool
    Amount pool_cashflow = collectPoolCashflow(payment_date);
    
    // Process bond payments
    processBondPayments(payment_date);
    
    // Distribute cashflow through waterfall
    distributeCashflow(pool_cashflow, payment_date);
    
    // Update credit metrics and reserves
    updateCreditMetrics(payment_date);
    updateReserves(payment_date);
    
    // Check coverage tests
    checkCoverageTests(payment_date);
    
    // Update last payment date
    last_payment_date_ = payment_date;
}

std::map<std::string, Amount> BondDeal::calculateMetrics(Date as_of_date) const {
    std::map<std::string, Amount> metrics;
    
    metrics["total_notional"] = calculateTotalNotional();
    metrics["expected_loss"] = calculateExpectedLoss();
    metrics["portfolio_duration"] = portfolio_duration_;
    metrics["weighted_avg_rating"] = weighted_average_rating_;
    metrics["excess_spread"] = calculateExcessSpread();
    metrics["portfolio_yield"] = calculatePortfolioYield();
    
    // Reserve levels
    auto reserve_it = accounts_.find(reserve_account_id_);
    if (reserve_it != accounts_.end()) {
        metrics["reserve_balance"] = reserve_it->second.getBalance();
    }
    
    return metrics;
}

std::string BondDeal::getStatus() const {
    if (status_ == DealStatus::ACTIVE) {
        // Check for any triggers that might change status
        double avg_rating = calculateWeightedAverageRating();
        if (avg_rating < 7.0) { // Below BBB-
            return "Active - Credit Watch";
        }
        
        Amount expected_loss = calculateExpectedLoss();
        Amount total_notional = calculateTotalNotional();
        if (total_notional > 0 && expected_loss / total_notional > 0.05) {
            return "Active - High Risk";
        }
        
        return "Active - Performing";
    }
    
    return dealStatusToString(status_);
}

void BondDeal::updateAssumptions(const std::map<std::string, double>& new_assumptions) {
    for (const auto& [key, value] : new_assumptions) {
        if (key == "default_rate_multiplier") {
            for (auto& [bond_id, bond] : bonds_) {
                bond.probability_of_default *= value;
            }
        } else if (key == "recovery_rate") {
            for (auto& [bond_id, bond] : bonds_) {
                bond.recovery_rate = value;
                bond.loss_given_default = 1.0 - value;
            }
        }
    }
    
    updatePortfolioMetrics();
}

std::vector<Date> BondDeal::getPaymentDates(Date start_date, Date end_date) const {
    std::vector<Date> dates;
    
    // Use quarterly payment schedule as default for bonds
    Date current = start_date;
    while (current <= end_date) {
        dates.push_back(current);
        current = current + 90; // Quarterly
    }
    
    return dates;
}

void BondDeal::generateReports(Date report_date) const {
    // In practice, would generate comprehensive reports
    // For now, just validate the call
}

// Portfolio analytics
double BondDeal::calculatePortfolioDuration() const {
    if (bonds_.empty()) return 0.0;
    
    double weighted_duration = 0.0;
    Amount total_notional = 0.0;
    
    for (const auto& [bond_id, bond] : bonds_) {
        double yield = 0.05; // Simplified
        double duration = bond.calculateModifiedDuration(yield);
        weighted_duration += duration * bond.outstanding_balance;
        total_notional += bond.outstanding_balance;
    }
    
    return total_notional > 0 ? weighted_duration / total_notional : 0.0;
}

double BondDeal::calculateWeightedAverageRating() const {
    if (bonds_.empty()) return 0.0;
    
    double weighted_rating = 0.0;
    Amount total_notional = 0.0;
    
    for (const auto& [bond_id, bond] : bonds_) {
        double rating_numeric = static_cast<double>(bond.rating);
        weighted_rating += rating_numeric * bond.outstanding_balance;
        total_notional += bond.outstanding_balance;
    }
    
    return total_notional > 0 ? weighted_rating / total_notional : 0.0;
}

Amount BondDeal::calculateTotalNotional() const {
    Amount total = 0.0;
    for (const auto& [bond_id, bond] : bonds_) {
        total += bond.outstanding_balance;
    }
    return total;
}

std::map<CreditRating, Amount> BondDeal::getRatingDistribution() const {
    std::map<CreditRating, Amount> distribution;
    
    for (const auto& [bond_id, bond] : bonds_) {
        distribution[bond.rating] += bond.outstanding_balance;
    }
    
    return distribution;
}

// Credit risk analysis
Amount BondDeal::calculateExpectedLoss() const {
    Amount total_el = 0.0;
    for (const auto& [bond_id, bond] : bonds_) {
        total_el += bond.calculateExpectedLoss();
    }
    return total_el;
}

Amount BondDeal::calculateUnexpectedLoss(double confidence_level) const {
    // Simplified UL calculation
    Amount expected_loss = calculateExpectedLoss();
    double portfolio_volatility = 0.15; // Simplified
    
    // Use normal approximation for confidence interval
    double z_score = (confidence_level == 0.99) ? 2.33 : 1.65;
    
    return expected_loss * portfolio_volatility * z_score;
}

std::map<std::string, double> BondDeal::calculateBondProbabilities() const {
    std::map<std::string, double> probabilities;
    
    for (const auto& [bond_id, bond] : bonds_) {
        probabilities[bond_id] = bond.probability_of_default;
    }
    
    return probabilities;
}

// Performance metrics
double BondDeal::calculatePortfolioYield() const {
    if (bonds_.empty()) return 0.0;
    
    double weighted_yield = 0.0;
    Amount total_notional = 0.0;
    
    for (const auto& [bond_id, bond] : bonds_) {
        weighted_yield += bond.coupon_rate * bond.outstanding_balance;
        total_notional += bond.outstanding_balance;
    }
    
    return total_notional > 0 ? weighted_yield / total_notional : 0.0;
}

double BondDeal::calculateExcessSpread() const {
    double portfolio_yield = calculatePortfolioYield();
    double funding_cost = 0.03; // Simplified
    
    return portfolio_yield - funding_cost;
}

std::map<std::string, Amount> BondDeal::getCreditLosses(Date start_date, Date end_date) const {
    std::map<std::string, Amount> losses;
    
    for (const auto& [date, loss] : historical_losses_) {
        if (date >= start_date && date <= end_date) {
            std::string period = std::to_string(date);
            losses[period] = loss;
        }
    }
    
    return losses;
}

// Stress testing
void BondDeal::applyRatingDowngrade(int notches) {
    for (auto& [bond_id, bond] : bonds_) {
        int current_rating = static_cast<int>(bond.rating);
        int new_rating = std::min(current_rating + notches, static_cast<int>(CreditRating::D));
        bond.updateCreditRating(static_cast<CreditRating>(new_rating));
    }
    
    updatePortfolioMetrics();
}

void BondDeal::applyDefaultScenario(double default_rate_multiplier) {
    for (auto& [bond_id, bond] : bonds_) {
        bond.probability_of_default *= default_rate_multiplier;
        bond.probability_of_default = std::min(bond.probability_of_default, 1.0);
    }
}

std::map<std::string, Amount> BondDeal::stressTestWaterfall(double loss_scenario) const {
    // Apply stress scenario to available funds
    Amount stress_funds = 1000000.0 * (1.0 - loss_scenario); // Simplified
    
    return waterfall_.distributeFunds(stress_funds, bonds_);
}

// Private helper methods
void BondDeal::processBondPayments(Date payment_date) {
    for (auto& [bond_id, bond] : bonds_) {
        if (!bond.is_defaulted) {
            bond.last_payment_date = payment_date;
        }
    }
}

void BondDeal::updateCreditMetrics(Date as_of_date) {
    weighted_average_rating_ = calculateWeightedAverageRating();
    portfolio_duration_ = calculatePortfolioDuration();
    
    // Store historical data
    Amount current_loss = calculateExpectedLoss();
    historical_losses_[as_of_date] = current_loss;
    portfolio_ratings_[as_of_date] = weighted_average_rating_;
}

void BondDeal::checkCoverageTests(Date test_date) {
    Amount pool_cashflow = 1000000.0; // Simplified
    Amount bond_payments = 0.0;
    
    for (const auto& [bond_id, bond] : bonds_) {
        bond_payments += bond.calculateCouponPayment();
    }
    
    bool passes_coverage = waterfall_.checkCoverageRatios(pool_cashflow, bond_payments);
    if (!passes_coverage) {
        // Would trigger remedial actions in practice
    }
}

Amount BondDeal::collectPoolCashflow(Date collection_date) {
    if (!underlying_pool_) return 0.0;
    
    // Simplified pool cashflow collection
    return 1000000.0; // Would calculate from actual pool
}

void BondDeal::distributeCashflow(Amount available_funds, Date distribution_date) {
    auto distributions = waterfall_.distributeFunds(available_funds, bonds_);
    
    // Apply distributions to accounts
    for (const auto& [bond_id, amount] : distributions) {
        // In practice, would update bond balances and investor accounts
    }
}

void BondDeal::updateReserves(Date update_date) {
    Amount required_reserves = waterfall_.calculateRequiredReserves(bonds_);
    
    auto reserve_it = accounts_.find(reserve_account_id_);
    if (reserve_it != accounts_.end()) {
        Amount current_reserves = reserve_it->second.getBalance();
        if (current_reserves < required_reserves) {
            // Would trigger reserve funding in practice
        }
    }
}

double BondDeal::calculateCorrelationAdjustment() const {
    // Simplified correlation model
    return 0.85; // 85% correlation adjustment
}

Amount BondDeal::calculateRiskAdjustedReserves() const {
    Amount base_reserves = waterfall_.calculateRequiredReserves(bonds_);
    double correlation_adj = calculateCorrelationAdjustment();
    
    return base_reserves * correlation_adj;
}

void BondDeal::updatePortfolioMetrics() {
    weighted_average_rating_ = calculateWeightedAverageRating();
    portfolio_duration_ = calculatePortfolioDuration();
}

} // namespace Structura