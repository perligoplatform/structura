#include "clo_deal.h"
#include "../core/types.h"
#include <algorithm>
#include <numeric>
#include <sstream>
#include <stdexcept>

namespace Structura {

// CLOTrancheType utility functions
std::string cloTrancheTypeToString(CLOTrancheType type) {
    switch (type) {
        case CLOTrancheType::AAA: return "AAA";
        case CLOTrancheType::AA: return "AA";
        case CLOTrancheType::A: return "A";
        case CLOTrancheType::BBB: return "BBB";
        case CLOTrancheType::BB: return "BB";
        case CLOTrancheType::B: return "B";
        case CLOTrancheType::EQUITY: return "EQUITY";
        default: return "UNKNOWN";
    }
}

CLOTrancheType stringToCLOTrancheType(const std::string& str) {
    if (str == "AAA") return CLOTrancheType::AAA;
    if (str == "AA") return CLOTrancheType::AA;
    if (str == "A") return CLOTrancheType::A;
    if (str == "BBB") return CLOTrancheType::BBB;
    if (str == "BB") return CLOTrancheType::BB;
    if (str == "B") return CLOTrancheType::B;
    if (str == "EQUITY") return CLOTrancheType::EQUITY;
    throw std::invalid_argument("Unknown CLO tranche type: " + str);
}

// CLOTranche implementation
CLOTranche::CLOTranche(const std::string& id, CLOTrancheType type, const std::string& rating,
                       Amount notional, Rate coupon, bool floating)
    : tranche_id(id), tranche_type(type), credit_rating(rating), notional_amount(notional),
      outstanding_balance(notional), coupon_rate(coupon), payment_frequency(PaymentFrequency::Quarterly),
      subordination_percentage(0.0), credit_enhancement_level(0.0), overcollateralization_amount(0.0),
      interest_coverage_reserve(0.0), is_floating_rate(floating), reference_rate("SOFR"),
      spread_over_reference(0.0), floor_rate(0.0), cap_rate(0.0), total_principal_paid(0.0),
      total_interest_paid(0.0), cumulative_losses(0.0), current_rating_factor(1.0) {
    
    // Set default subordination levels based on tranche type
    switch (type) {
        case CLOTrancheType::AAA:
            subordination_percentage = 0.35;
            credit_enhancement_level = 0.40;
            current_rating_factor = 1.0;
            break;
        case CLOTrancheType::AA:
            subordination_percentage = 0.28;
            credit_enhancement_level = 0.33;
            current_rating_factor = 0.95;
            break;
        case CLOTrancheType::A:
            subordination_percentage = 0.20;
            credit_enhancement_level = 0.25;
            current_rating_factor = 0.85;
            break;
        case CLOTrancheType::BBB:
            subordination_percentage = 0.12;
            credit_enhancement_level = 0.17;
            current_rating_factor = 0.75;
            break;
        case CLOTrancheType::BB:
            subordination_percentage = 0.06;
            credit_enhancement_level = 0.09;
            current_rating_factor = 0.60;
            break;
        case CLOTrancheType::B:
            subordination_percentage = 0.02;
            credit_enhancement_level = 0.04;
            current_rating_factor = 0.40;
            break;
        case CLOTrancheType::EQUITY:
            subordination_percentage = 0.0;
            credit_enhancement_level = 0.0;
            current_rating_factor = 0.20;
            break;
    }
    
    // Set typical floating rate parameters for CLO tranches
    if (is_floating_rate) {
        switch (type) {
            case CLOTrancheType::AAA:
                spread_over_reference = 0.0120; // 120 bps
                floor_rate = 0.0050; // 50 bps floor
                break;
            case CLOTrancheType::AA:
                spread_over_reference = 0.0150; // 150 bps
                floor_rate = 0.0075; // 75 bps floor
                break;
            case CLOTrancheType::A:
                spread_over_reference = 0.0200; // 200 bps
                floor_rate = 0.0100; // 100 bps floor
                break;
            case CLOTrancheType::BBB:
                spread_over_reference = 0.0300; // 300 bps
                floor_rate = 0.0150; // 150 bps floor
                break;
            case CLOTrancheType::BB:
                spread_over_reference = 0.0500; // 500 bps
                floor_rate = 0.0200; // 200 bps floor
                break;
            case CLOTrancheType::B:
                spread_over_reference = 0.0800; // 800 bps
                floor_rate = 0.0300; // 300 bps floor
                break;
            case CLOTrancheType::EQUITY:
                // Equity doesn't have fixed coupon
                break;
        }
        cap_rate = 0.1500; // 15% cap for all floating rate tranches
    }
}

Amount CLOTranche::calculateInterestPayment(Rate current_reference_rate) const {
    if (outstanding_balance <= 0.0) return 0.0;
    
    Rate effective_rate = coupon_rate;
    
    if (is_floating_rate) {
        effective_rate = current_reference_rate + spread_over_reference;
        effective_rate = std::max(effective_rate, floor_rate);
        effective_rate = std::min(effective_rate, cap_rate);
    }
    
    // Quarterly payment frequency typical for CLOs
    double quarterly_rate = effective_rate / 4.0;
    return outstanding_balance * quarterly_rate;
}

Amount CLOTranche::calculatePrincipalPayment(Amount available_principal) const {
    if (outstanding_balance <= 0.0) return 0.0;
    return std::min(available_principal, outstanding_balance);
}

void CLOTranche::makePayment(Amount principal, Amount interest, Date payment_date) {
    outstanding_balance -= principal;
    total_principal_paid += principal;
    total_interest_paid += interest;
    last_payment_date = payment_date;
}

double CLOTranche::getCurrentYield(Rate current_reference_rate) const {
    if (outstanding_balance <= 0.0) return 0.0;
    
    Rate effective_rate = coupon_rate;
    if (is_floating_rate) {
        effective_rate = current_reference_rate + spread_over_reference;
        effective_rate = std::max(effective_rate, floor_rate);
        effective_rate = std::min(effective_rate, cap_rate);
    }
    
    return effective_rate;
}

double CLOTranche::getCumulativeLossRate() const {
    if (notional_amount <= 0.0) return 0.0;
    return cumulative_losses / notional_amount;
}

bool CLOTranche::isFullyPaid() const {
    return outstanding_balance <= 0.01; // Small tolerance for floating point
}

// CLOWaterfall implementation
CLOWaterfall::CLOWaterfall() : reinvestment_allowed(true), maximum_ccc_concentration(0.075),
                               maximum_single_obligor_concentration(0.02), minimum_weighted_average_spread(0.04) {
    
    // Standard CLO interest payment sequence
    interest_payment_sequence = {
        "senior_management_fee",
        "trustee_fee", 
        "administrative_fee",
        "AAA_interest",
        "AA_interest", 
        "A_interest",
        "BBB_interest",
        "subordinate_management_fee",
        "BB_interest",
        "B_interest"
        // Equity gets residual after all senior payments
    };
    
    // Standard CLO principal payment sequence (sequential after reinvestment period)
    principal_payment_sequence = {
        "AAA_principal",
        "AA_principal",
        "A_principal", 
        "BBB_principal",
        "BB_principal",
        "B_principal"
        // Equity gets residual
    };
    
    // Loss allocation (reverse of payment waterfall)
    loss_allocation = {
        {"EQUITY", 1.0},    // First loss
        {"B", 0.9},
        {"BB", 0.8},
        {"BBB", 0.7},
        {"A", 0.5},
        {"AA", 0.3},
        {"AAA", 0.1}        // Last loss (most protected)
    };
    
    // Standard CLO overcollateralization ratios
    overcollateralization_ratios = {
        {"AAA", 1.40},      // 140% OC test
        {"AA", 1.35},       // 135% OC test  
        {"A", 1.30},        // 130% OC test
        {"BBB", 1.20},      // 120% OC test
        {"BB", 1.10},       // 110% OC test
        {"B", 1.05}         // 105% OC test
    };
    
    // Standard CLO interest coverage ratios
    interest_coverage_ratios = {
        {"AAA", 1.30},      // 130% IC test
        {"AA", 1.25},       // 125% IC test
        {"A", 1.20},        // 120% IC test
        {"BBB", 1.15},      // 115% IC test
        {"BB", 1.10},       // 110% IC test
        {"B", 1.05}         // 105% IC test
    };
}

std::map<std::string, Amount> CLOWaterfall::processInterestWaterfall(
    Amount available_interest,
    const std::map<std::string, CLOTranche>& tranches,
    const std::map<std::string, Amount>& required_fees) const {
    
    std::map<std::string, Amount> distributions;
    Amount remaining = available_interest;
    
    // Pay fees and interest according to waterfall sequence
    for (const auto& payment_item : interest_payment_sequence) {
        if (remaining <= 0.0) break;
        
        Amount required_amount = 0.0;
        
        // Handle fee payments
        if (required_fees.find(payment_item) != required_fees.end()) {
            required_amount = required_fees.at(payment_item);
        }
        // Handle tranche interest payments
        else {
            for (const auto& [tranche_id, tranche] : tranches) {
                std::string interest_key = cloTrancheTypeToString(tranche.tranche_type) + "_interest";
                if (payment_item == interest_key) {
                    required_amount = tranche.calculateInterestPayment();
                    break;
                }
            }
        }
        
        Amount payment = std::min(required_amount, remaining);
        if (payment > 0.0) {
            distributions[payment_item] = payment;
            remaining -= payment;
        }
    }
    
    // Any remaining goes to equity
    if (remaining > 0.0) {
        distributions["equity_distribution"] = remaining;
    }
    
    return distributions;
}

std::map<std::string, Amount> CLOWaterfall::processPrincipalWaterfall(
    Amount available_principal,
    const std::map<std::string, CLOTranche>& tranches) const {
    
    std::map<std::string, Amount> distributions;
    Amount remaining = available_principal;
    
    // During reinvestment period, principal may be reinvested rather than distributed
    if (reinvestment_allowed) {
        distributions["reinvestment"] = remaining;
        return distributions;
    }
    
    // After reinvestment period, pay down tranches sequentially
    for (const auto& payment_item : principal_payment_sequence) {
        if (remaining <= 0.0) break;
        
        for (const auto& [tranche_id, tranche] : tranches) {
            std::string principal_key = cloTrancheTypeToString(tranche.tranche_type) + "_principal";
            if (payment_item == principal_key) {
                Amount payment = tranche.calculatePrincipalPayment(remaining);
                if (payment > 0.0) {
                    distributions[tranche_id] = payment;
                    remaining -= payment;
                }
                break;
            }
        }
    }
    
    // Any remaining goes to equity
    if (remaining > 0.0) {
        distributions["equity_distribution"] = remaining;
    }
    
    return distributions;
}

std::map<std::string, Amount> CLOWaterfall::processLossWaterfall(
    Amount total_losses,
    const std::map<std::string, CLOTranche>& tranches) const {
    
    std::map<std::string, Amount> loss_allocations;
    Amount remaining_losses = total_losses;
    
    // Allocate losses in reverse order of payment priority
    std::vector<std::string> loss_sequence = {"EQUITY", "B", "BB", "BBB", "A", "AA", "AAA"};
    
    for (const auto& tranche_type_str : loss_sequence) {
        if (remaining_losses <= 0.0) break;
        
        // Find the corresponding tranche
        for (const auto& [tranche_id, tranche] : tranches) {
            if (cloTrancheTypeToString(tranche.tranche_type) == tranche_type_str) {
                Amount tranche_loss = std::min(remaining_losses, tranche.outstanding_balance);
                if (tranche_loss > 0.0) {
                    loss_allocations[tranche_id] = tranche_loss;
                    remaining_losses -= tranche_loss;
                }
                break;
            }
        }
    }
    
    return loss_allocations;
}

bool CLOWaterfall::passesOvercollateralizationTests(const Pool<CorporateLoan>& loan_pool,
                                                   const std::map<std::string, CLOTranche>& tranches) const {
    Amount pool_value = loan_pool.getTotalBalance();
    
    for (const auto& [test_name, required_ratio] : overcollateralization_ratios) {
        Amount cumulative_debt = 0.0;
        
        // Calculate cumulative debt for this test level
        for (const auto& [tranche_id, tranche] : tranches) {
            std::string tranche_rating = cloTrancheTypeToString(tranche.tranche_type);
            if (tranche_rating == test_name || 
                (test_name == "AAA" && (tranche_rating == "AAA" || tranche_rating == "AA" || tranche_rating == "A")) ||
                (test_name == "AA" && (tranche_rating == "AA" || tranche_rating == "A")) ||
                (test_name == "A" && tranche_rating == "A")) {
                cumulative_debt += tranche.outstanding_balance;
            }
        }
        
        double actual_ratio = (cumulative_debt > 0.0) ? pool_value / cumulative_debt : 999.0;
        if (actual_ratio < required_ratio) {
            return false;
        }
    }
    
    return true;
}

bool CLOWaterfall::passesInterestCoverageTests(Amount available_interest,
                                              const std::map<std::string, CLOTranche>& tranches) const {
    for (const auto& [test_name, required_ratio] : interest_coverage_ratios) {
        Amount cumulative_interest = 0.0;
        
        // Calculate cumulative interest requirements for this test level
        for (const auto& [tranche_id, tranche] : tranches) {
            std::string tranche_rating = cloTrancheTypeToString(tranche.tranche_type);
            if (tranche_rating == test_name ||
                (test_name == "AAA" && (tranche_rating == "AAA" || tranche_rating == "AA" || tranche_rating == "A")) ||
                (test_name == "AA" && (tranche_rating == "AA" || tranche_rating == "A")) ||
                (test_name == "A" && tranche_rating == "A")) {
                cumulative_interest += tranche.calculateInterestPayment();
            }
        }
        
        double actual_ratio = (cumulative_interest > 0.0) ? available_interest / cumulative_interest : 999.0;
        if (actual_ratio < required_ratio) {
            return false;
        }
    }
    
    return true;
}

bool CLOWaterfall::passesQualityTests(const Pool<CorporateLoan>& loan_pool) const {
    // Check CCC concentration limit
    Amount total_pool_balance = loan_pool.getTotalBalance();
    Amount ccc_balance = 0.0;
    
    auto loans = loan_pool.getAssets();
    for (const auto& loan : loans) {
        // Assume CCC rating check - would need actual rating field
        // This is a placeholder for the rating check logic
        if (loan->getCurrentBalance() < total_pool_balance * 0.01) { // Placeholder
            ccc_balance += loan->getCurrentBalance();
        }
    }
    
    double ccc_concentration = (total_pool_balance > 0.0) ? ccc_balance / total_pool_balance : 0.0;
    if (ccc_concentration > maximum_ccc_concentration) {
        return false;
    }
    
    return true;
}

bool CLOWaterfall::checkConcentrationLimits(const Pool<CorporateLoan>& loan_pool) const {
    Amount total_balance = loan_pool.getTotalBalance();
    auto loans = loan_pool.getAssets();
    
    // Check single obligor concentration
    std::map<std::string, Amount> obligor_balances;
    for (const auto& loan : loans) {
        std::string obligor_id = loan->getObligor().getId();
        obligor_balances[obligor_id] += loan->getCurrentBalance();
    }
    
    for (const auto& [obligor_id, balance] : obligor_balances) {
        double concentration = balance / total_balance;
        if (concentration > maximum_single_obligor_concentration) {
            return false;
        }
    }
    
    return true;
}

bool CLOWaterfall::checkCreditQualityLimits(const Pool<CorporateLoan>& loan_pool) const {
    // Implementation would check various credit quality limits
    // This is a placeholder for comprehensive credit quality checks
    return true;
}

bool CLOWaterfall::checkMaturityProfile(const Pool<CorporateLoan>& loan_pool) const {
    // Implementation would check weighted average life and maturity constraints
    // This is a placeholder for maturity profile validation
    return true;
}

// CLOPortfolioManager implementation
CLOPortfolioManager::CLOPortfolioManager() : actively_managed(true), reinvestment_capacity(0.0),
                                            maximum_trading_volume_per_period(0.20), total_reinvestments(0.0),
                                            total_dispositions(0.0), portfolio_turnover_rate(0.0),
                                            weighted_average_spread_improvement(0.0) {}

bool CLOPortfolioManager::canReinvestProceeds(Amount proceeds, const Pool<CorporateLoan>& current_pool) const {
    if (!actively_managed || proceeds <= 0.0) return false;
    
    // Check if we're still in reinvestment period
    Date current_date = Date(); // Would need actual current date
    if (current_date > reinvestment_period_end) return false;
    
    // Check if reinvestment capacity allows it
    return proceeds <= reinvestment_capacity;
}

std::vector<std::string> CLOPortfolioManager::identifyTradingOpportunities(const Pool<CorporateLoan>& pool) const {
    std::vector<std::string> opportunities;
    
    auto loans = pool.getAssets();
    for (const auto& loan : loans) {
        // Identify loans that might benefit from trading
        // This is a placeholder for sophisticated trading logic
        if (loan->getCurrentBalance() > 0.0) {
            opportunities.push_back(loan->getAssetId());
        }
    }
    
    return opportunities;
}

bool CLOPortfolioManager::evaluateNewLoanAddition(const CorporateLoan& loan, const Pool<CorporateLoan>& current_pool) const {
    // Evaluate if new loan meets portfolio criteria
    // This would include credit analysis, concentration checks, etc.
    // Placeholder implementation
    return loan.getCurrentBalance() > 0.0;
}

std::map<std::string, double> CLOPortfolioManager::generateTradingReport() const {
    return {
        {"total_reinvestments", total_reinvestments},
        {"total_dispositions", total_dispositions},
        {"portfolio_turnover_rate", portfolio_turnover_rate},
        {"spread_improvement", weighted_average_spread_improvement}
    };
}

std::map<std::string, double> CLOPortfolioManager::calculatePortfolioMetrics(const Pool<CorporateLoan>& pool) const {
    // Calculate comprehensive portfolio metrics
    // This is a placeholder for detailed portfolio analytics
    return {
        {"weighted_average_spread", 0.05},
        {"weighted_average_life", 4.5},
        {"portfolio_yield", 0.065}
    };
}

// CLOAnalytics implementation
CLOAnalytics::CLOAnalytics() : weighted_average_spread(0.0), weighted_average_price(0.0),
                              weighted_average_life(0.0), weighted_average_rating_factor(0.0),
                              portfolio_credit_score(0.0), expected_credit_losses(0.0),
                              unexpected_credit_losses(0.0), default_probability(0.0),
                              recovery_rate_assumption(0.70), current_excess_spread(0.0),
                              reinvestment_yield(0.0), total_return_on_equity(0.0) {}

void CLOAnalytics::calculateMetrics(const Pool<CorporateLoan>& loan_pool,
                                   const std::map<std::string, CLOTranche>& tranches) {
    Amount total_balance = loan_pool.getTotalBalance();
    auto loans = loan_pool.getAssets();
    
    if (total_balance <= 0.0 || loans.empty()) return;
    
    // Calculate weighted average spread
    Amount weighted_spread_sum = 0.0;
    for (const auto& loan : loans) {
        Amount balance = loan->getCurrentBalance();
        Rate spread = loan->getCurrentRate(); // Placeholder - would need actual spread
        weighted_spread_sum += balance * spread;
    }
    weighted_average_spread = weighted_spread_sum / total_balance;
    
    // Calculate weighted average life (placeholder)
    weighted_average_life = 4.5; // Would calculate based on loan maturities
    
    // Calculate weighted average rating factor
    Amount weighted_rating_sum = 0.0;
    for (const auto& loan : loans) {
        Amount balance = loan->getCurrentBalance();
        double rating_factor = 0.85; // Placeholder - would derive from actual rating
        weighted_rating_sum += balance * rating_factor;
    }
    weighted_average_rating_factor = weighted_rating_sum / total_balance;
    
    // Calculate expected credit losses (simplified)
    expected_credit_losses = total_balance * default_probability * (1.0 - recovery_rate_assumption);
    
    // Calculate tranche metrics
    tranche_internal_rates_of_return.clear();
    for (const auto& [tranche_id, tranche] : tranches) {
        // Placeholder IRR calculation
        tranche_internal_rates_of_return[tranche_id] = tranche.getCurrentYield();
    }
}

std::map<std::string, double> CLOAnalytics::getConcentrationAnalysis(const Pool<CorporateLoan>& pool) const {
    // Detailed concentration analysis by various dimensions
    return {
        {"max_single_obligor", 0.015},
        {"top_10_obligors", 0.12},
        {"max_industry", 0.08},
        {"geographic_concentration", 0.25}
    };
}

std::map<std::string, Amount> CLOAnalytics::getCreditLossProjections() const {
    return {
        {"year_1_expected_losses", expected_credit_losses * 0.3},
        {"year_2_expected_losses", expected_credit_losses * 0.25},
        {"year_3_expected_losses", expected_credit_losses * 0.2},
        {"total_expected_losses", expected_credit_losses}
    };
}

std::map<std::string, double> CLOAnalytics::calculateTrancheMetrics(const std::map<std::string, CLOTranche>& tranches) const {
    std::map<std::string, double> metrics;
    
    for (const auto& [tranche_id, tranche] : tranches) {
        std::string prefix = tranche_id + "_";
        metrics[prefix + "yield"] = tranche.getCurrentYield();
        metrics[prefix + "loss_rate"] = tranche.getCumulativeLossRate();
        metrics[prefix + "rating_factor"] = tranche.getRatingFactor();
    }
    
    return metrics;
}

// CLODeal implementation
CLODeal::CLODeal(const DealInfo& deal_info) : DealBase(deal_info), loan_pool_(nullptr),
                                             management_fee_rate_(0.004), trustee_fee_rate_(0.0002),
                                             administrative_fee_rate_(0.0001), senior_management_fee_(0.0),
                                             subordinate_management_fee_(0.0) {
    
    // Set up standard CLO accounts
    principal_collection_account_id_ = "principal_collection";
    interest_collection_account_id_ = "interest_collection";
    expense_account_id_ = "expense";
    reserve_account_id_ = "reserve";
    reinvestment_account_id_ = "reinvestment";
    equity_distribution_account_id_ = "equity_distribution";
    
    // Create accounts with zero initial balance
    addAccount(principal_collection_account_id_, std::make_unique<Account>(principal_collection_account_id_, 0.0));
    addAccount(interest_collection_account_id_, std::make_unique<Account>(interest_collection_account_id_, 0.0));
    addAccount(expense_account_id_, std::make_unique<Account>(expense_account_id_, 0.0));
    addAccount(reserve_account_id_, std::make_unique<Account>(reserve_account_id_, 0.0));
    addAccount(reinvestment_account_id_, std::make_unique<Account>(reinvestment_account_id_, 0.0));
    addAccount(equity_distribution_account_id_, std::make_unique<Account>(equity_distribution_account_id_, 0.0));
}

void CLODeal::setLoanPool(Pool<CorporateLoan>* pool) {
    loan_pool_ = pool;
}

void CLODeal::addTranche(const CLOTranche& tranche) {
    tranches_[tranche.tranche_id] = tranche;
}

void CLODeal::setupWaterfall(const CLOWaterfall& waterfall) {
    waterfall_ = waterfall;
}

void CLODeal::setupPortfolioManager(const CLOPortfolioManager& manager) {
    portfolio_manager_ = manager;
}

void CLODeal::setManagementFees(Rate mgmt_fee, Rate trustee_fee, Rate admin_fee) {
    management_fee_rate_ = mgmt_fee;
    trustee_fee_rate_ = trustee_fee;
    administrative_fee_rate_ = admin_fee;
}

void CLODeal::runWaterfall(const Date& paymentDate) {
    if (!loan_pool_) return;
    
    // Process loan collections
    processLoanCollections(paymentDate);
    
    // Calculate available funds
    Amount available_interest = getAccount(interest_collection_account_id_)->getBalance();
    Amount available_principal = getAccount(principal_collection_account_id_)->getBalance();
    
    // Calculate required fees
    std::map<std::string, Amount> required_fees = {
        {"senior_management_fee", calculateManagementFees(paymentDate) * 0.6},
        {"trustee_fee", calculateTrusteeFees(paymentDate)},
        {"administrative_fee", calculateAdministrativeFees(paymentDate)},
        {"subordinate_management_fee", calculateManagementFees(paymentDate) * 0.4}
    };
    
    // Run coverage tests
    bool coverage_tests_pass = runCoverageTests(paymentDate);
    
    // Process interest waterfall
    auto interest_distributions = waterfall_.processInterestWaterfall(available_interest, tranches_, required_fees);
    
    // Process principal waterfall (may redirect to reinvestment)
    auto principal_distributions = waterfall_.processPrincipalWaterfall(available_principal, tranches_);
    
    // Distribute funds according to waterfall results
    distributeCollections(available_interest + available_principal, paymentDate);
    
    // Update analytics
    updatePortfolioAnalytics(paymentDate);
    
    // Record historical data
    historical_collections_[paymentDate] = available_interest + available_principal;
    
    // Store coverage test results
    coverage_test_history_[paymentDate] = getCoverageRatios();
}

Balance CLODeal::calculateDealValue(const Date& valuationDate) const {
    if (!loan_pool_) return 0.0;
    
    // Deal value is sum of:
    // 1. Loan pool market value
    // 2. Cash in accounts
    // 3. Accrued interest
    
    Balance pool_value = loan_pool_->getTotalBalance();
    Balance cash_value = getTotalAccountBalance();
    
    // Add some market value adjustments (simplified)
    Balance market_adjustment = pool_value * 0.02; // 2% market premium
    
    return pool_value + cash_value + market_adjustment;
}

std::vector<std::string> CLODeal::validate() const {
    std::vector<std::string> errors = validateBasicDealStructure();
    
    if (!loan_pool_) {
        errors.push_back("CLO Deal must have a loan pool assigned");
    }
    
    if (tranches_.empty()) {
        errors.push_back("CLO Deal must have at least one tranche");
    }
    
    // Validate tranche structure
    Amount total_tranche_notional = 0.0;
    bool has_equity = false;
    
    for (const auto& [tranche_id, tranche] : tranches_) {
        total_tranche_notional += tranche.notional_amount;
        if (tranche.tranche_type == CLOTrancheType::EQUITY) {
            has_equity = true;
        }
    }
    
    if (!has_equity) {
        errors.push_back("CLO Deal must have an equity tranche");
    }
    
    if (loan_pool_ && total_tranche_notional > loan_pool_->getTotalBalance() * 1.1) {
        errors.push_back("Total tranche notional exceeds reasonable leverage limits");
    }
    
    return errors;
}

std::string CLODeal::getDealSummary() const {
    std::ostringstream summary;
    summary << "CLO Deal: " << getDealName() << "\n";
    summary << "Total Tranches: " << tranches_.size() << "\n";
    
    if (loan_pool_) {
        summary << "Loan Pool Balance: $" << loan_pool_->getTotalBalance() << "\n";
        summary << "Number of Loans: " << loan_pool_->size() << "\n";
    }
    
    Amount total_tranche_balance = 0.0;
    for (const auto& [tranche_id, tranche] : tranches_) {
        total_tranche_balance += tranche.outstanding_balance;
    }
    summary << "Total Tranche Balance: $" << total_tranche_balance << "\n";
    
    summary << "Deal Status: " << statusToString(getStatus()) << "\n";
    
    return summary.str();
}

void CLODeal::processPaymentDate(const Date& paymentDate) {
    setCurrentDate(paymentDate);
    runWaterfall(paymentDate);
    
    // Accrue interest on accounts
    accrueInterestOnAllAccounts(paymentDate);
    
    // Process any scheduled activities
    manageReinvestmentAccount(paymentDate);
    processEquityDistributions(paymentDate);
}

void CLODeal::processLoanDefault(const std::string& loan_id, Date default_date, Amount recovery_amount) {
    if (!loan_pool_) return;
    
    // Find the loan and process default
    auto loans = loan_pool_->getAssets();
    for (auto& loan : loans) {
        if (loan->getAssetId() == loan_id) {
            Amount loss_amount = loan->getCurrentBalance() - recovery_amount;
            
            // Record the loss
            historical_defaults_[default_date] += loss_amount;
            historical_recoveries_[default_date] += recovery_amount;
            
            // Allocate losses through waterfall
            allocateLosses(loss_amount, default_date);
            
            break;
        }
    }
}

void CLODeal::processLoanPrepayment(const std::string& loan_id, Date prepayment_date, Amount prepayment_amount) {
    // Process prepayment and add to principal collection account
    Account* principal_account = getAccount(principal_collection_account_id_);
    if (principal_account) {
        principal_account->deposit(prepayment_amount, prepayment_date);
    }
}

void CLODeal::reinvestProceeds(Amount proceeds, Date reinvestment_date) {
    if (portfolio_manager_.canReinvestProceeds(proceeds, *loan_pool_)) {
        // Move proceeds to reinvestment account
        transferBetweenAccounts(principal_collection_account_id_, reinvestment_account_id_, proceeds);
        portfolio_manager_.total_reinvestments += proceeds;
    }
}

void CLODeal::disposeLoan(const std::string& loan_id, Date disposition_date, Amount sale_price) {
    // Process loan sale and record proceeds
    Account* principal_account = getAccount(principal_collection_account_id_);
    if (principal_account) {
        principal_account->deposit(sale_price, disposition_date);
    }
    
    portfolio_manager_.total_dispositions += sale_price;
}

bool CLODeal::runCoverageTests(Date test_date) {
    if (!loan_pool_) return false;
    
    bool oc_tests_pass = waterfall_.passesOvercollateralizationTests(*loan_pool_, tranches_);
    
    Amount available_interest = getAccount(interest_collection_account_id_)->getBalance();
    bool ic_tests_pass = waterfall_.passesInterestCoverageTests(available_interest, tranches_);
    
    bool quality_tests_pass = waterfall_.passesQualityTests(*loan_pool_);
    
    return oc_tests_pass && ic_tests_pass && quality_tests_pass;
}

std::map<std::string, bool> CLODeal::checkPortfolioCompliance() const {
    if (!loan_pool_) return {};
    
    return {
        {"concentration_limits", waterfall_.checkConcentrationLimits(*loan_pool_)},
        {"credit_quality_limits", waterfall_.checkCreditQualityLimits(*loan_pool_)},
        {"maturity_profile", waterfall_.checkMaturityProfile(*loan_pool_)}
    };
}

void CLODeal::handleCoverageTestFailure(const std::string& test_name, Date failure_date) {
    // Implement coverage test failure handling
    // This might involve diverting principal payments, increasing reserves, etc.
    
    if (test_name.find("overcollateralization") != std::string::npos) {
        // Redirect principal payments to improve OC ratios
        waterfall_.reinvestment_allowed = false;
    }
    
    if (test_name.find("interest_coverage") != std::string::npos) {
        // May need to reduce subordinate payments
        // Implementation would modify waterfall behavior
    }
}

void CLODeal::allocateLosses(Amount total_losses, Date loss_date) {
    auto loss_allocations = waterfall_.processLossWaterfall(total_losses, tranches_);
    
    for (const auto& [tranche_id, loss_amount] : loss_allocations) {
        if (tranches_.find(tranche_id) != tranches_.end()) {
            tranches_[tranche_id].cumulative_losses += loss_amount;
            tranches_[tranche_id].outstanding_balance = 
                std::max(0.0, tranches_[tranche_id].outstanding_balance - loss_amount);
        }
    }
}

Amount CLODeal::calculateExpectedLosses() const {
    return analytics_.expected_credit_losses;
}

Amount CLODeal::calculateUnexpectedLosses(double confidence_level) const {
    // Simplified unexpected loss calculation
    Amount expected_losses = calculateExpectedLosses();
    double multiplier = (confidence_level >= 0.99) ? 3.0 : 2.0;
    return expected_losses * multiplier;
}

void CLODeal::updateCreditRatings(const std::map<std::string, std::string>& rating_changes) {
    // Update tranche credit ratings and adjust pricing/metrics accordingly
    for (const auto& [tranche_id, new_rating] : rating_changes) {
        if (tranches_.find(tranche_id) != tranches_.end()) {
            tranches_[tranche_id].credit_rating = new_rating;
            // Would also update rating factors, subordination levels, etc.
        }
    }
}

Amount CLODeal::calculateManagementFees(Date calculation_date) const {
    if (!loan_pool_) return 0.0;
    
    Amount portfolio_balance = loan_pool_->getTotalBalance();
    return portfolio_balance * management_fee_rate_ / 4.0; // Quarterly
}

Amount CLODeal::calculateTrusteeFees(Date calculation_date) const {
    Amount total_tranche_balance = 0.0;
    for (const auto& [tranche_id, tranche] : tranches_) {
        total_tranche_balance += tranche.outstanding_balance;
    }
    return total_tranche_balance * trustee_fee_rate_ / 4.0; // Quarterly
}

Amount CLODeal::calculateAdministrativeFees(Date calculation_date) const {
    Amount total_tranche_balance = 0.0;
    for (const auto& [tranche_id, tranche] : tranches_) {
        total_tranche_balance += tranche.outstanding_balance;
    }
    return total_tranche_balance * administrative_fee_rate_ / 4.0; // Quarterly
}

void CLODeal::payFees(Date payment_date) {
    Amount mgmt_fees = calculateManagementFees(payment_date);
    Amount trustee_fees = calculateTrusteeFees(payment_date);
    Amount admin_fees = calculateAdministrativeFees(payment_date);
    
    // Pay fees from expense account
    Account* expense_account = getAccount(expense_account_id_);
    if (expense_account) {
        expense_account->withdraw(mgmt_fees + trustee_fees + admin_fees, payment_date);
    }
}

double CLODeal::calculatePortfolioYield() const {
    return analytics_.weighted_average_spread;
}

double CLODeal::calculateExcessSpread() const {
    return analytics_.current_excess_spread;
}

double CLODeal::calculateWeightedAverageLife() const {
    return analytics_.weighted_average_life;
}

std::map<std::string, double> CLODeal::getTrancheMetrics() const {
    return analytics_.calculateTrancheMetrics(tranches_);
}

std::map<std::string, double> CLODeal::getCoverageRatios() const {
    std::map<std::string, double> ratios;
    
    if (!loan_pool_) return ratios;
    
    Amount pool_value = loan_pool_->getTotalBalance();
    
    // Calculate actual OC ratios
    for (const auto& [test_name, required_ratio] : waterfall_.overcollateralization_ratios) {
        Amount cumulative_debt = 0.0;
        for (const auto& [tranche_id, tranche] : tranches_) {
            std::string tranche_rating = cloTrancheTypeToString(tranche.tranche_type);
            if (tranche_rating == test_name) {
                cumulative_debt += tranche.outstanding_balance;
            }
        }
        
        double actual_ratio = (cumulative_debt > 0.0) ? pool_value / cumulative_debt : 999.0;
        ratios["OC_" + test_name] = actual_ratio;
    }
    
    // Calculate actual IC ratios
    Amount available_interest = getAccount(interest_collection_account_id_)->getBalance();
    for (const auto& [test_name, required_ratio] : waterfall_.interest_coverage_ratios) {
        Amount cumulative_interest = 0.0;
        for (const auto& [tranche_id, tranche] : tranches_) {
            std::string tranche_rating = cloTrancheTypeToString(tranche.tranche_type);
            if (tranche_rating == test_name) {
                cumulative_interest += tranche.calculateInterestPayment();
            }
        }
        
        double actual_ratio = (cumulative_interest > 0.0) ? available_interest / cumulative_interest : 999.0;
        ratios["IC_" + test_name] = actual_ratio;
    }
    
    return ratios;
}

std::map<std::string, Amount> CLODeal::getSectorConcentration() const {
    return analytics_.getConcentrationAnalysis(*loan_pool_);
}

std::map<std::string, Amount> CLODeal::getObligorConcentration() const {
    // Implementation would analyze obligor concentration
    return {};
}

std::map<std::string, Amount> CLODeal::getRatingConcentration() const {
    // Implementation would analyze rating concentration
    return {};
}

std::map<std::string, Amount> CLODeal::getGeographicConcentration() const {
    // Implementation would analyze geographic concentration
    return {};
}

void CLODeal::applyDefaultScenario(double default_rate_multiplier) {
    // Apply stress scenario with increased default rates
    Amount stressed_losses = calculateExpectedLosses() * default_rate_multiplier;
    allocateLosses(stressed_losses, getCurrentDate());
}

void CLODeal::applyRecoveryRateStress(double recovery_rate_shock) {
    // Apply stress to recovery rate assumptions
    analytics_.recovery_rate_assumption *= (1.0 + recovery_rate_shock);
    analytics_.recovery_rate_assumption = std::max(0.0, std::min(1.0, analytics_.recovery_rate_assumption));
}

void CLODeal::applySpreadStress(double spread_shock) {
    // Apply spread stress to portfolio yield
    analytics_.weighted_average_spread += spread_shock;
}

std::map<std::string, Amount> CLODeal::stressTestWaterfall(double loss_scenario, double spread_shock) const {
    // Run waterfall under stress scenario
    // This is a simplified implementation
    std::map<std::string, Amount> stressed_distributions;
    
    Amount stressed_interest = analytics_.weighted_average_spread * (1.0 + spread_shock);
    Amount stressed_losses = calculateExpectedLosses() * loss_scenario;
    
    // Would run full waterfall with stressed parameters
    stressed_distributions["total_interest"] = stressed_interest;
    stressed_distributions["total_losses"] = stressed_losses;
    
    return stressed_distributions;
}

std::map<std::string, double> CLODeal::generateMonthlyReport(Date report_date) const {
    std::map<std::string, double> report;
    
    report["portfolio_balance"] = loan_pool_ ? loan_pool_->getTotalBalance() : 0.0;
    report["weighted_average_spread"] = analytics_.weighted_average_spread;
    report["weighted_average_life"] = analytics_.weighted_average_life;
    report["current_excess_spread"] = analytics_.current_excess_spread;
    
    // Add coverage ratios
    auto coverage_ratios = getCoverageRatios();
    for (const auto& [ratio_name, ratio_value] : coverage_ratios) {
        report[ratio_name] = ratio_value;
    }
    
    return report;
}

std::map<std::string, Amount> CLODeal::generateCashFlowReport(Date start_date, Date end_date) const {
    std::map<std::string, Amount> cash_flows;
    
    Amount total_collections = 0.0;
    Amount total_distributions = 0.0;
    
    for (const auto& [date, amount] : historical_collections_) {
        if (date >= start_date && date <= end_date) {
            total_collections += amount;
        }
    }
    
    cash_flows["total_collections"] = total_collections;
    cash_flows["total_distributions"] = total_distributions;
    
    return cash_flows;
}

std::map<std::string, double> CLODeal::generatePortfolioStatistics() const {
    if (!loan_pool_) return {};
    
    return portfolio_manager_.calculatePortfolioMetrics(*loan_pool_);
}

// Private implementation methods
void CLODeal::processLoanCollections(Date collection_date) {
    if (!loan_pool_) return;
    
    // Collect scheduled payments from loan pool
    Amount principal_collections = 0.0;
    Amount interest_collections = 0.0;
    
    auto loans = loan_pool_->getAssets();
    for (auto& loan : loans) {
        // Process scheduled payment (simplified)
        Amount payment = loan->calculateScheduledPayment(collection_date);
        Amount interest_portion = payment * 0.7; // Simplified allocation
        Amount principal_portion = payment * 0.3;
        
        interest_collections += interest_portion;
        principal_collections += principal_portion;
    }
    
    // Deposit collections into respective accounts
    getAccount(interest_collection_account_id_)->deposit(interest_collections, collection_date);
    getAccount(principal_collection_account_id_)->deposit(principal_collections, collection_date);
}

void CLODeal::updatePortfolioAnalytics(Date update_date) {
    if (!loan_pool_) return;
    
    analytics_.calculateMetrics(*loan_pool_, tranches_);
    
    // Store historical metrics
    portfolio_metrics_history_[update_date] = analytics_.weighted_average_spread;
}

void CLODeal::distributeCollections(Amount collections, Date distribution_date) {
    // Distribute collections according to waterfall results
    // This is a simplified implementation
    
    // Pay fees first
    payFees(distribution_date);
    
    // Then pay tranche interest and principal according to waterfall
    for (auto& [tranche_id, tranche] : tranches_) {
        Amount interest_payment = tranche.calculateInterestPayment();
        Amount principal_payment = std::min(collections * 0.1, tranche.outstanding_balance);
        
        tranche.makePayment(principal_payment, interest_payment, distribution_date);
    }
}

void CLODeal::manageReinvestmentAccount(Date management_date) {
    Account* reinvestment_account = getAccount(reinvestment_account_id_);
    if (reinvestment_account && reinvestment_account->getBalance() > 0.0) {
        // Process any pending reinvestments
        // This would involve actual loan acquisitions in a real implementation
    }
}

void CLODeal::processEquityDistributions(Date distribution_date) {
    Account* equity_account = getAccount(equity_distribution_account_id_);
    if (equity_account && equity_account->getBalance() > 0.0) {
        // Distribute equity proceeds to equity holders
        // This would involve actual cash distributions in a real implementation
    }
}

double CLODeal::calculateConcentrationRisk() const {
    if (!loan_pool_) return 0.0;
    
    // Calculate concentration risk metrics
    auto concentration_analysis = analytics_.getConcentrationAnalysis(*loan_pool_);
    
    double max_concentration = 0.0;
    for (const auto& [category, concentration] : concentration_analysis) {
        max_concentration = std::max(max_concentration, concentration);
    }
    
    return max_concentration;
}

Amount CLODeal::calculateRequiredOvercollateralization() const {
    Amount total_senior_debt = 0.0;
    
    for (const auto& [tranche_id, tranche] : tranches_) {
        if (tranche.tranche_type != CLOTrancheType::EQUITY) {
            total_senior_debt += tranche.outstanding_balance;
        }
    }
    
    // Required OC is typically 120-140% for CLOs
    return total_senior_debt * 0.35; // 35% overcollateralization
}

void CLODeal::updateCreditMetrics() {
    if (!loan_pool_) return;
    
    // Update various credit metrics based on current portfolio
    analytics_.calculateMetrics(*loan_pool_, tranches_);
}

void CLODeal::monitorCoverageTests() {
    // Monitor coverage tests and trigger actions if needed
    bool tests_pass = runCoverageTests(getCurrentDate());
    
    if (!tests_pass) {
        // Handle coverage test failures
        handleCoverageTestFailure("overcollateralization", getCurrentDate());
    }
}

void CLODeal::accrueFees(Date accrual_date) {
    // Accrue management fees, trustee fees, etc.
    Amount mgmt_fees = calculateManagementFees(accrual_date);
    Amount trustee_fees = calculateTrusteeFees(accrual_date);
    Amount admin_fees = calculateAdministrativeFees(accrual_date);
    
    // Add to expense account
    getAccount(expense_account_id_)->deposit(mgmt_fees + trustee_fees + admin_fees, accrual_date);
}

void CLODeal::processManagementFeePayment(Date payment_date) {
    Amount senior_fee = calculateManagementFees(payment_date) * 0.6;
    Amount subordinate_fee = calculateManagementFees(payment_date) * 0.4;
    
    senior_management_fee_ = senior_fee;
    subordinate_management_fee_ = subordinate_fee;
}

void CLODeal::processTrusteeFeePayment(Date payment_date) {
    Amount trustee_fee = calculateTrusteeFees(payment_date);
    
    // Pay from expense account
    Account* expense_account = getAccount(expense_account_id_);
    if (expense_account) {
        expense_account->withdraw(trustee_fee, payment_date);
    }
}

void CLODeal::checkReinvestmentCompliance() {
    // Check compliance with reinvestment period rules
    if (portfolio_manager_.actively_managed) {
        Date current_date = getCurrentDate();
        if (current_date > portfolio_manager_.reinvestment_period_end) {
            waterfall_.reinvestment_allowed = false;
        }
    }
}

void CLODeal::checkTradingCompliance() {
    // Check compliance with trading and portfolio management rules
    auto compliance_results = checkPortfolioCompliance();
    
    for (const auto& [test_name, passes] : compliance_results) {
        if (!passes) {
            // Handle compliance failure
            // This might involve restricting trading, etc.
        }
    }
}

void CLODeal::checkPortfolioQualityTests() {
    if (!loan_pool_) return;
    
    bool quality_tests_pass = waterfall_.passesQualityTests(*loan_pool_);
    if (!quality_tests_pass) {
        // Handle quality test failure
        // This might involve forced asset sales, increased reserves, etc.
    }
}

void CLODeal::generateTrancheReports(Date report_date) const {
    // Generate detailed tranche performance reports
    for (const auto& [tranche_id, tranche] : tranches_) {
        // Would generate comprehensive tranche reporting
        // Including payment history, credit metrics, etc.
    }
}

void CLODeal::generatePortfolioReport(Date report_date) const {
    // Generate detailed portfolio composition and performance report
    if (loan_pool_) {
        // Would generate comprehensive portfolio analytics
        // Including sector breakdown, credit quality, etc.
    }
}

void CLODeal::generateCoverageTestReport(Date report_date) const {
    // Generate coverage test compliance report
    auto coverage_ratios = getCoverageRatios();
    
    // Would compare actual ratios to required ratios
    // And report on compliance status
}

} // namespace Structura