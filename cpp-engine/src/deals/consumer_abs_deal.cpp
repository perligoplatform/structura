#include "consumer_abs_deal.h"
#include "../core/types.h"
#include <algorithm>
#include <numeric>
#include <sstream>
#include <stdexcept>

namespace Structura {

// ConsumerABSTrancheType utility functions
std::string consumerABSTrancheTypeToString(ConsumerABSTrancheType type) {
    switch (type) {
        case ConsumerABSTrancheType::CLASS_A: return "CLASS_A";
        case ConsumerABSTrancheType::CLASS_B: return "CLASS_B";
        case ConsumerABSTrancheType::CLASS_C: return "CLASS_C";
        case ConsumerABSTrancheType::CLASS_D: return "CLASS_D";
        case ConsumerABSTrancheType::RESIDUAL: return "RESIDUAL";
        default: return "UNKNOWN";
    }
}

ConsumerABSTrancheType stringToConsumerABSTrancheType(const std::string& str) {
    if (str == "CLASS_A") return ConsumerABSTrancheType::CLASS_A;
    if (str == "CLASS_B") return ConsumerABSTrancheType::CLASS_B;
    if (str == "CLASS_C") return ConsumerABSTrancheType::CLASS_C;
    if (str == "CLASS_D") return ConsumerABSTrancheType::CLASS_D;
    if (str == "RESIDUAL") return ConsumerABSTrancheType::RESIDUAL;
    throw std::invalid_argument("Unknown Consumer ABS tranche type: " + str);
}

// ConsumerABSTranche implementation
ConsumerABSTranche::ConsumerABSTranche(const std::string& id, ConsumerABSTrancheType type, 
                                       const std::string& rating, Amount notional, Rate coupon)
    : tranche_id(id), tranche_type(type), credit_rating(rating), notional_amount(notional),
      outstanding_balance(notional), coupon_rate(coupon), payment_frequency(PaymentFrequency::Monthly),
      subordination_percentage(0.0), credit_enhancement_level(0.0), reserve_account_target(0.0),
      has_step_down_provisions(false), step_down_threshold(0.02), has_turbo_provisions(false),
      turbo_threshold(0.015), total_principal_paid(0.0), total_interest_paid(0.0),
      cumulative_losses(0.0), current_rating_factor(1.0), is_revolving_tranche(false),
      available_funding_capacity(0.0) {
    
    // Set default subordination levels based on tranche type
    switch (type) {
        case ConsumerABSTrancheType::CLASS_A:
            subordination_percentage = 0.20;
            credit_enhancement_level = 0.25;
            current_rating_factor = 1.0;
            has_step_down_provisions = true;
            break;
        case ConsumerABSTrancheType::CLASS_B:
            subordination_percentage = 0.15;
            credit_enhancement_level = 0.18;
            current_rating_factor = 0.90;
            has_step_down_provisions = true;
            break;
        case ConsumerABSTrancheType::CLASS_C:
            subordination_percentage = 0.08;
            credit_enhancement_level = 0.12;
            current_rating_factor = 0.75;
            has_turbo_provisions = true;
            break;
        case ConsumerABSTrancheType::CLASS_D:
            subordination_percentage = 0.03;
            credit_enhancement_level = 0.05;
            current_rating_factor = 0.50;
            has_turbo_provisions = true;
            break;
        case ConsumerABSTrancheType::RESIDUAL:
            subordination_percentage = 0.0;
            credit_enhancement_level = 0.0;
            current_rating_factor = 0.25;
            break;
    }
}

Amount ConsumerABSTranche::calculateInterestPayment() const {
    if (outstanding_balance <= 0.0) return 0.0;
    
    // Monthly payment frequency typical for Consumer ABS
    double monthly_rate = coupon_rate / 12.0;
    return outstanding_balance * monthly_rate;
}

Amount ConsumerABSTranche::calculatePrincipalPayment(Amount available_principal) const {
    if (outstanding_balance <= 0.0) return 0.0;
    return std::min(available_principal, outstanding_balance);
}

void ConsumerABSTranche::makePayment(Amount principal, Amount interest, Date payment_date) {
    outstanding_balance -= principal;
    total_principal_paid += principal;
    total_interest_paid += interest;
    last_payment_date = payment_date;
}

double ConsumerABSTranche::getCurrentYield() const {
    return coupon_rate;
}

double ConsumerABSTranche::getCumulativeLossRate() const {
    if (notional_amount <= 0.0) return 0.0;
    return cumulative_losses / notional_amount;
}

bool ConsumerABSTranche::isFullyPaid() const {
    return outstanding_balance <= 0.01; // Small tolerance for floating point
}

bool ConsumerABSTranche::canStepDown(double portfolio_performance) const {
    if (!has_step_down_provisions) return false;
    return portfolio_performance >= step_down_threshold;
}

bool ConsumerABSTranche::shouldActivateTurbo(double excess_spread) const {
    if (!has_turbo_provisions) return false;
    return excess_spread >= turbo_threshold;
}

// ConsumerABSWaterfall implementation
ConsumerABSWaterfall::ConsumerABSWaterfall() : has_excess_spread_capture(true), 
                                               minimum_excess_spread_target(0.015),
                                               has_principal_payment_window(true),
                                               payment_window_days(5),
                                               cash_collateral_account_target(0.0),
                                               spread_account_target(0.0),
                                               portfolio_yield_trigger(0.08),
                                               loss_rate_trigger(0.03) {
    
    // Standard Consumer ABS interest payment sequence
    interest_payment_sequence = {
        "servicing_fee",
        "trustee_fee",
        "class_a_interest",
        "class_b_interest", 
        "class_c_interest",
        "class_d_interest"
        // Residual gets remainder
    };
    
    // Standard Consumer ABS principal payment sequence
    principal_payment_sequence = {
        "class_a_principal",
        "class_b_principal",
        "class_c_principal", 
        "class_d_principal"
        // Residual gets remainder
    };
    
    // Loss allocation (reverse of payment waterfall)
    loss_allocation = {
        {"RESIDUAL", 1.0},      // First loss
        {"CLASS_D", 0.9},
        {"CLASS_C", 0.7},
        {"CLASS_B", 0.5},
        {"CLASS_A", 0.2}        // Last loss (most protected)
    };
    
    // Standard Consumer ABS overcollateralization ratios
    overcollateralization_ratios = {
        {"CLASS_A", 1.25},      // 125% OC test
        {"CLASS_B", 1.20},      // 120% OC test  
        {"CLASS_C", 1.15},      // 115% OC test
        {"CLASS_D", 1.10}       // 110% OC test
    };
    
    // Early amortization triggers
    early_amortization_triggers = {
        {"excess_spread", 0.005},       // 50 bps minimum excess spread
        {"delinquency_rate", 0.08},     // 8% delinquency trigger
        {"loss_rate", 0.03},            // 3% loss rate trigger
        {"servicer_rating", 3.0}        // Servicer rating threshold
    };
    
    // Rapid amortization triggers (more severe)
    rapid_amortization_triggers = {
        {"excess_spread", 0.002},       // 20 bps minimum excess spread
        {"delinquency_rate", 0.12},     // 12% delinquency trigger
        {"loss_rate", 0.05},            // 5% loss rate trigger
        {"servicer_default", 1.0}       // Servicer default
    };
}

std::map<std::string, Amount> ConsumerABSWaterfall::processInterestWaterfall(
    Amount available_interest,
    const std::map<std::string, ConsumerABSTranche>& tranches,
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
                std::string interest_key = consumerABSTrancheTypeToString(tranche.tranche_type) + "_interest";
                std::transform(interest_key.begin(), interest_key.end(), interest_key.begin(), ::tolower);
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
    
    // Any remaining goes to excess spread capture or residual
    if (remaining > 0.0) {
        if (has_excess_spread_capture && remaining > minimum_excess_spread_target) {
            distributions["excess_spread_account"] = remaining * 0.5;
            distributions["residual_distribution"] = remaining * 0.5;
        } else {
            distributions["residual_distribution"] = remaining;
        }
    }
    
    return distributions;
}

std::map<std::string, Amount> ConsumerABSWaterfall::processPrincipalWaterfall(
    Amount available_principal,
    const std::map<std::string, ConsumerABSTranche>& tranches,
    bool early_amortization_triggered) const {
    
    std::map<std::string, Amount> distributions;
    Amount remaining = available_principal;
    
    // If early amortization is triggered, pay down sequentially
    if (early_amortization_triggered) {
        for (const auto& payment_item : principal_payment_sequence) {
            if (remaining <= 0.0) break;
            
            for (const auto& [tranche_id, tranche] : tranches) {
                std::string principal_key = consumerABSTrancheTypeToString(tranche.tranche_type) + "_principal";
                std::transform(principal_key.begin(), principal_key.end(), principal_key.begin(), ::tolower);
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
    } else {
        // Normal amortization - pro rata to all tranches or targeted paydown
        distributions["controlled_amortization"] = remaining;
    }
    
    // Any remaining goes to residual
    if (remaining > 0.0) {
        distributions["residual_distribution"] = remaining;
    }
    
    return distributions;
}

// ConsumerABSDeal implementation
ConsumerABSDeal::ConsumerABSDeal(const DealInfo& deal_info) : DealBase(deal_info), consumer_pool_(nullptr),
                                                             servicing_fee_rate_(0.0075), 
                                                             trustee_fee_rate_(0.0002),
                                                             monthly_servicing_fee_(0.0),
                                                             monthly_trustee_fee_(0.0) {
    
    // Set up standard Consumer ABS accounts
    principal_collection_account_id_ = "principal_collection";
    interest_collection_account_id_ = "interest_collection";
    reserve_account_id_ = "reserve";
    spread_account_id_ = "spread";
    cash_collateral_account_id_ = "cash_collateral";
    servicer_advance_account_id_ = "servicer_advance";
    charge_off_account_id_ = "charge_off";
    
    // Create accounts with zero initial balance
    addAccount(principal_collection_account_id_, std::make_unique<Account>(principal_collection_account_id_, 0.0));
    addAccount(interest_collection_account_id_, std::make_unique<Account>(interest_collection_account_id_, 0.0));
    addAccount(reserve_account_id_, std::make_unique<Account>(reserve_account_id_, 0.0));
    addAccount(spread_account_id_, std::make_unique<Account>(spread_account_id_, 0.0));
    addAccount(cash_collateral_account_id_, std::make_unique<Account>(cash_collateral_account_id_, 0.0));
    addAccount(servicer_advance_account_id_, std::make_unique<Account>(servicer_advance_account_id_, 0.0));
    addAccount(charge_off_account_id_, std::make_unique<Account>(charge_off_account_id_, 0.0));
}

// Stub implementations for the remaining methods to enable compilation
void ConsumerABSDeal::setConsumerPool(Pool<ConsumerInstallment>*) {}
void ConsumerABSDeal::addTranche(const ConsumerABSTranche&) {}
void ConsumerABSDeal::setupWaterfall(const ConsumerABSWaterfall&) {}
void ConsumerABSDeal::setupServicer(const ConsumerABSServicer&) {}
void ConsumerABSDeal::setServicingFees(Rate, Rate) {}

void ConsumerABSDeal::runWaterfall(const Date&) {}
Balance ConsumerABSDeal::calculateDealValue(const Date&) const { return 0.0; }
std::vector<std::string> ConsumerABSDeal::validate() const { return validateBasicDealStructure(); }
std::string ConsumerABSDeal::getDealSummary() const { return "Consumer ABS Deal: " + getDealName(); }
void ConsumerABSDeal::processPaymentDate(const Date& date) { setCurrentDate(date); }

// All other methods as stubs
void ConsumerABSDeal::processCustomerPayments(Date) {}
void ConsumerABSDeal::processDelinquencies(Date) {}
void ConsumerABSDeal::processChargeOffs(Date) {}
void ConsumerABSDeal::handleLoanModifications(Date) {}
bool ConsumerABSDeal::checkPerformanceTriggers(Date) { return true; }
void ConsumerABSDeal::handleEarlyAmortization(Date, const std::string&) {}
void ConsumerABSDeal::handleRapidAmortization(Date, const std::string&) {}
void ConsumerABSDeal::allocateChargeOffs(Amount, Date) {}
Amount ConsumerABSDeal::calculateExpectedLosses() const { return 0.0; }
Amount ConsumerABSDeal::calculateUnexpectedLosses(double) const { return 0.0; }
void ConsumerABSDeal::updateCreditEnhancements(Date) {}
Amount ConsumerABSDeal::calculateServicingFees(Date) const { return 0.0; }
Amount ConsumerABSDeal::calculateTrusteeFees(Date) const { return 0.0; }
void ConsumerABSDeal::payServicingFees(Date) {}
double ConsumerABSDeal::calculatePortfolioYield() const { return 0.0; }
double ConsumerABSDeal::calculateExcessSpread() const { return 0.0; }
double ConsumerABSDeal::calculateMonthlyPaymentRate() const { return 0.0; }
std::map<std::string, double> ConsumerABSDeal::getTrancheMetrics() const { return {}; }
std::map<int, double> ConsumerABSDeal::getDelinquencyRates() const { return {}; }
std::map<std::string, Amount> ConsumerABSDeal::getGeographicConcentration() const { return {}; }
std::map<std::string, Amount> ConsumerABSDeal::getFICODistribution() const { return {}; }
std::map<std::string, Amount> ConsumerABSDeal::getLoanPurposeDistribution() const { return {}; }
std::map<int, Amount> ConsumerABSDeal::getSeasoningDistribution() const { return {}; }
Amount ConsumerABSDeal::calculateRequiredReserves() const { return 0.0; }
Amount ConsumerABSDeal::calculateCashCollateralRequirement() const { return 0.0; }
Amount ConsumerABSDeal::calculateSpreadAccountRequirement() const { return 0.0; }
void ConsumerABSDeal::manageReserveAccounts(Date) {}
void ConsumerABSDeal::applyDelinquencyStress(double) {}
void ConsumerABSDeal::applyChargeOffStress(double) {}
void ConsumerABSDeal::applyInterestRateStress(double) {}
std::map<std::string, Amount> ConsumerABSDeal::stressTestWaterfall(double, double) const { return {}; }
std::map<std::string, double> ConsumerABSDeal::generateMonthlyReport(Date) const { return {}; }
std::map<int, Amount> ConsumerABSDeal::generateDelinquencyReport(Date) const { return {}; }
std::map<std::string, Amount> ConsumerABSDeal::generateCashFlowReport(Date, Date) const { return {}; }
std::map<std::string, double> ConsumerABSDeal::generatePortfolioStatistics() const { return {}; }

// Private method stubs
void ConsumerABSDeal::processConsumerCollections(Date) {}
void ConsumerABSDeal::updatePortfolioAnalytics(Date) {}
void ConsumerABSDeal::distributeCollections(Amount, Date) {}
void ConsumerABSDeal::manageServicerAdvances(Date) {}
void ConsumerABSDeal::processCreditEnhancements(Date) {}
double ConsumerABSDeal::calculateConcentrationRisk() const { return 0.0; }
Amount ConsumerABSDeal::calculateRequiredOvercollateralization() const { return 0.0; }
void ConsumerABSDeal::updateDelinquencyMetrics() {}
void ConsumerABSDeal::monitorPerformanceTriggers() {}
void ConsumerABSDeal::accrueServicingFees(Date) {}
void ConsumerABSDeal::processServicingFeePayment(Date) {}
void ConsumerABSDeal::processTrusteeFeePayment(Date) {}
void ConsumerABSDeal::fundReserveAccounts(Date) {}
void ConsumerABSDeal::releaseExcessReserves(Date) {}
void ConsumerABSDeal::manageCashCollateral(Date) {}
void ConsumerABSDeal::manageSpreadAccount(Date) {}
void ConsumerABSDeal::evaluatePerformanceTriggers(Date) {}
void ConsumerABSDeal::handleTriggerActivation(const std::string&, Date) {}
void ConsumerABSDeal::adjustWaterfallForTriggers() {}
void ConsumerABSDeal::monitorServicingPerformance() {}
void ConsumerABSDeal::handleServicingTransfer(const std::string&, Date) {}
void ConsumerABSDeal::processServicerReplacements() {}
void ConsumerABSDeal::generateTrancheReports(Date) const {}
void ConsumerABSDeal::generatePortfolioPerformanceReport(Date) const {}
void ConsumerABSDeal::generateServicingReport(Date) const {}
void ConsumerABSDeal::generateTriggerStatusReport(Date) const {}

// Additional stub implementations for supporting classes
ConsumerABSServicer::ConsumerABSServicer() : servicing_fee_rate(0.0075) {}
Amount ConsumerABSServicer::calculateServicingFees(const Pool<ConsumerInstallment>&, Date) const { return 0.0; }
Amount ConsumerABSServicer::processCollections(Pool<ConsumerInstallment>&, Date) { return 0.0; }
void ConsumerABSServicer::processDelinquencies(Pool<ConsumerInstallment>&, Date) {}
void ConsumerABSServicer::processChargeOffs(Pool<ConsumerInstallment>&, Date) {}
void ConsumerABSServicer::processCustomerPayments(Pool<ConsumerInstallment>&, Date) {}
void ConsumerABSServicer::handleCustomerInquiries(const std::string&, const std::string&) {}
void ConsumerABSServicer::processLoanModifications(Pool<ConsumerInstallment>&, Date) {}
std::map<std::string, double> ConsumerABSServicer::generateServicingReport(Date) const { return {}; }
std::map<int, Amount> ConsumerABSServicer::getDelinquencyBucketReport(const Pool<ConsumerInstallment>&) const { return {}; }

ConsumerABSAnalytics::ConsumerABSAnalytics() : weighted_average_coupon(0.0) {}
void ConsumerABSAnalytics::calculateMetrics(const Pool<ConsumerInstallment>&, const std::map<std::string, ConsumerABSTranche>&) {}
std::map<std::string, double> ConsumerABSAnalytics::getDelinquencyAnalysis(const Pool<ConsumerInstallment>&) const { return {}; }
std::map<std::string, Amount> ConsumerABSAnalytics::getLossProjections(const Pool<ConsumerInstallment>&) const { return {}; }
std::map<std::string, double> ConsumerABSAnalytics::calculateTrancheMetrics(const std::map<std::string, ConsumerABSTranche>&) const { return {}; }
std::map<Date, double> ConsumerABSAnalytics::performVintageAnalysis(const Pool<ConsumerInstallment>&) const { return {}; }
std::map<int, double> ConsumerABSAnalytics::calculateSeasoningCurves(const Pool<ConsumerInstallment>&) const { return {}; }

ConsumerABSTriggersManager::ConsumerABSTriggersManager() : early_amortization_triggered(false) {}
bool ConsumerABSTriggersManager::evaluateEarlyAmortizationTriggers(const Pool<ConsumerInstallment>&, const ConsumerABSAnalytics&, Date) { return false; }
bool ConsumerABSTriggersManager::evaluateRapidAmortizationTriggers(const Pool<ConsumerInstallment>&, const ConsumerABSAnalytics&, Date) { return false; }
void ConsumerABSTriggersManager::activateEarlyAmortization(const std::string&, Date) {}
void ConsumerABSTriggersManager::activateRapidAmortization(const std::string&, Date) {}
void ConsumerABSTriggersManager::resetTriggers(Date) {}
std::map<std::string, double> ConsumerABSTriggersManager::getCurrentTriggerStatus() const { return {}; }
std::map<Date, std::string> ConsumerABSTriggersManager::getTriggerHistory() const { return {}; }

std::map<std::string, Amount> ConsumerABSWaterfall::processLossWaterfall(Amount, const std::map<std::string, ConsumerABSTranche>&) const { return {}; }
bool ConsumerABSWaterfall::checkEarlyAmortizationTriggers(const Pool<ConsumerInstallment>&, const std::map<std::string, ConsumerABSTranche>&) const { return false; }
bool ConsumerABSWaterfall::checkRapidAmortizationTriggers(const Pool<ConsumerInstallment>&) const { return false; }
Amount ConsumerABSWaterfall::calculateRequiredCashCollateral(const std::map<std::string, ConsumerABSTranche>&) const { return 0.0; }
Amount ConsumerABSWaterfall::calculateRequiredSpreadAccount(const Pool<ConsumerInstallment>&) const { return 0.0; }
bool ConsumerABSWaterfall::passesOvercollateralizationTests(const Pool<ConsumerInstallment>&, const std::map<std::string, ConsumerABSTranche>&) const { return true; }

} // namespace Structura