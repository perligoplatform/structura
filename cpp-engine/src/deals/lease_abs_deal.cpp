#include "lease_abs_deal.h"
#include <algorithm>
#include <cmath>
#include <stdexcept>
#include <numeric>

namespace Structura {

// LeaseTrancheType conversion
std::string leaseTrancheTypeToString(LeaseTrancheType type) {
    switch (type) {
        case LeaseTrancheType::SENIOR_A: return "Senior A";
        case LeaseTrancheType::SENIOR_B: return "Senior B";
        case LeaseTrancheType::MEZZANINE: return "Mezzanine";
        case LeaseTrancheType::SUBORDINATE: return "Subordinate";
        case LeaseTrancheType::RESIDUAL: return "Residual";
        default: return "Unknown";
    }
}

// LeaseABSTranche implementation
LeaseABSTranche::LeaseABSTranche(const std::string& id, LeaseTrancheType type, Amount notional, Rate coupon)
    : tranche_id(id), tranche_type(type), notional_amount(notional), outstanding_balance(notional),
      coupon_rate(coupon), payment_frequency(PaymentFrequency::Quarterly) {
    
    // Set default subordination levels
    switch (type) {
        case LeaseTrancheType::SENIOR_A:
            subordination_percentage = 0.0;
            credit_enhancement_level = 0.15;
            break;
        case LeaseTrancheType::SENIOR_B:
            subordination_percentage = 0.05;
            credit_enhancement_level = 0.10;
            break;
        case LeaseTrancheType::MEZZANINE:
            subordination_percentage = 0.15;
            credit_enhancement_level = 0.05;
            break;
        case LeaseTrancheType::SUBORDINATE:
            subordination_percentage = 0.25;
            credit_enhancement_level = 0.0;
            break;
        case LeaseTrancheType::RESIDUAL:
            subordination_percentage = 1.0;
            credit_enhancement_level = 0.0;
            break;
    }
    
    reserve_account_target = notional * credit_enhancement_level;
    has_yield_maintenance = (type <= LeaseTrancheType::SENIOR_B);
    has_call_protection = (type <= LeaseTrancheType::MEZZANINE);
    
    total_principal_paid = 0.0;
    total_interest_paid = 0.0;
    cumulative_losses = 0.0;
}

Amount LeaseABSTranche::calculateInterestPayment() const {
    if (outstanding_balance <= 0) return 0.0;
    
    double periods_per_year = static_cast<double>(payment_frequency);
    return outstanding_balance * (coupon_rate / periods_per_year);
}

Amount LeaseABSTranche::calculatePrincipalPayment(Amount available_principal) const {
    return std::min(available_principal, outstanding_balance);
}

void LeaseABSTranche::makePayment(Amount principal, Amount interest, Date payment_date) {
    total_principal_paid += principal;
    total_interest_paid += interest;
    outstanding_balance = std::max(0.0, outstanding_balance - principal);
    last_payment_date = payment_date;
}

double LeaseABSTranche::getCurrentYield() const {
    if (outstanding_balance <= 0) return 0.0;
    return coupon_rate;
}

double LeaseABSTranche::getCumulativeLossRate() const {
    if (notional_amount <= 0) return 0.0;
    return cumulative_losses / notional_amount;
}

bool LeaseABSTranche::isFullyPaid() const {
    return outstanding_balance <= 0.01; // Small tolerance for rounding
}

// LeaseABSWaterfall implementation
std::map<std::string, Amount> LeaseABSWaterfall::processInterestWaterfall(
    Amount available_interest, const std::map<std::string, LeaseABSTranche>& tranches) const {
    
    std::map<std::string, Amount> distributions;
    Amount remaining_interest = available_interest;
    
    // Pay interest in order of seniority
    for (const auto& tranche_id : payment_sequence) {
        if (remaining_interest <= 0) break;
        
        auto tranche_it = tranches.find(tranche_id);
        if (tranche_it == tranches.end()) continue;
        
        const auto& tranche = tranche_it->second;
        Amount required_interest = tranche.calculateInterestPayment();
        Amount payment = std::min(required_interest, remaining_interest);
        
        distributions[tranche_id] = payment;
        remaining_interest -= payment;
    }
    
    return distributions;
}

std::map<std::string, Amount> LeaseABSWaterfall::processPrincipalWaterfall(
    Amount available_principal, const std::map<std::string, LeaseABSTranche>& tranches) const {
    
    std::map<std::string, Amount> distributions;
    Amount remaining_principal = available_principal;
    
    // Pay principal in order of seniority
    for (const auto& tranche_id : payment_sequence) {
        if (remaining_principal <= 0) break;
        
        auto tranche_it = tranches.find(tranche_id);
        if (tranche_it == tranches.end()) continue;
        
        const auto& tranche = tranche_it->second;
        Amount payment = tranche.calculatePrincipalPayment(remaining_principal);
        
        distributions[tranche_id] = payment;
        remaining_principal -= payment;
    }
    
    return distributions;
}

std::map<std::string, Amount> LeaseABSWaterfall::processLossWaterfall(
    Amount total_losses, const std::map<std::string, LeaseABSTranche>& tranches) const {
    
    std::map<std::string, Amount> loss_allocations;
    Amount remaining_losses = total_losses;
    
    // Allocate losses in reverse order (subordinate first)
    auto reverse_sequence = payment_sequence;
    std::reverse(reverse_sequence.begin(), reverse_sequence.end());
    
    for (const auto& tranche_id : reverse_sequence) {
        if (remaining_losses <= 0) break;
        
        auto tranche_it = tranches.find(tranche_id);
        if (tranche_it == tranches.end()) continue;
        
        const auto& tranche = tranche_it->second;
        Amount tranche_capacity = tranche.outstanding_balance;
        Amount allocated_loss = std::min(remaining_losses, tranche_capacity);
        
        loss_allocations[tranche_id] = allocated_loss;
        remaining_losses -= allocated_loss;
    }
    
    return loss_allocations;
}

Amount LeaseABSWaterfall::calculateTotalReserveRequirement(const std::map<std::string, LeaseABSTranche>& tranches) const {
    Amount total_reserves = 0.0;
    
    for (const auto& [tranche_id, tranche] : tranches) {
        total_reserves += tranche.reserve_account_target;
    }
    
    return total_reserves;
}

bool LeaseABSWaterfall::checkCoverageRatios(Amount pool_cashflow, Amount required_payments) const {
    if (required_payments <= 0) return true;
    
    double coverage_ratio = pool_cashflow / required_payments;
    return coverage_ratio >= 1.05; // 105% coverage requirement
}

// ResidualValueManager implementation
ResidualValueManager::ResidualValueManager()
    : total_estimated_residual(0.0), guaranteed_residual_value(0.0), realized_residual_value(0.0),
      residual_value_ratio(0.0), guarantee_coverage_ratio(0.0), residual_value_insurance(0.0) {}

void ResidualValueManager::updateResidualEstimates(const Pool<Lease>& lease_pool) {
    total_estimated_residual = 0.0;
    guaranteed_residual_value = 0.0;
    
    auto leases = lease_pool.getAssets();
    for (const auto& lease : leases) {
        total_estimated_residual += lease.getEstimatedResidualValue();
        guaranteed_residual_value += lease.getGuaranteedResidualValue();
    }
    
    residual_value_ratio = lease_pool.getCurrentBalance() > 0 ? 
                          total_estimated_residual / lease_pool.getCurrentBalance() : 0.0;
    
    guarantee_coverage_ratio = total_estimated_residual > 0 ? 
                              guaranteed_residual_value / total_estimated_residual : 0.0;
}

Amount ResidualValueManager::processResidualRealization(Date realization_date, Amount realized_amount) {
    realized_residual_value += realized_amount;
    residual_realization_schedule[realization_date] = realized_amount;
    
    return realized_amount;
}

Amount ResidualValueManager::calculateResidualRisk() const {
    // Risk is the potential shortfall from guaranteed residual value
    Amount potential_shortfall = std::max(0.0, guaranteed_residual_value - total_estimated_residual);
    
    // Apply volatility factor
    double volatility_factor = 0.25; // 25% volatility assumption
    
    return potential_shortfall * volatility_factor;
}

void ResidualValueManager::addResidualValueInsurance(Amount coverage_amount, Rate premium_rate) {
    residual_value_insurance = coverage_amount;
    // Premium would be tracked separately in practice
}

Amount ResidualValueManager::calculateInsuranceClaim(Amount shortfall) const {
    return std::min(shortfall, residual_value_insurance);
}

// LeaseABSAnalytics implementation
LeaseABSAnalytics::LeaseABSAnalytics()
    : weighted_average_lease_term(0.0), weighted_average_remaining_term(0.0),
      weighted_average_payment(0.0), concentration_ratio_by_lessee(0.0),
      concentration_ratio_by_equipment_type(0.0), portfolio_credit_score(0.0),
      expected_credit_losses(0.0), unexpected_credit_losses(0.0),
      default_probability(0.0), residual_value_ratio(0.0),
      total_residual_exposure(0.0), residual_realization_rate(0.0) {}

void LeaseABSAnalytics::calculateMetrics(const Pool<Lease>& lease_pool) {
    auto leases = lease_pool.getAssets();
    if (leases.empty()) return;
    
    Amount total_balance = lease_pool.getCurrentBalance();
    Amount weighted_term = 0.0;
    Amount weighted_payment = 0.0;
    double weighted_credit_score = 0.0;
    
    for (const auto& lease : leases) {
        Amount balance = lease.getCurrentBalance();
        if (balance <= 0) continue;
        
        double weight = balance / total_balance;
        
        weighted_term += lease.getLeaseTermMonths() * weight;
        weighted_payment += lease.getMonthlyPayment() * weight;
        weighted_credit_score += lease.getLessee().getCreditRiskScore() * weight;
        
        total_residual_exposure += lease.getEstimatedResidualValue();
    }
    
    weighted_average_lease_term = weighted_term;
    weighted_average_payment = weighted_payment;
    portfolio_credit_score = weighted_credit_score;
    residual_value_ratio = total_balance > 0 ? total_residual_exposure / total_balance : 0.0;
    
    // Simplified credit loss calculation
    expected_credit_losses = total_balance * 0.02 * (1.0 - portfolio_credit_score);
    unexpected_credit_losses = expected_credit_losses * 2.5; // Rough multiplier
    default_probability = 0.02 * (1.0 - portfolio_credit_score);
}

std::map<std::string, double> LeaseABSAnalytics::getConcentrationAnalysis(const Pool<Lease>& lease_pool) const {
    std::map<std::string, double> concentrations;
    
    // Simplified concentration analysis
    concentrations["Single Lessee Max"] = concentration_ratio_by_lessee;
    concentrations["Equipment Type Max"] = concentration_ratio_by_equipment_type;
    concentrations["Top 5 Lessees"] = concentration_ratio_by_lessee * 3.0; // Approximation
    
    return concentrations;
}

std::map<std::string, Amount> LeaseABSAnalytics::getCreditLossProjections() const {
    std::map<std::string, Amount> projections;
    
    projections["Expected Losses"] = expected_credit_losses;
    projections["Unexpected Losses (99%)"] = unexpected_credit_losses;
    projections["Best Case"] = expected_credit_losses * 0.5;
    projections["Stress Case"] = expected_credit_losses * 3.0;
    
    return projections;
}

// LeaseABSDeal implementation
LeaseABSDeal::LeaseABSDeal(const DealInfo& deal_info)
    : DealBase(deal_info), lease_pool_(nullptr) {
    
    // Create deal-specific accounts
    principal_collection_account_id_ = "principal_collection_" + deal_info.dealName;
    interest_collection_account_id_ = "interest_collection_" + deal_info.dealName;
    reserve_account_id_ = "reserve_" + deal_info.dealName;
    residual_account_id_ = "residual_" + deal_info.dealName;
    expense_account_id_ = "expense_" + deal_info.dealName;
    
    // Add accounts to the base class account map
    accounts_[principal_collection_account_id_] = Account(principal_collection_account_id_, AccountType::PRINCIPAL_COLLECTION);
    accounts_[interest_collection_account_id_] = Account(interest_collection_account_id_, AccountType::INTEREST_COLLECTION);
    accounts_[reserve_account_id_] = Account(reserve_account_id_, AccountType::RESERVE);
    accounts_[residual_account_id_] = Account(residual_account_id_, AccountType::CASH);
    accounts_[expense_account_id_] = Account(expense_account_id_, AccountType::EXPENSE);
}

void LeaseABSDeal::setLeasePool(Pool<Lease>* pool) {
    lease_pool_ = pool;
    
    if (pool) {
        // Initialize analytics with pool data
        analytics_.calculateMetrics(*pool);
        residual_manager_.updateResidualEstimates(*pool);
    }
}

void LeaseABSDeal::addTranche(const LeaseABSTranche& tranche) {
    tranches_[tranche.tranche_id] = tranche;
}

void LeaseABSDeal::setupWaterfall(const LeaseABSWaterfall& waterfall) {
    waterfall_ = waterfall;
}

void LeaseABSDeal::setupResidualValueManagement(const ResidualValueManager& manager) {
    residual_manager_ = manager;
}

bool LeaseABSDeal::validateDeal() const {
    // Check that we have tranches
    if (tranches_.empty()) return false;
    
    // Check that waterfall is configured
    if (waterfall_.payment_sequence.empty()) return false;
    
    // Check that all tranches in waterfall exist
    for (const auto& tranche_id : waterfall_.payment_sequence) {
        if (tranches_.find(tranche_id) == tranches_.end()) return false;
    }
    
    // Check that we have a lease pool
    if (!lease_pool_) return false;
    
    // Check that required accounts exist
    if (accounts_.find(reserve_account_id_) == accounts_.end()) return false;
    
    return true;
}

void LeaseABSDeal::processPaymentDate(Date payment_date) {
    if (status_ != DealStatus::ACTIVE) return;
    
    // Process lease collections
    processLeaseCollections(payment_date);
    
    // Update portfolio analytics
    updatePortfolioAnalytics(payment_date);
    
    // Distribute collections through waterfall
    Amount total_collections = 1000000.0; // Simplified - would calculate from lease pool
    distributeCollections(total_collections, payment_date);
    
    // Manage reserve accounts
    manageReserveAccounts(payment_date);
    
    // Process residual value realizations
    processResidualValue(payment_date);
    
    // Update last payment date
    last_payment_date_ = payment_date;
}

std::map<std::string, Amount> LeaseABSDeal::calculateMetrics(Date as_of_date) const {
    std::map<std::string, Amount> metrics;
    
    // Tranche metrics
    Amount total_notional = 0.0;
    Amount total_outstanding = 0.0;
    for (const auto& [tranche_id, tranche] : tranches_) {
        total_notional += tranche.notional_amount;
        total_outstanding += tranche.outstanding_balance;
    }
    
    metrics["total_notional"] = total_notional;
    metrics["total_outstanding"] = total_outstanding;
    metrics["total_principal_paid"] = total_notional - total_outstanding;
    
    // Portfolio metrics
    metrics["portfolio_balance"] = lease_pool_ ? lease_pool_->getCurrentBalance() : 0.0;
    metrics["expected_losses"] = analytics_.expected_credit_losses;
    metrics["residual_exposure"] = analytics_.total_residual_exposure;
    metrics["weighted_avg_term"] = analytics_.weighted_average_lease_term;
    
    // Performance metrics
    metrics["portfolio_yield"] = calculatePortfolioYield();
    metrics["excess_spread"] = calculateExcessSpread();
    metrics["weighted_avg_life"] = calculateWeightedAverageLife();
    
    return metrics;
}

std::string LeaseABSDeal::getStatus() const {
    if (status_ == DealStatus::ACTIVE) {
        // Check for triggers that might change status
        double expected_loss_rate = 0.0;
        if (lease_pool_) {
            Amount pool_balance = lease_pool_->getCurrentBalance();
            if (pool_balance > 0) {
                expected_loss_rate = analytics_.expected_credit_losses / pool_balance;
            }
        }
        
        if (expected_loss_rate > 0.05) {
            return "Active - High Loss Environment";
        }
        
        double residual_risk = getResidualValueRatio();
        if (residual_risk > 0.4) {
            return "Active - High Residual Risk";
        }
        
        return "Active - Performing";
    }
    
    return dealStatusToString(status_);
}

void LeaseABSDeal::updateAssumptions(const std::map<std::string, double>& new_assumptions) {
    for (const auto& [key, value] : new_assumptions) {
        if (key == "default_rate_multiplier") {
            analytics_.default_probability *= value;
            analytics_.expected_credit_losses *= value;
        } else if (key == "residual_value_stress") {
            residual_manager_.total_estimated_residual *= (1.0 - value);
            analytics_.total_residual_exposure *= (1.0 - value);
        } else if (key == "prepayment_rate") {
            // Would update prepayment assumptions
        }
    }
    
    // Recalculate analytics if we have a pool
    if (lease_pool_) {
        analytics_.calculateMetrics(*lease_pool_);
    }
}

std::vector<Date> LeaseABSDeal::getPaymentDates(Date start_date, Date end_date) const {
    std::vector<Date> dates;
    
    // Use quarterly payment schedule for lease ABS
    Date current = start_date;
    while (current <= end_date) {
        dates.push_back(current);
        current = current + 90; // Quarterly
    }
    
    return dates;
}

void LeaseABSDeal::generateReports(Date report_date) const {
    generateTrancheReports(report_date);
    generatePortfolioReport(report_date);
    generateResidualValueReport(report_date);
}

// Lease ABS specific implementations
void LeaseABSDeal::processLeaseMaturity(const std::string& lease_id, Date maturity_date) {
    // Process final lease payment and residual value realization
    // Implementation would depend on specific lease terms
}

void LeaseABSDeal::processEarlyTermination(const std::string& lease_id, Date termination_date, Amount termination_value) {
    // Process early termination payment and equipment return
    residual_manager_.processResidualRealization(termination_date, termination_value);
}

void LeaseABSDeal::processResidualRealization(const std::string& lease_id, Amount realized_value) {
    Date current_date = Date(); // Would use actual date
    residual_manager_.processResidualRealization(current_date, realized_value);
    
    // Deposit to residual account
    auto residual_it = accounts_.find(residual_account_id_);
    if (residual_it != accounts_.end()) {
        residual_it->second.deposit(realized_value);
    }
}

void LeaseABSDeal::processDefault(const std::string& lease_id, Date default_date, Amount recovery_amount) {
    // Calculate loss amount
    Amount total_loss = 100000.0; // Simplified - would calculate based on lease
    Amount net_loss = std::max(0.0, total_loss - recovery_amount);
    
    // Allocate losses through waterfall
    allocateLosses(net_loss, default_date);
    
    // Track historical losses
    historical_losses_[default_date] = net_loss;
}

void LeaseABSDeal::allocateLosses(Amount total_losses, Date loss_date) {
    auto loss_allocations = waterfall_.processLossWaterfall(total_losses, tranches_);
    
    // Apply losses to tranches
    for (auto& [tranche_id, allocation] : loss_allocations) {
        auto tranche_it = tranches_.find(tranche_id);
        if (tranche_it != tranches_.end()) {
            tranche_it->second.cumulative_losses += allocation;
            tranche_it->second.outstanding_balance = std::max(0.0, 
                tranche_it->second.outstanding_balance - allocation);
        }
    }
}

Amount LeaseABSDeal::calculateExpectedLosses() const {
    return analytics_.expected_credit_losses;
}

Amount LeaseABSDeal::calculateUnexpectedLosses(double confidence_level) const {
    return analytics_.unexpected_credit_losses;
}

// Portfolio analysis methods
std::map<LeaseType, Amount> LeaseABSDeal::getLeaseTypeConcentration() const {
    std::map<LeaseType, Amount> concentrations;
    
    if (!lease_pool_) return concentrations;
    
    auto leases = lease_pool_->getAssets();
    for (const auto& lease : leases) {
        concentrations[lease.getLeaseType()] += lease.getCurrentBalance();
    }
    
    return concentrations;
}

std::map<std::string, Amount> LeaseABSDeal::getLesseeConcentration() const {
    std::map<std::string, Amount> concentrations;
    
    if (!lease_pool_) return concentrations;
    
    auto leases = lease_pool_->getAssets();
    for (const auto& lease : leases) {
        std::string lessee_id = lease.getLessee().lessee_id;
        concentrations[lessee_id] += lease.getCurrentBalance();
    }
    
    return concentrations;
}

std::map<std::string, Amount> LeaseABSDeal::getGeographicConcentration() const {
    std::map<std::string, Amount> concentrations;
    
    // Simplified implementation - would need geographic data in lease structure
    concentrations["Northeast"] = 0.0;
    concentrations["Southeast"] = 0.0;
    concentrations["Midwest"] = 0.0;
    concentrations["Southwest"] = 0.0;
    concentrations["West"] = 0.0;
    
    return concentrations;
}

// Performance metrics
double LeaseABSDeal::calculatePortfolioYield() const {
    if (!lease_pool_) return 0.0;
    
    Amount total_balance = lease_pool_->getCurrentBalance();
    if (total_balance <= 0) return 0.0;
    
    Amount weighted_yield = 0.0;
    auto leases = lease_pool_->getAssets();
    
    for (const auto& lease : leases) {
        Amount balance = lease.getCurrentBalance();
        Rate lease_rate = lease.getCurrentRate();
        weighted_yield += (balance / total_balance) * lease_rate;
    }
    
    return weighted_yield;
}

double LeaseABSDeal::calculateExcessSpread() const {
    double portfolio_yield = calculatePortfolioYield();
    double weighted_coupon = 0.0;
    Amount total_notional = 0.0;
    
    for (const auto& [tranche_id, tranche] : tranches_) {
        weighted_coupon += tranche.notional_amount * tranche.coupon_rate;
        total_notional += tranche.notional_amount;
    }
    
    if (total_notional > 0) {
        weighted_coupon /= total_notional;
    }
    
    return portfolio_yield - weighted_coupon;
}

double LeaseABSDeal::calculateWeightedAverageLife() const {
    return analytics_.weighted_average_remaining_term / 12.0; // Convert months to years
}

std::map<std::string, double> LeaseABSDeal::getTrancheMetrics() const {
    std::map<std::string, double> metrics;
    
    for (const auto& [tranche_id, tranche] : tranches_) {
        std::string prefix = tranche_id + "_";
        metrics[prefix + "current_yield"] = tranche.getCurrentYield();
        metrics[prefix + "loss_rate"] = tranche.getCumulativeLossRate();
        metrics[prefix + "paydown_rate"] = tranche.notional_amount > 0 ? 
            tranche.total_principal_paid / tranche.notional_amount : 0.0;
    }
    
    return metrics;
}

// Residual value methods
Amount LeaseABSDeal::getTotalResidualExposure() const {
    return analytics_.total_residual_exposure;
}

double LeaseABSDeal::getResidualValueRatio() const {
    return analytics_.residual_value_ratio;
}

std::map<Date, Amount> LeaseABSDeal::getResidualRealizationSchedule() const {
    return residual_manager_.residual_realization_schedule;
}

Amount LeaseABSDeal::calculateResidualValueRisk() const {
    return residual_manager_.calculateResidualRisk();
}

// Stress testing
void LeaseABSDeal::applyLeaseDefaultScenario(double default_rate_multiplier) {
    analytics_.default_probability *= default_rate_multiplier;
    analytics_.expected_credit_losses *= default_rate_multiplier;
    analytics_.unexpected_credit_losses *= default_rate_multiplier;
}

void LeaseABSDeal::applyResidualValueStress(double residual_value_shock) {
    residual_manager_.total_estimated_residual *= (1.0 - residual_value_shock);
    analytics_.total_residual_exposure *= (1.0 - residual_value_shock);
    analytics_.residual_value_ratio *= (1.0 - residual_value_shock);
}

void LeaseABSDeal::applyInterestRateStress(double rate_shock) {
    // Apply rate shock to floating rate tranches
    for (auto& [tranche_id, tranche] : tranches_) {
        tranche.coupon_rate += rate_shock;
    }
}

std::map<std::string, Amount> LeaseABSDeal::stressTestWaterfall(double loss_scenario, double residual_shock) const {
    // Apply stress scenarios
    Amount stressed_losses = analytics_.expected_credit_losses * (1.0 + loss_scenario);
    Amount available_funds = 1000000.0 * (1.0 - loss_scenario); // Simplified
    
    // Process stressed waterfall
    return waterfall_.processInterestWaterfall(available_funds, tranches_);
}

// Private helper methods
void LeaseABSDeal::processLeaseCollections(Date collection_date) {
    if (!lease_pool_) return;
    
    Amount total_collections = 0.0;
    auto leases = lease_pool_->getAssets();
    
    for (const auto& lease : leases) {
        Amount payment = lease.getMonthlyPayment();
        total_collections += payment;
    }
    
    // Deposit collections to appropriate accounts
    auto principal_it = accounts_.find(principal_collection_account_id_);
    auto interest_it = accounts_.find(interest_collection_account_id_);
    
    if (principal_it != accounts_.end() && interest_it != accounts_.end()) {
        Amount principal_portion = total_collections * 0.7; // Simplified split
        Amount interest_portion = total_collections * 0.3;
        
        principal_it->second.deposit(principal_portion);
        interest_it->second.deposit(interest_portion);
    }
    
    historical_collections_[collection_date] = total_collections;
}

void LeaseABSDeal::updatePortfolioAnalytics(Date update_date) {
    if (lease_pool_) {
        analytics_.calculateMetrics(*lease_pool_);
        residual_manager_.updateResidualEstimates(*lease_pool_);
    }
    
    // Store historical metrics
    portfolio_metrics_[update_date] = analytics_.portfolio_credit_score;
}

void LeaseABSDeal::distributeCollections(Amount collections, Date distribution_date) {
    // Split collections into interest and principal
    Amount interest_collections = collections * 0.3; // Simplified
    Amount principal_collections = collections * 0.7;
    
    // Distribute through waterfall
    auto interest_distributions = waterfall_.processInterestWaterfall(interest_collections, tranches_);
    auto principal_distributions = waterfall_.processPrincipalWaterfall(principal_collections, tranches_);
    
    // Apply payments to tranches
    for (auto& [tranche_id, tranche] : tranches_) {
        Amount interest_payment = interest_distributions[tranche_id];
        Amount principal_payment = principal_distributions[tranche_id];
        
        tranche.makePayment(principal_payment, interest_payment, distribution_date);
    }
}

void LeaseABSDeal::manageReserveAccounts(Date management_date) {
    Amount required_reserves = calculateRequiredReserves();
    
    auto reserve_it = accounts_.find(reserve_account_id_);
    if (reserve_it != accounts_.end()) {
        Amount current_reserves = reserve_it->second.getBalance();
        
        if (current_reserves < required_reserves) {
            // Would trigger reserve funding mechanism
        }
    }
}

void LeaseABSDeal::processResidualValue(Date processing_date) {
    // Check for lease maturities and process residual value realizations
    // Implementation would depend on specific lease maturity events
}

double LeaseABSDeal::calculateConcentrationRisk() const {
    if (!lease_pool_) return 0.0;
    
    auto lessee_concentrations = getLesseeConcentration();
    Amount total_balance = lease_pool_->getCurrentBalance();
    
    double max_concentration = 0.0;
    for (const auto& [lessee_id, balance] : lessee_concentrations) {
        double concentration = balance / total_balance;
        max_concentration = std::max(max_concentration, concentration);
    }
    
    return max_concentration;
}

Amount LeaseABSDeal::calculateRequiredReserves() const {
    return waterfall_.calculateTotalReserveRequirement(tranches_);
}

void LeaseABSDeal::updateCreditMetrics() {
    if (lease_pool_) {
        analytics_.calculateMetrics(*lease_pool_);
    }
}

// Reporting methods
void LeaseABSDeal::generateTrancheReports(Date report_date) const {
    // Generate detailed tranche performance reports
    // Implementation would create comprehensive reporting
}

void LeaseABSDeal::generatePortfolioReport(Date report_date) const {
    // Generate portfolio-level analytics report
    // Implementation would create detailed portfolio analysis
}

void LeaseABSDeal::generateResidualValueReport(Date report_date) const {
    // Generate residual value analysis report
    // Implementation would create residual value tracking reports
}

} // namespace Structura