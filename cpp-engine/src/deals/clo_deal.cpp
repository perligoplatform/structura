// Minimal working CLO deal implementation - stub methods to fix compilation
#include "clo_deal.h"
#include "../core/types.h"
#include <stdexcept>

namespace Structura {

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

// CLOTranche stub implementation
CLOTranche::CLOTranche(const std::string& id, CLOTrancheType type, const std::string& rating,
                       Amount notional, Rate coupon, bool floating)
    : tranche_id(id), tranche_type(type), credit_rating(rating), notional_amount(notional),
      outstanding_balance(notional), coupon_rate(coupon), is_floating_rate(floating) {}

Amount CLOTranche::calculateInterestPayment(Rate) const { return outstanding_balance * coupon_rate / 4.0; }
Amount CLOTranche::calculatePrincipalPayment(Amount available) const { return std::min(available, outstanding_balance); }
void CLOTranche::makePayment(Amount principal, Amount interest, Date) { 
    outstanding_balance -= principal; 
    total_principal_paid += principal;
    total_interest_paid += interest;
}
double CLOTranche::getCurrentYield(Rate) const { return coupon_rate; }
double CLOTranche::getCumulativeLossRate() const { return cumulative_losses / notional_amount; }
bool CLOTranche::isFullyPaid() const { return outstanding_balance <= 0.01; }

// CLOWaterfall stub implementation  
CLOWaterfall::CLOWaterfall() : reinvestment_allowed(true), maximum_ccc_concentration(0.075),
                               maximum_single_obligor_concentration(0.02), minimum_weighted_average_spread(0.04) {}

std::map<std::string, Amount> CLOWaterfall::processInterestWaterfall(Amount available, const std::map<std::string, CLOTranche>&, const std::map<std::string, Amount>&) const {
    return {{"equity_distribution", available}};
}

std::map<std::string, Amount> CLOWaterfall::processPrincipalWaterfall(Amount available, const std::map<std::string, CLOTranche>&) const {
    return {{"reinvestment", available}};
}

std::map<std::string, Amount> CLOWaterfall::processLossWaterfall(Amount losses, const std::map<std::string, CLOTranche>&) const {
    return {{"equity_loss", losses}};
}

bool CLOWaterfall::passesOvercollateralizationTests(const Pool<CorporateLoan>&, const std::map<std::string, CLOTranche>&) const { return true; }
bool CLOWaterfall::passesInterestCoverageTests(Amount, const std::map<std::string, CLOTranche>&) const { return true; }
bool CLOWaterfall::passesQualityTests(const Pool<CorporateLoan>&) const { return true; }
bool CLOWaterfall::checkConcentrationLimits(const Pool<CorporateLoan>&) const { return true; }
bool CLOWaterfall::checkCreditQualityLimits(const Pool<CorporateLoan>&) const { return true; }
bool CLOWaterfall::checkMaturityProfile(const Pool<CorporateLoan>&) const { return true; }

// CLOPortfolioManager stub implementation
CLOPortfolioManager::CLOPortfolioManager() : actively_managed(true), total_reinvestments(0.0), total_dispositions(0.0) {}
bool CLOPortfolioManager::canReinvestProceeds(Amount, const Pool<CorporateLoan>&) const { return true; }
std::vector<std::string> CLOPortfolioManager::identifyTradingOpportunities(const Pool<CorporateLoan>&) const { return {}; }
bool CLOPortfolioManager::evaluateNewLoanAddition(const CorporateLoan&, const Pool<CorporateLoan>&) const { return true; }
std::map<std::string, double> CLOPortfolioManager::generateTradingReport() const { return {}; }
std::map<std::string, double> CLOPortfolioManager::calculatePortfolioMetrics(const Pool<CorporateLoan>&) const { return {}; }

// CLOAnalytics stub implementation
CLOAnalytics::CLOAnalytics() : weighted_average_spread(0.05), expected_credit_losses(0.0) {}
void CLOAnalytics::calculateMetrics(const Pool<CorporateLoan>&, const std::map<std::string, CLOTranche>&) {}
std::map<std::string, double> CLOAnalytics::getConcentrationAnalysis(const Pool<CorporateLoan>&) const { return {}; }
std::map<std::string, Amount> CLOAnalytics::getCreditLossProjections() const { return {}; }
std::map<std::string, double> CLOAnalytics::calculateTrancheMetrics(const std::map<std::string, CLOTranche>&) const { return {}; }

// CLODeal implementation
CLODeal::CLODeal(const DealInfo& deal_info) : DealBase(deal_info), loan_pool_(nullptr) {
    addAccount("principal_collection", std::make_unique<Account>("principal_collection", 0.0));
    addAccount("interest_collection", std::make_unique<Account>("interest_collection", 0.0));
}

void CLODeal::setLoanPool(Pool<CorporateLoan>* pool) { loan_pool_ = pool; }
void CLODeal::addTranche(const CLOTranche& tranche) { tranches_[tranche.tranche_id] = tranche; }
void CLODeal::setupWaterfall(const CLOWaterfall& waterfall) { waterfall_ = waterfall; }
void CLODeal::setupPortfolioManager(const CLOPortfolioManager& manager) { portfolio_manager_ = manager; }
void CLODeal::setManagementFees(Rate mgmt, Rate trustee, Rate admin) { 
    management_fee_rate_ = mgmt; 
    trustee_fee_rate_ = trustee;
    administrative_fee_rate_ = admin;
}

void CLODeal::runWaterfall(const Date&) {
    // Stub implementation
}

Balance CLODeal::calculateDealValue(const Date&) const {
    return loan_pool_ ? loan_pool_->getOriginalBalance() : 0.0;
}

std::vector<std::string> CLODeal::validate() const {
    auto errors = validateBasicDealStructure();
    if (!loan_pool_) errors.push_back("No loan pool assigned");
    return errors;
}

std::string CLODeal::getDealSummary() const {
    return "CLO Deal: " + getDealName();
}

void CLODeal::processPaymentDate(const Date& date) {
    setCurrentDate(date);
    runWaterfall(date);
}

// Stub implementations for remaining methods
void CLODeal::processLoanDefault(const std::string&, Date, Amount) {}
void CLODeal::processLoanPrepayment(const std::string&, Date, Amount) {}
void CLODeal::reinvestProceeds(Amount, Date) {}
void CLODeal::disposeLoan(const std::string&, Date, Amount) {}
bool CLODeal::runCoverageTests(Date) { return true; }
std::map<std::string, bool> CLODeal::checkPortfolioCompliance() const { return {}; }
void CLODeal::handleCoverageTestFailure(const std::string&, Date) {}
void CLODeal::allocateLosses(Amount, Date) {}
Amount CLODeal::calculateExpectedLosses() const { return 0.0; }
Amount CLODeal::calculateUnexpectedLosses(double) const { return 0.0; }
void CLODeal::updateCreditRatings(const std::map<std::string, std::string>&) {}
Amount CLODeal::calculateManagementFees(Date) const { return 0.0; }
Amount CLODeal::calculateTrusteeFees(Date) const { return 0.0; }
Amount CLODeal::calculateAdministrativeFees(Date) const { return 0.0; }
void CLODeal::payFees(Date) {}
double CLODeal::calculatePortfolioYield() const { return 0.05; }
double CLODeal::calculateExcessSpread() const { return 0.02; }
double CLODeal::calculateWeightedAverageLife() const { return 4.5; }
std::map<std::string, double> CLODeal::getTrancheMetrics() const { return {}; }
std::map<std::string, double> CLODeal::getCoverageRatios() const { return {}; }
std::map<std::string, Amount> CLODeal::getSectorConcentration() const { return {}; }
std::map<std::string, Amount> CLODeal::getObligorConcentration() const { return {}; }
std::map<std::string, Amount> CLODeal::getRatingConcentration() const { return {}; }
std::map<std::string, Amount> CLODeal::getGeographicConcentration() const { return {}; }
void CLODeal::applyDefaultScenario(double) {}
void CLODeal::applyRecoveryRateStress(double) {}
void CLODeal::applySpreadStress(double) {}
std::map<std::string, Amount> CLODeal::stressTestWaterfall(double, double) const { return {}; }
std::map<std::string, double> CLODeal::generateMonthlyReport(Date) const { return {}; }
std::map<std::string, Amount> CLODeal::generateCashFlowReport(Date, Date) const { return {}; }
std::map<std::string, double> CLODeal::generatePortfolioStatistics() const { return {}; }

// Private method stubs
void CLODeal::processLoanCollections(Date) {}
void CLODeal::updatePortfolioAnalytics(Date) {}
void CLODeal::distributeCollections(Amount, Date) {}
void CLODeal::manageReinvestmentAccount(Date) {}
void CLODeal::processEquityDistributions(Date) {}
double CLODeal::calculateConcentrationRisk() const { return 0.0; }
Amount CLODeal::calculateRequiredOvercollateralization() const { return 0.0; }
void CLODeal::updateCreditMetrics() {}
void CLODeal::monitorCoverageTests() {}
void CLODeal::accrueFees(Date) {}
void CLODeal::processManagementFeePayment(Date) {}
void CLODeal::processTrusteeFeePayment(Date) {}
void CLODeal::checkReinvestmentCompliance() {}
void CLODeal::checkTradingCompliance() {}
void CLODeal::checkPortfolioQualityTests() {}
void CLODeal::generateTrancheReports(Date) const {}
void CLODeal::generatePortfolioReport(Date) const {}
void CLODeal::generateCoverageTestReport(Date) const {}

} // namespace Structura