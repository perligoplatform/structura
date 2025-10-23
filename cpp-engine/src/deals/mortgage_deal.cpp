#include "mortgage_deal.h"
#include <algorithm>
#include <numeric>
#include <sstream>
#include <cmath>

namespace Structura {

MortgageDeal::MortgageDeal(const DealInfo& dealInfo, std::unique_ptr<Pool<Mortgage>> pool)
    : DealBase(dealInfo), mortgagePool_(std::move(pool)),
      totalOriginalBalance_(0.0), currentPoolBalance_(0.0),
      weightedAverageCoupon_(0.0), weightedAverageMaturity_(0),
      cumulativePrincipalCollections_(0.0), cumulativeInterestCollections_(0.0),
      cumulativePrepayments_(0.0), cumulativeDefaults_(0.0), cumulativeRecoveries_(0.0) {
    
    if (mortgagePool_) {
        updatePoolMetrics();
    }
}

void MortgageDeal::setMortgagePool(std::unique_ptr<Pool<Mortgage>> pool) {
    mortgagePool_ = std::move(pool);
    if (mortgagePool_) {
        updatePoolMetrics();
    }
}

void MortgageDeal::addBond(const MortgageBond& bond) {
    bonds_.push_back(bond);
    // Sort bonds by payment priority (lower numbers first)
    std::sort(bonds_.begin(), bonds_.end(), 
             [](const MortgageBond& a, const MortgageBond& b) {
                 return a.paymentPriority < b.paymentPriority;
             });
}

MortgageBond* MortgageDeal::getBond(const std::string& bondName) {
    auto it = std::find_if(bonds_.begin(), bonds_.end(),
                          [&bondName](const MortgageBond& bond) {
                              return bond.bondName == bondName;
                          });
    return (it != bonds_.end()) ? &(*it) : nullptr;
}

Balance MortgageDeal::getTotalBondBalance() const {
    return std::accumulate(bonds_.begin(), bonds_.end(), 0.0,
                          [](Balance sum, const MortgageBond& bond) {
                              return sum + bond.currentBalance;
                          });
}

Balance MortgageDeal::getCurrentPoolBalance() const {
    if (!mortgagePool_) {
        return 0.0;
    }
    
    return mortgagePool_->getCurrentBalance();
}

Rate MortgageDeal::getWeightedAverageCoupon() const {
    if (!mortgagePool_) {
        return 0.0;
    }
    
    Balance totalBalance = 0.0;
    Rate weightedRate = 0.0;
    
    for (const auto& mortgage : mortgagePool_->getAssets()) {
        Balance balance = mortgage.getCurrentBalance();
        Rate rate = mortgage.getCurrentRate();
        totalBalance += balance;
        weightedRate += balance * rate;
    }
    
    return totalBalance > 0.0 ? weightedRate / totalBalance : 0.0;
}

int MortgageDeal::getWeightedAverageMaturity() const {
    if (!mortgagePool_) {
        return 0;
    }
    
    // Calculate WAM based on remaining months to maturity
    Balance totalBalance = 0.0;
    double weightedMaturity = 0.0;
    
    for (const auto& mortgage : mortgagePool_->getAssets()) {
        Balance balance = mortgage.getCurrentBalance();
        int remainingMonths = mortgage.getRemainingTerms();
        
        totalBalance += balance;
        weightedMaturity += balance * remainingMonths;
    }
    
    if (totalBalance <= 0) {
        return 0;
    }
    
    return static_cast<int>(weightedMaturity / totalBalance);
}

Rate MortgageDeal::getCurrentLossRate() const {
    if (totalOriginalBalance_ == 0.0) {
        return 0.0;
    }
    
    Balance currentLosses = cumulativeDefaults_ - cumulativeRecoveries_;
    return currentLosses / getCurrentPoolBalance();
}

Rate MortgageDeal::getCumulativeLossRate() const {
    if (totalOriginalBalance_ == 0.0) {
        return 0.0;
    }
    
    Balance netLosses = cumulativeDefaults_ - cumulativeRecoveries_;
    return netLosses / totalOriginalBalance_;
}

Rate MortgageDeal::getDelinquencyRate() const {
    if (!mortgagePool_) {
        return 0.0;
    }
    
    Balance delinquentBalance = 0.0;
    Balance totalBalance = getCurrentPoolBalance();
    
    for (const auto& mortgage : mortgagePool_->getAssets()) {
        if (mortgage.getStatus() == Status::Defaulted) {
            delinquentBalance += mortgage.getCurrentBalance();
        }
    }
    
    return (totalBalance > 0) ? delinquentBalance / totalBalance : 0.0;
}

Balance MortgageDeal::get60PlusDayDelinquencies() const {
    if (!mortgagePool_) {
        return 0.0;
    }
    
    Balance delinquentBalance = 0.0;
    for (const auto& mortgage : mortgagePool_->getAssets()) {
        // Assuming we have a way to check delinquency days
        if (mortgage.getStatus() == Status::Defaulted) {
            delinquentBalance += mortgage.getCurrentBalance();
        }
    }
    
    return delinquentBalance;
}

void MortgageDeal::runWaterfall(const Date& paymentDate) {
    // First, collect cashflows from the mortgage pool
    collectPoolCashflows(paymentDate);
    
    // Handle any defaults that occurred this period
    handleDefaults(paymentDate);
    
    // Execute each step of the waterfall
    for (const auto& step : waterfall_.steps) {
        Balance availableAmount = getAvailableAccountBalance(step.sourceAccount);
        if (availableAmount > 0.0) {
            executeWaterfallStep(step, availableAmount);
        }
    }
}

Balance MortgageDeal::calculateDealValue(const Date& valuationDate) const {
    if (!mortgagePool_) {
        return 0.0;
    }
    
    // Use the pool's pricing mechanism
    // For now, return current pool balance as approximate value
    // In future, implement proper pricing using calculateLiquidationAmount
    return mortgagePool_->getCurrentBalance();
}

std::vector<std::string> MortgageDeal::validate() const {
    std::vector<std::string> errors = validateBasicDealStructure();
    
    // Mortgage-specific validations
    if (!mortgagePool_) {
        errors.push_back("Mortgage deal must have a mortgage pool");
    }
    
    if (bonds_.empty()) {
        errors.push_back("Mortgage deal must have at least one bond");
    }
    
    // Check that bond balances don't exceed pool balance
    Balance totalBondBalance = getTotalBondBalance();
    Balance poolBalance = getCurrentPoolBalance();
    if (totalBondBalance > poolBalance * 1.01) {  // Allow 1% tolerance
        errors.push_back("Total bond balance exceeds pool balance");
    }
    
    // Check that all bonds have valid priorities
    for (const auto& bond : bonds_) {
        if (bond.paymentPriority < 0) {
            errors.push_back("Bond " + bond.bondName + " has invalid payment priority");
        }
    }
    
    // Check waterfall structure
    if (waterfall_.steps.empty()) {
        errors.push_back("Mortgage deal must have a waterfall structure");
    }
    
    return errors;
}

std::string MortgageDeal::getDealSummary() const {
    std::ostringstream summary;
    
    summary << "=== MORTGAGE DEAL SUMMARY ===\n";
    summary << "Deal Name: " << getDealName() << "\n";
    summary << "Status: " << statusToString(getStatus()) << "\n";
    summary << "Current Date: " << getCurrentDate() << "\n\n";
    
    summary << "=== POOL INFORMATION ===\n";
    summary << "Original Balance: $" << totalOriginalBalance_ << "\n";
    summary << "Current Balance: $" << getCurrentPoolBalance() << "\n";
    summary << "WAC: " << (getWeightedAverageCoupon() * 100) << "%\n";
    summary << "WAM: " << getWeightedAverageMaturity() << " months\n\n";
    
    summary << "=== PERFORMANCE METRICS ===\n";
    summary << "Cumulative Principal Collections: $" << cumulativePrincipalCollections_ << "\n";
    summary << "Cumulative Interest Collections: $" << cumulativeInterestCollections_ << "\n";
    summary << "Cumulative Prepayments: $" << cumulativePrepayments_ << "\n";
    summary << "Cumulative Defaults: $" << cumulativeDefaults_ << "\n";
    summary << "Cumulative Recoveries: $" << cumulativeRecoveries_ << "\n";
    summary << "Current Loss Rate: " << (getCurrentLossRate() * 100) << "%\n";
    summary << "Cumulative Loss Rate: " << (getCumulativeLossRate() * 100) << "%\n";
    summary << "Delinquency Rate: " << (getDelinquencyRate() * 100) << "%\n\n";
    
    summary << "=== BOND INFORMATION ===\n";
    for (const auto& bond : bonds_) {
        summary << bond.bondName << ": $" << bond.currentBalance 
                << " (" << (bond.couponRate * 100) << "%, Priority " 
                << bond.paymentPriority << ")\n";
    }
    
    return summary.str();
}

void MortgageDeal::initialize() {
    DealBase::initialize();
    
    // Create standard accounts for mortgage deals
    addAccount("Collection", std::make_unique<Account>("Collection Account", 0.0));
    addAccount("Reserve", std::make_unique<Account>("Reserve Account", 0.0));
    addAccount("Excess", std::make_unique<Account>("Excess Spread", 0.0));
    
    // Create waterfall if not already set
    if (waterfall_.steps.empty()) {
        waterfall_ = createStandardWaterfall();
    }
    
    // Set up monthly payment schedule if not already set
    if (getPaymentDates().empty()) {
        auto paymentDates = generateMonthlyPaymentSchedule(
            dealInfo_.firstPaymentDate, dealInfo_.maturityDate);
        setPaymentDates(paymentDates);
    }
}

bool MortgageDeal::canAccelerate() const {
    // Mortgage deals can typically be accelerated if losses exceed threshold
    if (!DealBase::canAccelerate()) {
        return false;
    }
    
    // Check if losses exceed acceleration threshold (e.g., 5%)
    Rate lossThreshold = 0.05;  // 5%
    return getCumulativeLossRate() > lossThreshold;
}

void MortgageDeal::collectPoolCashflows(const Date& collectionDate) {
    if (!mortgagePool_) {
        return;
    }
    
    // Collect cashflows from the pool
    PoolCashflow poolCashflow = mortgagePool_->aggregateAssetCashflows(1);
    
    // Update performance metrics
    updatePerformanceMetrics(poolCashflow);
    
    // Deposit collections into collection account
    Account* collectionAccount = getAccount("Collection");
    if (collectionAccount) {
        Balance totalCollections = 0.0;
        if (!poolCashflow.principalPayments.empty()) {
            totalCollections += poolCashflow.principalPayments[0];
        }
        if (!poolCashflow.interestPayments.empty()) {
            totalCollections += poolCashflow.interestPayments[0];
        }
        if (!poolCashflow.prepayments.empty()) {
            totalCollections += poolCashflow.prepayments[0];
        }
        if (!poolCashflow.recoveries.empty()) {
            totalCollections += poolCashflow.recoveries[0];
        }
        
        collectionAccount->deposit(collectionDate, totalCollections, "Pool collections");
    }
}

void MortgageDeal::distributePrincipalToBonds(Balance principalAmount) {
    // Distribute principal sequentially by priority
    Balance remainingPrincipal = principalAmount;
    
    for (auto& bond : bonds_) {
        if (remainingPrincipal <= 0.0) {
            break;
        }
        
        Balance paymentAmount = std::min(remainingPrincipal, bond.currentBalance);
        bond.currentBalance -= paymentAmount;
        remainingPrincipal -= paymentAmount;
    }
}

void MortgageDeal::distributeInterestToBonds(Balance interestAmount) {
    // Distribute interest pro-rata based on current balances
    Balance totalBondBalance = getTotalBondBalance();
    if (totalBondBalance == 0.0) {
        return;
    }
    
    Balance remainingInterest = interestAmount;
    
    for (auto& bond : bonds_) {
        if (remainingInterest <= 0.0) {
            break;
        }
        
        Balance proRataShare = (bond.currentBalance / totalBondBalance) * interestAmount;
        Balance couponPayment = bond.currentBalance * bond.couponRate / 12.0;  // Monthly
        Balance paymentAmount = std::min({proRataShare, couponPayment, remainingInterest});
        
        remainingInterest -= paymentAmount;
    }
}

void MortgageDeal::handleDefaults(const Date& asOfDate) {
    if (!mortgagePool_) {
        return;
    }
    
    Balance defaultAmount = 0.0;
    
    for (const auto& mortgage : mortgagePool_->getAssets()) {
        if (mortgage.getStatus() == Status::Defaulted) {
            defaultAmount += mortgage.getCurrentBalance();
        }
    }
    
    cumulativeDefaults_ += defaultAmount;
}

void MortgageDeal::processRecoveries(Balance recoveryAmount, const Date& recoveryDate) {
    cumulativeRecoveries_ += recoveryAmount;
    
    // Deposit recoveries into collection account
    Account* collectionAccount = getAccount("Collection");
    if (collectionAccount) {
        collectionAccount->deposit(recoveryDate, recoveryAmount, "Recoveries");
    }
}

MortgageWaterfall MortgageDeal::createStandardWaterfall() {
    MortgageWaterfall waterfall;
    
    // Standard mortgage deal waterfall
    waterfall.addStep({"Fees and Expenses", "Fees", "Collection"});
    waterfall.addStep({"Senior Interest", "SeniorInterest", "Collection"});
    waterfall.addStep({"Senior Principal", "SeniorPrincipal", "Collection"});
    waterfall.addStep({"Reserve Fund", "Reserve", "Collection"});
    waterfall.addStep({"Subordinate Interest", "SubordinateInterest", "Collection"});
    waterfall.addStep({"Subordinate Principal", "SubordinatePrincipal", "Collection"});
    waterfall.addStep({"Excess Spread", "Excess", "Collection"});
    
    return waterfall;
}

std::vector<Date> MortgageDeal::generateMonthlyPaymentSchedule(const Date& firstPayment, 
                                                             const Date& maturity) {
    std::vector<Date> paymentDates;
    Date currentDate = firstPayment;
    
    while (currentDate <= maturity) {
        paymentDates.push_back(currentDate);
        // Add one month (approximate - in real implementation would use proper date arithmetic)
        currentDate = Date(currentDate.serialNumber() + 30);
    }
    
    return paymentDates;
}

void MortgageDeal::updatePoolMetrics() {
    if (!mortgagePool_) {
        return;
    }
    
    const auto stats = mortgagePool_->calculateCurrentStats();
    totalOriginalBalance_ = stats.getField(CutoffFields::IssuanceBalance);
    currentPoolBalance_ = stats.getField(CutoffFields::RuntimeCurrentPoolBalance);
    // Note: weighted average coupon would need to be calculated separately
}

void MortgageDeal::updatePerformanceMetrics(const PoolCashflow& poolCashflow) {
    if (!poolCashflow.principalPayments.empty()) {
        cumulativePrincipalCollections_ += poolCashflow.principalPayments[0];
    }
    if (!poolCashflow.interestPayments.empty()) {
        cumulativeInterestCollections_ += poolCashflow.interestPayments[0];
    }
    if (!poolCashflow.prepayments.empty()) {
        cumulativePrepayments_ += poolCashflow.prepayments[0];
    }
    // Defaults and recoveries are handled separately
}

Balance MortgageDeal::calculateSubordinationAmount() const {
    Balance subordinateBalance = 0.0;
    
    for (const auto& bond : bonds_) {
        if (bond.isSubordinated) {
            subordinateBalance += bond.currentBalance;
        }
    }
    
    return subordinateBalance;
}

void MortgageDeal::executeWaterfallStep(const MortgageWaterfall::WaterfallStep& step, 
                                      Balance availableAmount) {
    Balance transferAmount = availableAmount;
    
    // Apply maximum amount limit if specified
    if (step.maxAmount > 0.0) {
        transferAmount = std::min(transferAmount, step.maxAmount);
    }
    
    if (transferAmount > 0.0) {
        transferBetweenAccounts(step.sourceAccount, step.targetAccount, transferAmount);
    }
}

} // namespace Structura