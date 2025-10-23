#ifndef STRUCTURA_MORTGAGE_DEAL_H
#define STRUCTURA_MORTGAGE_DEAL_H

#include "../core/deal_base.h"
#include "../assets/mortgage.h"
#include "../assets/pool.h"
#include <memory>

namespace Structura {

/**
 * @brief Bond/tranche information for mortgage-backed securities
 */
struct MortgageBond {
    std::string bondName;
    std::string rating;
    Balance originalBalance;
    Balance currentBalance;
    Rate couponRate;
    int paymentPriority;  // Lower numbers pay first
    bool isSubordinated;
    
    MortgageBond(const std::string& name, Balance balance, Rate rate, int priority)
        : bondName(name), originalBalance(balance), currentBalance(balance),
          couponRate(rate), paymentPriority(priority), isSubordinated(false) {}
};

/**
 * @brief Waterfall structure for mortgage deals
 */
struct MortgageWaterfall {
    struct WaterfallStep {
        std::string description;
        std::string targetAccount;
        std::string sourceAccount;
        Balance maxAmount;  // 0 = no limit
        
        WaterfallStep(const std::string& desc, const std::string& target,
                     const std::string& source = "Collection", Balance max = 0.0)
            : description(desc), targetAccount(target), sourceAccount(source), maxAmount(max) {}
    };
    
    std::vector<WaterfallStep> steps;
    
    void addStep(const WaterfallStep& step) {
        steps.push_back(step);
    }
};

/**
 * @brief Concrete implementation for mortgage-backed security deals
 * 
 * Handles mortgage pool management, MBS bond payments, and mortgage-specific
 * waterfall logic including credit enhancement and subordination.
 */
class MortgageDeal : public DealBase {
private:
    std::unique_ptr<Pool<Mortgage>> mortgagePool_;
    std::vector<MortgageBond> bonds_;
    MortgageWaterfall waterfall_;
    
    // Deal-specific metrics
    Balance totalOriginalBalance_;
    Balance currentPoolBalance_;
    Rate weightedAverageCoupon_;
    int weightedAverageMaturity_;
    
    // Performance metrics
    Balance cumulativePrincipalCollections_;
    Balance cumulativeInterestCollections_;
    Balance cumulativePrepayments_;
    Balance cumulativeDefaults_;
    Balance cumulativeRecoveries_;

public:
    /**
     * @brief Construct a new Mortgage Deal
     */
    MortgageDeal(const DealInfo& dealInfo, std::unique_ptr<Pool<Mortgage>> pool);
    
    // Pool Management
    void setMortgagePool(std::unique_ptr<Pool<Mortgage>> pool);
    const Pool<Mortgage>* getMortgagePool() const { return mortgagePool_.get(); }
    Pool<Mortgage>* getMortgagePool() { return mortgagePool_.get(); }
    
    // Bond Management
    void addBond(const MortgageBond& bond);
    const std::vector<MortgageBond>& getBonds() const { return bonds_; }
    MortgageBond* getBond(const std::string& bondName);
    Balance getTotalBondBalance() const;
    
    // Waterfall Management
    void setWaterfall(const MortgageWaterfall& waterfall) { waterfall_ = waterfall; }
    const MortgageWaterfall& getWaterfall() const { return waterfall_; }
    
    // Deal Metrics
    Balance getTotalOriginalBalance() const { return totalOriginalBalance_; }
    Balance getCurrentPoolBalance() const;
    Rate getWeightedAverageCoupon() const;
    int getWeightedAverageMaturity() const;
    
    // Performance Metrics
    Balance getCumulativePrincipalCollections() const { return cumulativePrincipalCollections_; }
    Balance getCumulativeInterestCollections() const { return cumulativeInterestCollections_; }
    Balance getCumulativePrepayments() const { return cumulativePrepayments_; }
    Balance getCumulativeDefaults() const { return cumulativeDefaults_; }
    Balance getCumulativeRecoveries() const { return cumulativeRecoveries_; }
    
    // Loss and delinquency metrics
    Rate getCurrentLossRate() const;
    Rate getCumulativeLossRate() const;
    Rate getDelinquencyRate() const;
    Balance get60PlusDayDelinquencies() const;
    
    // Implementation of pure virtual methods from DealBase
    void runWaterfall(const Date& paymentDate) override;
    Balance calculateDealValue(const Date& valuationDate) const override;
    std::vector<std::string> validate() const override;
    std::string getDealSummary() const override;
    
    // Override virtual methods with mortgage-specific logic
    void initialize() override;
    bool canAccelerate() const override;
    
    // Mortgage-specific operations
    void collectPoolCashflows(const Date& collectionDate);
    void distributePrincipalToBonds(Balance principalAmount);
    void distributeInterestToBonds(Balance interestAmount);
    void handleDefaults(const Date& asOfDate);
    void processRecoveries(Balance recoveryAmount, const Date& recoveryDate);
    
    // Static helper methods
    static MortgageWaterfall createStandardWaterfall();
    static std::vector<Date> generateMonthlyPaymentSchedule(const Date& firstPayment, 
                                                           const Date& maturity);

private:
    void updatePoolMetrics();
    void updatePerformanceMetrics(const PoolCashflow& poolCashflow);
    Balance calculateSubordinationAmount() const;
    void executeWaterfallStep(const MortgageWaterfall::WaterfallStep& step, Balance availableAmount);
};

} // namespace Structura

#endif // STRUCTURA_MORTGAGE_DEAL_H