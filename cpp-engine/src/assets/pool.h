#pragma once

#include "core/types.h"
#include "assets/asset_base.h"
#include "assets/mortgage.h"
#include <vector>
#include <map>
#include <memory>
#include <optional>

namespace Structura {

// Pool-level statistics fields (from Types.hs CutoffFields)
enum class CutoffFields {
    IssuanceBalance,           // Pool issuance balance
    HistoryRecoveries,         // Cumulative recoveries
    HistoryInterest,           // Cumulative interest collected
    HistoryPrepayment,         // Cumulative prepayment collected
    HistoryPrepaymentPenalty,  // Cumulative prepayment penalty
    HistoryPrincipal,          // Cumulative principal collected
    HistoryRental,             // Cumulative rental collected
    HistoryDefaults,           // Cumulative default balance
    HistoryDelinquency,        // Cumulative delinquency balance
    HistoryLoss,               // Cumulative loss/write-off balance
    HistoryCash,               // Cumulative cash
    HistoryFeePaid,            // Cumulative fees paid
    AccruedInterest,           // Accrued interest at closing
    RuntimeCurrentPoolBalance  // Current pool balance
};

// String conversion for CutoffFields
std::string toString(CutoffFields field);

// Pool cashflow aggregation result
struct PoolCashflow {
    std::vector<Date> dates;
    std::vector<Balance> principalPayments;
    std::vector<Balance> interestPayments;
    std::vector<Balance> prepayments;
    std::vector<Balance> defaults;
    std::vector<Balance> recoveries;
    std::vector<Balance> remainingBalances;
    
    PoolCashflow() = default;
    
    // Get total cash flow for a period
    Balance getTotalCash(size_t index) const;
    
    // Check if cashflow is empty
    bool isEmpty() const;
    
    // Get size of cashflow
    size_t size() const;
};

// Pricing methods for pool valuation
class PricingMethod {
public:
    virtual ~PricingMethod() = default;
    virtual Balance calculateValue(const PoolCashflow& cashflow, Date valuationDate) const = 0;
    virtual std::string getMethodName() const = 0;
};

// Balance factor pricing (current balance * factor + default balance * factor)
class BalanceFactorPricing : public PricingMethod {
private:
    Rate currentFactor_;
    Rate defaultFactor_;
    
public:
    BalanceFactorPricing(Rate currentFactor, Rate defaultFactor);
    Balance calculateValue(const PoolCashflow& cashflow, Date valuationDate) const override;
    std::string getMethodName() const override;
};

// Present value pricing using discount rate
class PresentValuePricing : public PricingMethod {
private:
    Rate discountRate_;
    Rate recoveryRate_;
    
public:
    PresentValuePricing(Rate discountRate, Rate recoveryRate = 0.0);
    Balance calculateValue(const PoolCashflow& cashflow, Date valuationDate) const override;
    std::string getMethodName() const override;
};

// Forward declaration for Pool template
template<typename AssetType>
class Pool;

// Pool statistics aggregation
struct PoolStats {
    std::map<CutoffFields, Balance> statistics;
    
    PoolStats() = default;
    PoolStats(const std::map<CutoffFields, Balance>& stats) : statistics(stats) {}
    
    // Get a statistic value
    Balance getField(CutoffFields field) const;
    
    // Set a statistic value
    void setField(CutoffFields field, Balance value);
    
    // Combine with another PoolStats
    PoolStats& operator+=(const PoolStats& other);
    
    // Get beginning pool statistics (principal, prepay, delinquency, defaults, recoveries, loss)
    std::tuple<Balance, Balance, Balance, Balance, Balance, Balance> getBeginningStats() const;
};

// Pool class template to hold assets and manage cashflows
template<typename AssetType>
class Pool {
private:
    std::vector<AssetType> assets_;
    std::optional<PoolCashflow> futureCashflow_;
    std::optional<PoolCashflow> futureScheduleCashflow_;
    Date asOfDate_;
    std::optional<PoolStats> issuanceStats_;
    
public:
    // Constructors
    Pool() = default;
    Pool(const std::vector<AssetType>& assets, Date asOfDate);
    Pool(std::vector<AssetType>&& assets, Date asOfDate);
    
    // Asset management
    void addAsset(const AssetType& asset);
    void addAsset(AssetType&& asset);
    const std::vector<AssetType>& getAssets() const { return assets_; }
    std::vector<AssetType>& getAssets() { return assets_; }
    size_t getAssetCount() const { return assets_.size(); }
    
    // Date management
    Date getAsOfDate() const { return asOfDate_; }
    void setAsOfDate(Date date) { asOfDate_ = date; }
    
    // Cashflow management
    const std::optional<PoolCashflow>& getFutureCashflow() const { return futureCashflow_; }
    void setFutureCashflow(const PoolCashflow& cashflow) { futureCashflow_ = cashflow; }
    void setFutureCashflow(PoolCashflow&& cashflow) { futureCashflow_ = std::move(cashflow); }
    
    const std::optional<PoolCashflow>& getFutureScheduleCashflow() const { return futureScheduleCashflow_; }
    void setFutureScheduleCashflow(const PoolCashflow& cashflow) { futureScheduleCashflow_ = cashflow; }
    void setFutureScheduleCashflow(PoolCashflow&& cashflow) { futureScheduleCashflow_ = std::move(cashflow); }
    
    // Statistics management
    const std::optional<PoolStats>& getIssuanceStats() const { return issuanceStats_; }
    void setIssuanceStats(const PoolStats& stats) { issuanceStats_ = stats; }
    void setIssuanceStats(PoolStats&& stats) { issuanceStats_ = std::move(stats); }
    
    Balance getIssuanceField(CutoffFields field) const;
    
    // Pool operations
    Balance getCurrentBalance() const;
    Balance getOriginalBalance() const;
    size_t getPerformingAssetCount() const;
    size_t getDefaultedAssetCount() const;
    
    // Aggregate pool cashflows from all assets
    PoolCashflow aggregateAssetCashflows(int periods = 0) const;
    
    // Calculate liquidation amount using pricing method
    Balance calculateLiquidationAmount(const PricingMethod& pricingMethod, Date valuationDate) const;
    
    // Run pool projection with assumptions
    std::vector<PoolCashflow> runProjection(int periods) const;
    
    // Pool-level statistics
    PoolStats calculateCurrentStats() const;
    
    // Pricing functions
    Balance pricePool(const PricingMethod& pricingMethod, Date pricingDate) const;
};

// Pool utility functions
namespace PoolUtils {
    // Aggregate multiple pool cashflows
    PoolCashflow aggregateCashflows(const std::vector<PoolCashflow>& cashflows);
    
    // Calculate pool statistics from assets
    template<typename AssetType>
    PoolStats calculatePoolStats(const std::vector<AssetType>& assets);
    
    // Create mortgage pool from mortgages
    Pool<Mortgage> createMortgagePool(const std::vector<Mortgage>& mortgages, Date asOfDate);
}

} // namespace Structura

// Include template implementations
#include "pool_impl.h"