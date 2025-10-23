#pragma once

#include "pool.h"
#include <numeric>
#include <algorithm>
#include <stdexcept>

namespace Structura {

// Pool template implementations

template<typename AssetType>
Pool<AssetType>::Pool(const std::vector<AssetType>& assets, Date asOfDate)
    : assets_(assets), asOfDate_(asOfDate) {
}

template<typename AssetType>
Pool<AssetType>::Pool(std::vector<AssetType>&& assets, Date asOfDate)
    : assets_(std::move(assets)), asOfDate_(asOfDate) {
}

template<typename AssetType>
void Pool<AssetType>::addAsset(const AssetType& asset) {
    assets_.push_back(asset);
}

template<typename AssetType>
void Pool<AssetType>::addAsset(AssetType&& asset) {
    assets_.push_back(std::move(asset));
}

template<typename AssetType>
Balance Pool<AssetType>::getIssuanceField(CutoffFields field) const {
    if (!issuanceStats_) {
        throw std::runtime_error("No issuance statistics available for field: " + toString(field));
    }
    return issuanceStats_->getField(field);
}

template<typename AssetType>
Balance Pool<AssetType>::getCurrentBalance() const {
    return std::accumulate(assets_.begin(), assets_.end(), 0.0,
        [](Balance sum, const AssetType& asset) {
            return sum + asset.getCurrentBalance();
        });
}

template<typename AssetType>
Balance Pool<AssetType>::getOriginalBalance() const {
    return std::accumulate(assets_.begin(), assets_.end(), 0.0,
        [](Balance sum, const AssetType& asset) {
            return sum + asset.getOriginBalance();
        });
}

template<typename AssetType>
size_t Pool<AssetType>::getPerformingAssetCount() const {
    return std::count_if(assets_.begin(), assets_.end(),
        [](const AssetType& asset) {
            return asset.getStatus() == Status::Current;
        });
}

template<typename AssetType>
size_t Pool<AssetType>::getDefaultedAssetCount() const {
    return std::count_if(assets_.begin(), assets_.end(),
        [](const AssetType& asset) {
            return asset.getStatus() == Status::Defaulted;
        });
}

template<typename AssetType>
PoolCashflow Pool<AssetType>::aggregateAssetCashflows(int periods) const {
    if (assets_.empty()) {
        return PoolCashflow{};
    }
    
    // If periods not specified, use the maximum remaining terms from assets
    if (periods == 0) {
        periods = std::max_element(assets_.begin(), assets_.end(),
            [](const AssetType& a, const AssetType& b) {
                return a.getRemainingTerms() < b.getRemainingTerms();
            })->getRemainingTerms();
    }
    
    PoolCashflow poolCashflow;
    poolCashflow.dates.reserve(periods);
    poolCashflow.principalPayments.reserve(periods);
    poolCashflow.interestPayments.reserve(periods);
    poolCashflow.prepayments.reserve(periods);
    poolCashflow.defaults.reserve(periods);
    poolCashflow.recoveries.reserve(periods);
    poolCashflow.remainingBalances.reserve(periods);
    
    // Generate dates for the periods
    Date currentDate = asOfDate_;
    for (int i = 0; i < periods; ++i) {
        currentDate = currentDate + QuantLib::Period(1, QuantLib::Months);
        poolCashflow.dates.push_back(currentDate);
    }
    
    // Initialize all cashflow vectors with zeros
    poolCashflow.principalPayments.assign(periods, 0.0);
    poolCashflow.interestPayments.assign(periods, 0.0);
    poolCashflow.prepayments.assign(periods, 0.0);
    poolCashflow.defaults.assign(periods, 0.0);
    poolCashflow.recoveries.assign(periods, 0.0);
    poolCashflow.remainingBalances.assign(periods, 0.0);
    
    // Aggregate cashflows from all assets
    for (const auto& asset : assets_) {
        // For now, focus on basic payment aggregation
        // In a full implementation, this would project each asset's cashflow
        
        Balance currentBalance = asset.getCurrentBalance();
        int assetTerms = std::min(asset.getRemainingTerms(), periods);
        
        // Simple aggregation: distribute the current balance across remaining terms
        for (int i = 0; i < assetTerms && i < periods; ++i) {
            if (asset.getStatus() == Status::Current) {
                auto [interest, principal] = asset.calculateNextPayment();
                poolCashflow.interestPayments[i] += interest;
                poolCashflow.principalPayments[i] += principal;
                
                // Update remaining balance
                currentBalance -= principal;
                poolCashflow.remainingBalances[i] += currentBalance;
            } else if (asset.getStatus() == Status::Defaulted) {
                // For defaulted assets, add to defaults in first period
                if (i == 0) {
                    poolCashflow.defaults[i] += currentBalance;
                }
            }
        }
    }
    
    return poolCashflow;
}

template<typename AssetType>
Balance Pool<AssetType>::calculateLiquidationAmount(const PricingMethod& pricingMethod, Date valuationDate) const {
    if (!futureCashflow_) {
        return 0.0;
    }
    
    return pricingMethod.calculateValue(*futureCashflow_, valuationDate);
}

template<typename AssetType>
std::vector<PoolCashflow> Pool<AssetType>::runProjection(int periods) const {
    std::vector<PoolCashflow> projections;
    
    // For now, return a single aggregated cashflow
    // In a full implementation, this would run multiple scenarios
    projections.push_back(aggregateAssetCashflows(periods));
    
    return projections;
}

template<typename AssetType>
PoolStats Pool<AssetType>::calculateCurrentStats() const {
    PoolStats stats;
    
    Balance currentBalance = 0.0;
    Balance originalBalance = 0.0;
    Balance defaultBalance = 0.0;
    
    for (const auto& asset : assets_) {
        originalBalance += asset.getOriginBalance();
        currentBalance += asset.getCurrentBalance();
        
        if (asset.getStatus() == Status::Defaulted) {
            defaultBalance += asset.getCurrentBalance();
        }
    }
    
    stats.setField(CutoffFields::IssuanceBalance, originalBalance);
    stats.setField(CutoffFields::RuntimeCurrentPoolBalance, currentBalance);
    stats.setField(CutoffFields::HistoryDefaults, defaultBalance);
    
    return stats;
}

template<typename AssetType>
Balance Pool<AssetType>::pricePool(const PricingMethod& pricingMethod, Date pricingDate) const {
    // If we have future cashflow, use it for pricing
    if (futureCashflow_) {
        return pricingMethod.calculateValue(*futureCashflow_, pricingDate);
    }
    
    // Otherwise, aggregate current cashflows and price
    PoolCashflow cashflow = aggregateAssetCashflows();
    return pricingMethod.calculateValue(cashflow, pricingDate);
}

// PoolUtils template implementations
namespace PoolUtils {

template<typename AssetType>
PoolStats calculatePoolStats(const std::vector<AssetType>& assets) {
    PoolStats stats;
    
    Balance totalOriginal = 0.0;
    Balance totalCurrent = 0.0;
    Balance totalDefaults = 0.0;
    Balance totalInterest = 0.0;
    Balance totalPrincipal = 0.0;
    
    for (const auto& asset : assets) {
        totalOriginal += asset.getOriginBalance();
        totalCurrent += asset.getCurrentBalance();
        
        if (asset.getStatus() == Status::Defaulted) {
            totalDefaults += asset.getCurrentBalance();
        }
        
        // Calculate paid amounts (original - current for performing assets)
        if (asset.getStatus() == Status::Current) {
            Balance paidAmount = asset.getOriginBalance() - asset.getCurrentBalance();
            totalPrincipal += paidAmount; // Simplified - assumes all is principal
        }
    }
    
    stats.setField(CutoffFields::IssuanceBalance, totalOriginal);
    stats.setField(CutoffFields::RuntimeCurrentPoolBalance, totalCurrent);
    stats.setField(CutoffFields::HistoryDefaults, totalDefaults);
    stats.setField(CutoffFields::HistoryPrincipal, totalPrincipal);
    stats.setField(CutoffFields::HistoryInterest, totalInterest);
    
    return stats;
}

} // namespace PoolUtils

} // namespace Structura