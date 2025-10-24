#pragma once

#include "../core/types.h"
#include "../core/financial_types.h"
#include "../core/cashflow.h"
#include "../core/date_utils_enhanced.h"
#include <functional>

namespace Structura {

// Additional type mappings for enhanced utilities
using Rational = QuantLib::Real;  // Map Rational to QuantLib::Real for floating point operations

// Cutoff pair type for range finding
using CutoffPair = std::pair<DateUtils::CutoffType, DateUtils::CutoffType>;

}

using namespace Structura;
using CutoffType = Structura::DateUtils::CutoffType;
#include <vector>
#include <optional>
#include <string>
#include <map>
#include <variant>
#include <functional>
#include <algorithm>
#include <numeric>
#include <cmath>
#include <limits>

namespace Structura {
namespace Utils {

// ============================================================================
// MATHEMATICAL OPERATIONS
// ============================================================================

// Balance and Rate arithmetic operations
Balance mulBR(Balance balance, Rate rate);
Balance mulBIR(Balance balance, IRate rate);
Amount mulBI(Balance balance, IRate rate);
Rational mulIR(int multiplier, Rational rate);
Balance divideBI(Balance balance, int divisor);
Rational divideBB(Balance numerator, Balance denominator);

// Safe division operations
enum class DivisionBehavior {
    RETURN_INFINITY,
    RETURN_ZERO,
    RETURN_OPTIONAL
};

template<typename T>
T safeDivide(T numerator, T denominator, DivisionBehavior behavior = DivisionBehavior::RETURN_INFINITY);

std::optional<Rational> safeDiv(Rational numerator, Rational denominator);

// Split balance by rate
std::pair<Balance, Balance> splitBal(Rate rate, Balance balance);

// Rounding operations
enum class RoundingDirection {
    FLOOR,
    CEIL,
    ROUND
};

template<typename T>
T roundingBy(T value, T precision, RoundingDirection direction);

std::optional<Rational> roundingByM(std::optional<Rational> precision, Rational value, RoundingDirection direction);

// Min/Max operations for vectors
template<typename T>
T maximum(const std::vector<T>& values);

template<typename T>  
T minimum(const std::vector<T>& values);

// ============================================================================
// TIME SERIES TYPES AND OPERATIONS
// ============================================================================

// Time series point structure matching Haskell TsPoint
template<typename T>
struct TsPoint {
    Date date;
    T value;
    
    TsPoint(Date d, T v) : date(d), value(v) {}
    
    bool operator<(const TsPoint& other) const {
        return date < other.date;
    }
};

// Time series curve types matching Haskell Ts variants
struct FloatCurve {
    std::vector<TsPoint<Rational>> points;
    
    FloatCurve() = default;
    FloatCurve(std::vector<TsPoint<Rational>> pts) : points(std::move(pts)) {}
};

struct BalanceCurve {
    std::vector<TsPoint<Balance>> points;
    
    BalanceCurve() = default;
    BalanceCurve(std::vector<TsPoint<Balance>> pts) : points(std::move(pts)) {}
};

struct IRateCurve {
    std::vector<TsPoint<IRate>> points;
    
    IRateCurve() = default;
    IRateCurve(std::vector<TsPoint<IRate>> pts) : points(std::move(pts)) {}
};

struct RatioCurve {
    std::vector<TsPoint<Rational>> points;
    
    RatioCurve() = default;
    RatioCurve(std::vector<TsPoint<Rational>> pts) : points(std::move(pts)) {}
};

// Time series variant type matching Haskell Ts
using TimeSeriesVariant = std::variant<FloatCurve, BalanceCurve, IRateCurve, RatioCurve>;

// Get value from time series by date with cutoff logic
Rational getValByDate(const TimeSeriesVariant& ts, DateUtils::CutoffType cutoff, Date date);
std::vector<Rational> getValByDates(const TimeSeriesVariant& ts, DateUtils::CutoffType cutoff, const std::vector<Date>& dates);

// Extract values and dates from time series
std::vector<Rational> getTsVals(const TimeSeriesVariant& ts);
std::vector<Date> getTsDates(const TimeSeriesVariant& ts);
size_t getTsSize(const TimeSeriesVariant& ts);

// Create time series from parallel vectors
TimeSeriesVariant zipTs(const std::vector<Date>& dates, const std::vector<Rational>& values);
TimeSeriesVariant zipBalTs(const std::vector<Date>& dates, const std::vector<Balance>& values);

// Time series operations
TimeSeriesVariant multiplyTs(DateUtils::CutoffType cutoff, const TimeSeriesVariant& ts1, const TimeSeriesVariant& ts2);
TimeSeriesVariant shiftTsByAmt(const TimeSeriesVariant& ts, Rational delta);

// ============================================================================
// COLLECTION UTILITIES
// ============================================================================

// List manipulation
template<typename T>
std::vector<T> lastN(int n, const std::vector<T>& vec);

template<typename T>
std::vector<T> dropLastN(int n, const std::vector<T>& vec);

template<typename T>
std::vector<T> replace(const std::vector<T>& vec, int index, const T& newValue);

template<typename T>
std::vector<T> paddingDefault(const T& defaultValue, const std::vector<T>& vec, int targetSize);

template<typename T>
std::vector<T> capWith(const T& cap, const std::vector<T>& values);

template<typename T>
std::vector<T> floorWith(const T& floor, const std::vector<T>& values);

template<typename T>
std::vector<T> slice(int from, int to, const std::vector<T>& vec);

// Numeric operations on vectors
template<typename T>
std::vector<T> diffNum(const std::vector<T>& values);

template<typename T>
std::vector<T> scaleByFstElement(T newFirst, const std::vector<T>& values);

std::vector<Rational> scaleUpToOne(const std::vector<Rational>& values);

// Find operations
template<typename T>
std::optional<T> lastOf(const std::vector<T>& vec, std::function<bool(const T&)> predicate);

// Box finding for ranges
using CutoffPair = std::pair<CutoffType, CutoffType>;
template<typename T>
std::optional<std::pair<T, T>> findBox(CutoffPair cutoffs, T value, const std::vector<std::pair<T, T>>& ranges);

// ============================================================================
// FINANCIAL CALCULATIONS
// ============================================================================

// Pro-rata calculations
std::vector<Amount> prorataFactors(const std::vector<Balance>& dueAmounts, Amount totalToPay);

// Payment allocation strategies
enum class PaymentStrategy {
    SEQUENTIAL,
    PRO_RATA
};

template<typename T>
struct PaymentResult {
    std::vector<T> paidObjects;
    Amount remainingAmount;
};

// Sequential payment allocation
template<typename T>
PaymentResult<T> paySequentially(Date paymentDate, Amount amount,
                                std::function<Balance(const T&)> getDueAmount,
                                std::function<T(Amount, const T&)> payFunction,
                                const std::vector<T>& objectsToPay);

// Pro-rata payment allocation  
template<typename T>
PaymentResult<T> payProRata(Date paymentDate, Amount amount,
                           std::function<Balance(const T&)> getDueAmount,
                           std::function<T(Amount, const T&)> payFunction,
                           const std::vector<T>& objectsToPay);

// Weighted balance calculations
Balance calcWeightBalanceByDates(DayCount dayCount, const std::vector<Balance>& balances, 
                                const std::vector<Date>& dates);

// Rate conversions
Rate toPeriodRateByInterval(Rate annualRate, int days);

// ============================================================================
// MAP OPERATIONS
// ============================================================================

// Safe map operations
template<typename K, typename V>
std::optional<V> lookupM(const K& key, const std::map<K, V>& map);

template<typename K, typename V>
std::vector<std::optional<V>> lookupVs(const std::vector<K>& keys, const std::map<K, V>& map);

// Map transformations
template<typename K, typename V>
std::map<K, V> mapWithinMap(std::function<V(const V&)> transform, 
                           const std::vector<K>& keys, 
                           const std::map<K, V>& map);

// Payment operations in maps
template<typename T>
std::map<std::string, T> payInMap(Date paymentDate, Balance amount,
                                 std::function<Balance(const T&)> getDueFunction,
                                 std::function<T(Balance, const T&)> payFunction,
                                 const std::vector<std::string>& objectNames,
                                 PaymentStrategy strategy,
                                 const std::map<std::string, T>& inputMap);

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

// Validation functions
bool testSumToOne(const std::vector<Rate>& rates, double tolerance = 1e-10);

// Split by lengths
template<typename T>
std::vector<std::vector<T>> splitByLengths(const std::vector<T>& values, const std::vector<int>& lengths);

// List to map conversion
template<typename T>
std::map<std::string, T> lstToMapByFn(std::function<std::string(const T&)> keyFunction, 
                                     const std::vector<T>& objects);

// Debug utilities
std::string debugOnDate(Date startDate, Date endDate, Date currentDate);
template<typename T>
std::string showLength(const std::vector<T>& vec);

// ============================================================================
// TEMPLATE IMPLEMENTATIONS (Header-only for performance)
// ============================================================================

template<typename T>
T safeDivide(T numerator, T denominator, DivisionBehavior behavior) {
    if (denominator == T{0}) {
        switch (behavior) {
            case DivisionBehavior::RETURN_INFINITY:
                return std::numeric_limits<T>::infinity();
            case DivisionBehavior::RETURN_ZERO:
                return T{0};
            case DivisionBehavior::RETURN_OPTIONAL:
                // This should be used with the optional version
                return T{0};
        }
    }
    return numerator / denominator;
}

template<typename T>
T roundingBy(T value, T precision, RoundingDirection direction) {
    if (precision == T{0}) return value;
    
    switch (direction) {
        case RoundingDirection::FLOOR:
            return precision * std::floor(value / precision);
        case RoundingDirection::CEIL:
            return precision * std::ceil(value / precision);
        case RoundingDirection::ROUND:
            return precision * std::round(value / precision);
    }
    return value;
}

template<typename T>
T maximum(const std::vector<T>& values) {
    if (values.empty()) {
        throw std::invalid_argument("Cannot find maximum of empty vector");
    }
    return *std::max_element(values.begin(), values.end());
}

template<typename T>
T minimum(const std::vector<T>& values) {
    if (values.empty()) {
        throw std::invalid_argument("Cannot find minimum of empty vector");
    }
    return *std::min_element(values.begin(), values.end());
}

template<typename T>
std::vector<T> lastN(int n, const std::vector<T>& vec) {
    if (n <= 0) return {};
    if (n >= static_cast<int>(vec.size())) return vec;
    return std::vector<T>(vec.end() - n, vec.end());
}

template<typename T>
std::vector<T> dropLastN(int n, const std::vector<T>& vec) {
    if (n <= 0) return vec;
    if (n >= static_cast<int>(vec.size())) return {};
    return std::vector<T>(vec.begin(), vec.end() - n);
}

template<typename T>
std::vector<T> replace(const std::vector<T>& vec, int index, const T& newValue) {
    if (index < 0 || index >= static_cast<int>(vec.size())) {
        throw std::out_of_range("Index out of range in replace");
    }
    std::vector<T> result = vec;
    result[index] = newValue;
    return result;
}

template<typename T>
std::vector<T> paddingDefault(const T& defaultValue, const std::vector<T>& vec, int targetSize) {
    if (targetSize <= 0) return {};
    if (static_cast<int>(vec.size()) >= targetSize) {
        return std::vector<T>(vec.begin(), vec.begin() + targetSize);
    }
    
    std::vector<T> result = vec;
    result.resize(targetSize, defaultValue);
    return result;
}

template<typename T>
std::vector<T> capWith(const T& cap, const std::vector<T>& values) {
    std::vector<T> result;
    result.reserve(values.size());
    for (const auto& value : values) {
        result.push_back(std::min(cap, value));
    }
    return result;
}

template<typename T>
std::vector<T> floorWith(const T& floor, const std::vector<T>& values) {
    std::vector<T> result;
    result.reserve(values.size());
    for (const auto& value : values) {
        result.push_back(std::max(value, floor));
    }
    return result;
}

template<typename T>
std::vector<T> slice(int from, int to, const std::vector<T>& vec) {
    if (from < 0) from = 0;
    if (to > static_cast<int>(vec.size())) to = static_cast<int>(vec.size());
    if (from >= to) return {};
    
    return std::vector<T>(vec.begin() + from, vec.begin() + to);
}

template<typename T>
std::vector<T> diffNum(const std::vector<T>& values) {
    if (values.size() < 2) return {};
    
    std::vector<T> result;
    result.reserve(values.size() - 1);
    for (size_t i = 1; i < values.size(); ++i) {
        result.push_back(values[i] - values[i - 1]);
    }
    return result;
}

template<typename T>
std::vector<T> scaleByFstElement(T newFirst, const std::vector<T>& values) {
    if (values.empty()) return {};
    if (values[0] == T{0}) return values; // Avoid division by zero
    
    T scaleFactor = newFirst / values[0];
    std::vector<T> result;
    result.reserve(values.size());
    
    for (const auto& value : values) {
        result.push_back(value * scaleFactor);
    }
    return result;
}

template<typename T, typename Predicate>
std::optional<T> lastOf(const std::vector<T>& vec, Predicate predicate) {
    for (auto it = vec.rbegin(); it != vec.rend(); ++it) {
        if (predicate(*it)) {
            return *it;
        }
    }
    return std::nullopt;
}

template<typename T>
std::optional<std::pair<T, T>> findBox(CutoffPair cutoffs, T value, const std::vector<std::pair<T, T>>& ranges) {
    auto [leftCutoff, rightCutoff] = cutoffs;
    
    for (const auto& [low, high] : ranges) {
        bool leftMatch = (leftCutoff == CutoffType::INCLUDE) ? (value >= low) : (value > low);
        bool rightMatch = (rightCutoff == CutoffType::INCLUDE) ? (value <= high) : (value < high);
        
        if (leftMatch && rightMatch) {
            return std::make_pair(low, high);
        }
    }
    return std::nullopt;
}

template<typename T, typename GetDueFunction, typename PayFunction>
PaymentResult<T> paySequentially(Date paymentDate, Amount amount,
                                GetDueFunction getDueFunction,
                                PayFunction payFunction,
                                const std::vector<T>& objectsToPay) {
    std::vector<T> paidObjects;
    paidObjects.reserve(objectsToPay.size());
    Amount remainingAmount = amount;
    
    for (const auto& obj : objectsToPay) {
        if (remainingAmount <= 0) {
            paidObjects.push_back(obj); // No payment, keep original
            continue;
        }
        
        Balance dueAmount = getDueFunction(obj);
        Amount actualPayment = std::min(remainingAmount, dueAmount);
        T paidObject = payFunction(actualPayment, obj);
        
        paidObjects.push_back(paidObject);
        remainingAmount -= actualPayment;
    }
    
    return PaymentResult<T>{std::move(paidObjects), remainingAmount};
}

template<typename T, typename GetDueFunction, typename PayFunction>
PaymentResult<T> payProRata(Date paymentDate, Amount amount,
                           GetDueFunction getDueFunction,
                           PayFunction payFunction,
                           const std::vector<T>& objectsToPay) {
    // Calculate due amounts
    std::vector<Balance> dueAmounts;
    dueAmounts.reserve(objectsToPay.size());
    for (const auto& obj : objectsToPay) {
        dueAmounts.push_back(getDueFunction(obj));
    }
    
    // Calculate pro-rata allocations
    std::vector<Amount> allocations = prorataFactors(dueAmounts, amount);
    
    // Apply payments
    std::vector<T> paidObjects;
    paidObjects.reserve(objectsToPay.size());
    Amount totalPaid = 0;
    
    for (size_t i = 0; i < objectsToPay.size(); ++i) {
        T paidObject = payFunction(allocations[i], objectsToPay[i]);
        paidObjects.push_back(paidObject);
        totalPaid += allocations[i];
    }
    
    return PaymentResult<T>{std::move(paidObjects), amount - totalPaid};
}

template<typename K, typename V>
std::optional<V> lookupM(const K& key, const std::map<K, V>& map) {
    auto it = map.find(key);
    return (it != map.end()) ? std::make_optional(it->second) : std::nullopt;
}

template<typename K, typename V>
std::vector<std::optional<V>> lookupVs(const std::vector<K>& keys, const std::map<K, V>& map) {
    std::vector<std::optional<V>> result;
    result.reserve(keys.size());
    for (const auto& key : keys) {
        result.push_back(lookupM(key, map));
    }
    return result;
}

template<typename K, typename V, typename Transform>
std::map<K, V> mapWithinMap(Transform transform,
                           const std::vector<K>& keys,
                           const std::map<K, V>& sourceMap) {
    std::map<K, V> result = sourceMap;
    for (const auto& key : keys) {
        auto it = result.find(key);
        if (it != result.end()) {
            it->second = transform(it->second);
        }
    }
    return result;
}

template<typename T>
std::map<std::string, T> payInMap(Date paymentDate, Balance amount,
                                 std::function<Balance(const T&)> getDueFunction,
                                 std::function<T(Balance, const T&)> payFunction,
                                 const std::vector<std::string>& objectNames,
                                 PaymentStrategy strategy,
                                 const std::map<std::string, T>& inputMap) {
    // Extract objects to pay
    std::vector<T> objectsToPay;
    objectsToPay.reserve(objectNames.size());
    for (const auto& name : objectNames) {
        auto it = inputMap.find(name);
        if (it != inputMap.end()) {
            objectsToPay.push_back(it->second);
        }
    }
    
    // Apply payment strategy
    PaymentResult<T> result;
    if (strategy == PaymentStrategy::SEQUENTIAL) {
        result = paySequentially(paymentDate, amount, getDueFunction, payFunction, objectsToPay);
    } else {
        result = payProRata(paymentDate, amount, getDueFunction, payFunction, objectsToPay);
    }
    
    // Create result map
    std::map<std::string, T> resultMap = inputMap;
    for (size_t i = 0; i < objectNames.size() && i < result.paidObjects.size(); ++i) {
        resultMap[objectNames[i]] = result.paidObjects[i];
    }
    
    return resultMap;
}

template<typename T>
std::vector<std::vector<T>> splitByLengths(const std::vector<T>& values, const std::vector<int>& lengths) {
    std::vector<std::vector<T>> result;
    result.reserve(lengths.size());
    
    size_t offset = 0;
    for (int length : lengths) {
        if (length <= 0) {
            result.push_back({});
            continue;
        }
        
        size_t endPos = std::min(offset + static_cast<size_t>(length), values.size());
        if (offset < values.size()) {
            result.emplace_back(values.begin() + offset, values.begin() + endPos);
        } else {
            result.push_back({});
        }
        offset = endPos;
    }
    
    return result;
}

template<typename T, typename KeyFunction>
std::map<std::string, T> lstToMapByFn(KeyFunction keyFunction,
                                     const std::vector<T>& objects) {
    std::map<std::string, T> result;
    for (const auto& obj : objects) {
        std::string key = keyFunction(obj);
        result[key] = obj;
    }
    return result;
}

template<typename T>
std::string showLength(const std::vector<T>& vec) {
    return "Length: " + std::to_string(vec.size());
}

} // namespace Utils
} // namespace Structura