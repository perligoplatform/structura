#include "../core/enhanced_utils.h"
#include <cmath>
#include <numeric>
#include <algorithm>

using namespace QuantLib;
using namespace Structura;
using CutoffType = Structura::DateUtils::CutoffType;
#include <stdexcept>
#include <sstream>

namespace Structura {
namespace Utils {

// ============================================================================
// MATHEMATICAL OPERATIONS
// ============================================================================

Balance mulBR(Balance balance, Rate rate) {
    return balance * rate;
}

Balance mulBIR(Balance balance, IRate rate) {
    return balance * rate;
}

Amount mulBI(Balance balance, IRate rate) {
    return balance * rate;
}

Rational mulIR(int multiplier, Rational rate) {
    return Rational(multiplier) * rate;
}

Balance divideBI(Balance balance, int divisor) {
    if (divisor == 0) {
        throw std::invalid_argument("Division by zero in divideBI");
    }
    return balance / divisor;
}

Rational divideBB(Balance numerator, Balance denominator) {
    if (denominator == 0.0) {
        return std::numeric_limits<double>::infinity();
    }
    return numerator / denominator;
}

std::optional<Rational> safeDiv(Rational numerator, Rational denominator) {
    if (denominator == 0.0) {
        return std::nullopt;
    }
    return numerator / denominator;
}

std::pair<Balance, Balance> splitBal(Rate rate, Balance balance) {
    Balance first = mulBR(balance, rate);
    Balance second = mulBR(balance, 1.0 - rate);
    return std::make_pair(first, second);
}

std::optional<Rational> roundingByM(std::optional<Rational> precision, Rational value, RoundingDirection direction) {
    if (!precision.has_value()) {
        return value; // No rounding if precision not specified
    }
    return roundingBy(value, *precision, direction);
}

// ============================================================================
// TIME SERIES OPERATIONS
// ============================================================================

Rational getValByDate(const TimeSeriesVariant& ts, DateUtils::CutoffType cutoff, Date date) {
    return std::visit([cutoff, date](const auto& curve) -> Rational {
        using CurveType = std::decay_t<decltype(curve)>;
        
        if (curve.points.empty()) {
            return 0.0;
        }
        
        // Find the appropriate value based on cutoff type
        // This implements the Haskell logic: find in reverse order
        auto reversed_points = curve.points;
        std::reverse(reversed_points.begin(), reversed_points.end());
        
        for (const auto& point : reversed_points) {
            bool condition = false;
            if (cutoff == DateUtils::CutoffType::INCLUDE) {
                condition = (date >= point.date);  // Inc logic: d >= _d
            } else {
                condition = (date > point.date);   // Exc logic: d > _d  
            }
            
            if (condition) {
                if constexpr (std::is_same_v<CurveType, FloatCurve> || 
                             std::is_same_v<CurveType, RatioCurve>) {
                    return point.value;
                } else if constexpr (std::is_same_v<CurveType, BalanceCurve>) {
                    return static_cast<Rational>(point.value);
                } else if constexpr (std::is_same_v<CurveType, IRateCurve>) {
                    return static_cast<Rational>(point.value);
                }
            }
        }
        
        return 0.0; // Not found
    }, ts);
}

std::vector<Rational> getValByDates(const TimeSeriesVariant& ts, DateUtils::CutoffType cutoff, const std::vector<Date>& dates) {
    std::vector<Rational> result;
    result.reserve(dates.size());
    for (const auto& date : dates) {
        result.push_back(getValByDate(ts, cutoff, date));
    }
    return result;
}

std::vector<Rational> getTsVals(const TimeSeriesVariant& ts) {
    return std::visit([](const auto& curve) -> std::vector<Rational> {
        std::vector<Rational> result;
        result.reserve(curve.points.size());
        
        for (const auto& point : curve.points) {
            if constexpr (std::is_same_v<std::decay_t<decltype(curve)>, FloatCurve> ||
                         std::is_same_v<std::decay_t<decltype(curve)>, RatioCurve>) {
                result.push_back(point.value);
            } else {
                result.push_back(static_cast<Rational>(point.value));
            }
        }
        
        return result;
    }, ts);
}

std::vector<Date> getTsDates(const TimeSeriesVariant& ts) {
    return std::visit([](const auto& curve) -> std::vector<Date> {
        std::vector<Date> result;
        result.reserve(curve.points.size());
        
        for (const auto& point : curve.points) {
            result.push_back(point.date);
        }
        
        return result;
    }, ts);
}

size_t getTsSize(const TimeSeriesVariant& ts) {
    return std::visit([](const auto& curve) -> size_t {
        return curve.points.size();
    }, ts);
}

TimeSeriesVariant zipTs(const std::vector<Date>& dates, const std::vector<Rational>& values) {
    if (dates.size() != values.size()) {
        throw std::invalid_argument("zipTs: dates and values must have the same size");
    }
    
    std::vector<TsPoint<Rational>> points;
    points.reserve(dates.size());
    
    for (size_t i = 0; i < dates.size(); ++i) {
        points.emplace_back(dates[i], values[i]);
    }
    
    return TimeSeriesVariant(FloatCurve(std::move(points)));
}

TimeSeriesVariant zipBalTs(const std::vector<Date>& dates, const std::vector<Balance>& values) {
    if (dates.size() != values.size()) {
        throw std::invalid_argument("zipBalTs: dates and values must have the same size");
    }
    
    std::vector<TsPoint<Balance>> points;
    points.reserve(dates.size());
    
    for (size_t i = 0; i < dates.size(); ++i) {
        points.emplace_back(dates[i], values[i]);
    }
    
    return TimeSeriesVariant(BalanceCurve(std::move(points)));
}

TimeSeriesVariant multiplyTs(DateUtils::CutoffType cutoff, const TimeSeriesVariant& ts1, const TimeSeriesVariant& ts2) {
    // Extract dates from first time series
    auto dates = getTsDates(ts1);
    std::vector<Rational> multipliedValues;
    multipliedValues.reserve(dates.size());
    
    // For each date in ts1, get value from ts1 and corresponding value from ts2
    for (const auto& date : dates) {
        Rational val1 = getValByDate(ts1, cutoff, date);
        Rational val2 = getValByDate(ts2, cutoff, date);
        multipliedValues.push_back(val1 * val2);
    }
    
    return zipTs(dates, multipliedValues);
}

TimeSeriesVariant shiftTsByAmt(const TimeSeriesVariant& ts, Rational delta) {
    return std::visit([delta](const auto& curve) -> TimeSeriesVariant {
        using CurveType = std::decay_t<decltype(curve)>;
        
        if constexpr (std::is_same_v<CurveType, FloatCurve>) {
            std::vector<TsPoint<Rational>> newPoints;
            newPoints.reserve(curve.points.size());
            
            for (const auto& point : curve.points) {
                newPoints.emplace_back(point.date, point.value + delta);
            }
            
            return TimeSeriesVariant(FloatCurve(std::move(newPoints)));
        } else if constexpr (std::is_same_v<CurveType, IRateCurve>) {
            std::vector<TsPoint<Rate>> newPoints;
            newPoints.reserve(curve.points.size());
            
            for (const auto& point : curve.points) {
                newPoints.emplace_back(point.date, point.value + static_cast<Rate>(delta));
            }
            
            return TimeSeriesVariant(IRateCurve(std::move(newPoints)));
        } else {
            // For other curve types, return unchanged for now
            return TimeSeriesVariant(curve);
        }
    }, ts);
}// ============================================================================
// FINANCIAL CALCULATIONS
// ============================================================================

std::vector<Amount> prorataFactors(const std::vector<Balance>& dueAmounts, Amount totalToPay) {
    Balance totalDue = std::accumulate(dueAmounts.begin(), dueAmounts.end(), Balance{0});
    
    if (totalDue == 0.0) {
        return std::vector<Amount>(dueAmounts.size(), 0.0);
    }
    
    Amount actualToPay = std::min(totalToPay, totalDue);
    std::vector<Amount> result;
    result.reserve(dueAmounts.size());
    
    for (const auto& due : dueAmounts) {
        Amount allocation = (due / totalDue) * actualToPay;
        result.push_back(allocation);
    }
    
    return result;
}

Balance calcWeightBalanceByDates(DayCount dayCount, const std::vector<Balance>& balances, 
                                const std::vector<Date>& dates) {
    if (balances.size() + 1 != dates.size()) {
        throw std::invalid_argument("calcWeightBalanceByDates: balance count should be one less than date count");
    }
    
    // Get interval factors using our DateUtil functions
    std::vector<double> weights = DateUtils::getIntervalFactors(dates);
    
    if (weights.size() != balances.size()) {
        throw std::invalid_argument("calcWeightBalanceByDates: weights and balances size mismatch");
    }
    
    Balance weightedSum = 0.0;
    for (size_t i = 0; i < balances.size(); ++i) {
        weightedSum += balances[i] * weights[i];
    }
    
    return weightedSum;
}

Rate toPeriodRateByInterval(Rate annualRate, int days) {
    if (days <= 0) {
        throw std::invalid_argument("Days must be positive in toPeriodRateByInterval");
    }
    
    double dayFraction = static_cast<double>(days) / 365.0;
    double periodRate = 1.0 - std::pow(1.0 - annualRate, dayFraction);
    return periodRate;
}

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

bool testSumToOne(const std::vector<Rate>& rates, double tolerance) {
    Rate sum = std::accumulate(rates.begin(), rates.end(), Rate{0});
    return std::abs(sum - 1.0) <= tolerance;
}

std::vector<Rational> scaleUpToOne(const std::vector<Rational>& values) {
    Rational sum = std::accumulate(values.begin(), values.end(), Rational{0});
    
    if (sum == 0.0) {
        return values; // Avoid division by zero
    }
    
    Rational scaleFactor = 1.0 / sum;
    std::vector<Rational> result;
    result.reserve(values.size());
    
    for (const auto& value : values) {
        result.push_back(value * scaleFactor);
    }
    
    return result;
}

std::string debugOnDate(Date startDate, Date endDate, Date currentDate) {
    using namespace QuantLib;
    if (currentDate >= startDate && currentDate <= endDate) {
        std::ostringstream oss;
        oss << "Date: " << currentDate;
        return oss.str();
    }
    return "";
}

} // namespace Utils
} // namespace Structura