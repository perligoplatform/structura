#pragma once

#include "../core/types.h"
#include <vector>
#include <optional>
#include <string>
#include <variant>
#include <cmath>

namespace Structura {

// Type aliases from Haskell InterestRate module
using Spread = double;  // Basis points as decimal (e.g., 0.01 for 100bp)
using InitPeriod = int;
using PeriodicCap = std::optional<Spread>;
using LifetimeCap = std::optional<Rate>;
using PaymentCap = std::optional<Balance>;
using RateFloor = std::optional<Rate>;
using RateCap = std::optional<Rate>;
using InitCap = std::optional<Rate>;
using ResetDates = std::vector<Date>;
using StartRate = Rate;

// Interest rate index types
enum class Index {
    LIBOR_USD_1M,
    LIBOR_USD_3M,
    LIBOR_USD_6M,
    LIBOR_USD_12M,
    LIBOR_EUR_1M,
    LIBOR_EUR_3M,
    LIBOR_EUR_6M,
    LIBOR_EUR_12M,
    EURIBOR_1M,
    EURIBOR_3M,
    EURIBOR_6M,
    EURIBOR_12M,
    SOFR,        // Secured Overnight Financing Rate
    SONIA,       // Sterling Overnight Index Average
    BBSW,        // Bank Bill Swap Rate (Australia)
    IRPH,        // Índice de Referencia de Préstamos Hipotecarios (Spain)
    PRIME,       // Prime Rate
    TREASURY_1Y,
    TREASURY_2Y,
    TREASURY_5Y,
    TREASURY_10Y,
    TREASURY_30Y
};

// Day count conventions for interest calculations
enum class DayCount {
    DC_30E_360,     // ISMA European 30S/360 Special German Eurobond Basis
    DC_30Ep_360,    // 30E+/360
    DC_ACT_360,     // Actual/360, French
    DC_ACT_365,     // Actual/365
    DC_ACT_365A,    // Actual/365 Actual
    DC_ACT_365L,    // Actual/365 Leap Year
    DC_NL_365,      // Actual/365 No leap year
    DC_ACT_365F,    // Actual/365 Fixed, English
    DC_ACT_ACT,     // Actual/Actual ISDA
    DC_30_360_ISDA, // ISDA
    DC_30_360_German, // German
    DC_30_360_US    // 30/360 US Municipal, Bond basis
};

// Date pattern for rate resets
enum class DatePattern {
    MONTHLY,
    QUARTERLY,
    SEMI_ANNUALLY,
    ANNUALLY,
    CUSTOM
};

// Rounding methods for interest rates
template<typename T>
struct RoundingBy {
    enum class Method {
        ROUND_CEIL,
        ROUND_FLOOR, 
        ROUND_NEAREST
    };
    
    Method method;
    T precision;
    
    RoundingBy(Method m, T p) : method(m), precision(p) {}
    
    T apply(T value) const {
        switch (method) {
            case Method::ROUND_CEIL:
                return std::ceil(value / precision) * precision;
            case Method::ROUND_FLOOR:
                return std::floor(value / precision) * precision;
            case Method::ROUND_NEAREST:
                return std::round(value / precision) * precision;
            default:
                return value;
        }
    }
};

// Main rate type - represents different interest rate structures
class RateType {
public:
    // Variant to handle different rate types
    struct FixedRate {
        DayCount day_count;
        Rate rate;
        
        FixedRate(DayCount dc, Rate r) : day_count(dc), rate(r) {}
    };
    
    struct FloatingRate {
        DayCount day_count;
        Index index;
        Spread spread;
        Rate initial_rate;
        DatePattern reset_pattern;
        RateFloor floor;
        RateCap cap;
        std::optional<RoundingBy<Rate>> rounding;
        
        FloatingRate(DayCount dc, Index idx, Spread spd, Rate init_rate, 
                    DatePattern pattern, RateFloor f = std::nullopt, 
                    RateCap c = std::nullopt, 
                    std::optional<RoundingBy<Rate>> round = std::nullopt)
            : day_count(dc), index(idx), spread(spd), initial_rate(init_rate),
              reset_pattern(pattern), floor(f), cap(c), rounding(round) {}
    };
    
private:
    std::variant<FixedRate, FloatingRate> rate_type_;
    
public:
    // Constructors
    RateType(const FixedRate& fixed) : rate_type_(fixed) {}
    RateType(const FloatingRate& floating) : rate_type_(floating) {}
    
    // Static factory methods
    static RateType createFixed(DayCount day_count, Rate rate) {
        return RateType(FixedRate(day_count, rate));
    }
    
    static RateType createFloating(DayCount day_count, Index index, Spread spread, 
                                  Rate initial_rate, DatePattern pattern,
                                  RateFloor floor = std::nullopt, RateCap cap = std::nullopt) {
        return RateType(FloatingRate(day_count, index, spread, initial_rate, pattern, floor, cap));
    }
    
    // Accessors
    bool isFixed() const { return std::holds_alternative<FixedRate>(rate_type_); }
    bool isFloating() const { return std::holds_alternative<FloatingRate>(rate_type_); }
    
    const FixedRate& getFixed() const { return std::get<FixedRate>(rate_type_); }
    const FloatingRate& getFloating() const { return std::get<FloatingRate>(rate_type_); }
    
    // Utility methods
    DayCount getDayCount() const {
        return std::visit([](const auto& rate) { return rate.day_count; }, rate_type_);
    }
    
    std::optional<Spread> getSpread() const {
        if (isFloating()) {
            return getFloating().spread;
        }
        return std::nullopt;
    }
};

// Adjustable Rate Mortgage (ARM) structure
class ARM {
public:
    enum class Type {
        STANDARD_ARM,
        OTHER_ARM
    };
    
private:
    Type type_;
    InitPeriod init_period_;
    InitCap init_cap_;
    PeriodicCap periodic_cap_;
    LifetimeCap lifetime_cap_;
    RateFloor floor_;
    
public:
    // Constructors
    ARM(InitPeriod init_period, InitCap init_cap, PeriodicCap periodic_cap, 
        LifetimeCap lifetime_cap, RateFloor floor)
        : type_(Type::STANDARD_ARM), init_period_(init_period), init_cap_(init_cap),
          periodic_cap_(periodic_cap), lifetime_cap_(lifetime_cap), floor_(floor) {}
    
    ARM() : type_(Type::OTHER_ARM), init_period_(0) {}
    
    // Accessors
    Type getType() const { return type_; }
    InitPeriod getInitPeriod() const { return init_period_; }
    InitCap getInitCap() const { return init_cap_; }
    PeriodicCap getPeriodicCap() const { return periodic_cap_; }
    LifetimeCap getLifetimeCap() const { return lifetime_cap_; }
    RateFloor getFloor() const { return floor_; }
    
    bool isStandardARM() const { return type_ == Type::STANDARD_ARM; }
};

// Interest rate calculation functions
namespace InterestCalculations {
    // Calculate year count fraction for different day count conventions
    double yearCountFraction(DayCount day_count, const Date& start_date, const Date& end_date);
    
    // Calculate interest rate for a period
    Rate calcIntRate(const Date& start_date, const Date& end_date, Rate interest_rate, DayCount day_count);
    
    // Calculate interest rates for multiple periods
    std::vector<Rate> calcIntRates(DayCount day_count, Rate rate, const std::vector<Date>& dates);
    
    // Calculate interest amount on balance
    Balance calcInt(Balance balance, const Date& start_date, const Date& end_date, 
                   Rate interest_rate, DayCount day_count);
    
    // Generate rate reset dates for floating rate instruments
    std::vector<Date> getRateResetDates(const Date& start_date, const Date& end_date, 
                                       const std::optional<RateType>& rate_type);
}

// UseRate interface for objects that use interest rates
template<typename T>
class UseRate {
public:
    virtual ~UseRate() = default;
    virtual bool isAdjustableRate() const = 0;
    virtual std::optional<Index> getIndex() const = 0;  
    virtual std::optional<std::vector<Index>> getIndexes() const = 0;
    virtual std::vector<Date> getResetDates() const = 0;
    virtual std::optional<Spread> getSpread() const = 0;
};

// String conversion utilities
std::string indexToString(Index index);
std::string dayCountToString(DayCount day_count);
std::string datePatternToString(DatePattern pattern);

Index stringToIndex(const std::string& str);
DayCount stringToDayCount(const std::string& str);
DatePattern stringToDatePattern(const std::string& str);

} // namespace Structura