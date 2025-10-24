#include "interest_rate.h"
#include <cmath>
#include <stdexcept>
#include <map>
#include <algorithm>

namespace Structura {

// Year count fraction calculations
double InterestCalculations::yearCountFraction(DayCount day_count, const Date& start_date, const Date& end_date) {
    QuantLib::Integer days = end_date - start_date;
    
    switch (day_count) {
        case DayCount::DC_30E_360:
        case DayCount::DC_30Ep_360: {
            // 30/360 European
            int start_day = std::min(start_date.dayOfMonth(), 30);
            int end_day = std::min(end_date.dayOfMonth(), 30);
            int months = (end_date.year() - start_date.year()) * 12 + 
                        (end_date.month() - start_date.month());
            int day_diff = end_day - start_day;
            return (months * 30 + day_diff) / 360.0;
        }
        case DayCount::DC_ACT_360:
            return days / 360.0;
        case DayCount::DC_ACT_365:
        case DayCount::DC_ACT_365F:
            return days / 365.0;
        case DayCount::DC_ACT_365A: {
            // Actual/365 Actual - considers leap years
            int year1 = start_date.year();
            int year2 = end_date.year();
            if (year1 == year2) {
                bool is_leap = QuantLib::Date::isLeap(year1);
                return days / (is_leap ? 366.0 : 365.0);
            } else {
                // More complex calculation for multi-year periods
                double total_days = 0.0;
                Date current = start_date;
                while (current.year() < year2) {
                    Date year_end(31, QuantLib::December, current.year());
                    if (year_end > end_date) year_end = end_date;
                    
                    bool is_leap = QuantLib::Date::isLeap(current.year());
                    double year_days = is_leap ? 366.0 : 365.0;
                    total_days += (year_end - current) / year_days;
                    
                    current = Date(1, QuantLib::January, current.year() + 1);
                }
                if (current < end_date) {
                    bool is_leap = QuantLib::Date::isLeap(year2);
                    double year_days = is_leap ? 366.0 : 365.0;
                    total_days += (end_date - current) / year_days;
                }
                return total_days;
            }
        }
        case DayCount::DC_ACT_365L: {
            // Actual/365 Leap Year
            bool has_leap = false;
            for (int year = start_date.year(); year <= end_date.year(); ++year) {
                if (QuantLib::Date::isLeap(year)) {
                    has_leap = true;
                    break;
                }
            }
            return days / (has_leap ? 366.0 : 365.0);
        }
        case DayCount::DC_NL_365:
            // No leap year - always 365
            return days / 365.0;
        case DayCount::DC_ACT_ACT: {
            // Actual/Actual ISDA
            // This is a simplified version - full ISDA implementation is more complex
            return yearCountFraction(DayCount::DC_ACT_365A, start_date, end_date);
        }
        case DayCount::DC_30_360_ISDA:
        case DayCount::DC_30_360_German:
        case DayCount::DC_30_360_US: {
            // 30/360 US Municipal, Bond basis
            int y1 = start_date.year(), y2 = end_date.year();
            int m1 = start_date.month(), m2 = end_date.month();
            int d1 = start_date.dayOfMonth(), d2 = end_date.dayOfMonth();
            
            // Apply 30/360 rules
            if (d1 == 31) d1 = 30;
            if (d2 == 31 && d1 >= 30) d2 = 30;
            
            return ((y2 - y1) * 360 + (m2 - m1) * 30 + (d2 - d1)) / 360.0;
        }
        default:
            throw std::invalid_argument("Unsupported day count convention");
    }
}

// Calculate interest rate for a period
Rate InterestCalculations::calcIntRate(const Date& start_date, const Date& end_date, 
                                      Rate interest_rate, DayCount day_count) {
    double year_fraction = yearCountFraction(day_count, start_date, end_date);
    return interest_rate * year_fraction;
}

// Calculate interest rates for multiple periods
std::vector<Rate> InterestCalculations::calcIntRates(DayCount day_count, Rate rate, 
                                                     const std::vector<Date>& dates) {
    std::vector<Rate> rates;
    if (dates.size() < 2) return rates;
    
    rates.reserve(dates.size() - 1);
    for (size_t i = 0; i < dates.size() - 1; ++i) {
        Rate period_rate = calcIntRate(dates[i], dates[i + 1], rate, day_count);
        rates.push_back(period_rate);
    }
    return rates;
}

// Calculate interest amount on balance
Balance InterestCalculations::calcInt(Balance balance, const Date& start_date, const Date& end_date,
                                     Rate interest_rate, DayCount day_count) {
    double year_fraction = yearCountFraction(day_count, start_date, end_date);
    return balance * (year_fraction * interest_rate);
}

// Generate rate reset dates for floating rate instruments
std::vector<Date> InterestCalculations::getRateResetDates(const Date& start_date, const Date& end_date,
                                                         const std::optional<RateType>& rate_type) {
    std::vector<Date> reset_dates;
    
    if (!rate_type.has_value() || rate_type->isFixed()) {
        return reset_dates; // No reset dates for fixed rate or null
    }
    
    const auto& floating = rate_type->getFloating();
    Date current_date = start_date;
    
    while (current_date < end_date) {
        reset_dates.push_back(current_date);
        
        // Calculate next reset date based on pattern
        switch (floating.reset_pattern) {
            case DatePattern::MONTHLY:
                current_date = current_date + QuantLib::Period(1, QuantLib::Months);
                break;
            case DatePattern::QUARTERLY:
                current_date = current_date + QuantLib::Period(3, QuantLib::Months);
                break;
            case DatePattern::SEMI_ANNUALLY:
                current_date = current_date + QuantLib::Period(6, QuantLib::Months);
                break;
            case DatePattern::ANNUALLY:
                current_date = current_date + QuantLib::Period(1, QuantLib::Years);
                break;
            case DatePattern::CUSTOM:
                // For custom patterns, we'd need additional configuration
                // For now, default to quarterly
                current_date = current_date + QuantLib::Period(3, QuantLib::Months);
                break;
        }
    }
    
    return reset_dates;
}

// String conversion utilities
std::string indexToString(Index index) {
    static const std::map<Index, std::string> index_map = {
        {Index::LIBOR_USD_1M, "LIBOR_USD_1M"},
        {Index::LIBOR_USD_3M, "LIBOR_USD_3M"},
        {Index::LIBOR_USD_6M, "LIBOR_USD_6M"},
        {Index::LIBOR_USD_12M, "LIBOR_USD_12M"},
        {Index::LIBOR_EUR_1M, "LIBOR_EUR_1M"},
        {Index::LIBOR_EUR_3M, "LIBOR_EUR_3M"},
        {Index::LIBOR_EUR_6M, "LIBOR_EUR_6M"},
        {Index::LIBOR_EUR_12M, "LIBOR_EUR_12M"},
        {Index::EURIBOR_1M, "EURIBOR_1M"},
        {Index::EURIBOR_3M, "EURIBOR_3M"},
        {Index::EURIBOR_6M, "EURIBOR_6M"},
        {Index::EURIBOR_12M, "EURIBOR_12M"},
        {Index::SOFR, "SOFR"},
        {Index::SONIA, "SONIA"},
        {Index::BBSW, "BBSW"},
        {Index::IRPH, "IRPH"},
        {Index::PRIME, "PRIME"},
        {Index::TREASURY_1Y, "TREASURY_1Y"},
        {Index::TREASURY_2Y, "TREASURY_2Y"},
        {Index::TREASURY_5Y, "TREASURY_5Y"},
        {Index::TREASURY_10Y, "TREASURY_10Y"},
        {Index::TREASURY_30Y, "TREASURY_30Y"}
    };
    
    auto it = index_map.find(index);
    return it != index_map.end() ? it->second : "UNKNOWN";
}

std::string dayCountToString(DayCount day_count) {
    static const std::map<DayCount, std::string> dc_map = {
        {DayCount::DC_30E_360, "30E/360"},
        {DayCount::DC_30Ep_360, "30E+/360"},
        {DayCount::DC_ACT_360, "ACT/360"},
        {DayCount::DC_ACT_365, "ACT/365"},
        {DayCount::DC_ACT_365A, "ACT/365A"},
        {DayCount::DC_ACT_365L, "ACT/365L"},
        {DayCount::DC_NL_365, "NL/365"},
        {DayCount::DC_ACT_365F, "ACT/365F"},
        {DayCount::DC_ACT_ACT, "ACT/ACT"},
        {DayCount::DC_30_360_ISDA, "30/360_ISDA"},
        {DayCount::DC_30_360_German, "30/360_German"},
        {DayCount::DC_30_360_US, "30/360_US"}
    };
    
    auto it = dc_map.find(day_count);
    return it != dc_map.end() ? it->second : "UNKNOWN";
}

std::string datePatternToString(DatePattern pattern) {
    static const std::map<DatePattern, std::string> pattern_map = {
        {DatePattern::MONTHLY, "MONTHLY"},
        {DatePattern::QUARTERLY, "QUARTERLY"},
        {DatePattern::SEMI_ANNUALLY, "SEMI_ANNUALLY"},
        {DatePattern::ANNUALLY, "ANNUALLY"},
        {DatePattern::CUSTOM, "CUSTOM"}
    };
    
    auto it = pattern_map.find(pattern);
    return it != pattern_map.end() ? it->second : "UNKNOWN";
}

Index stringToIndex(const std::string& str) {
    static const std::map<std::string, Index> string_map = {
        {"LIBOR_USD_1M", Index::LIBOR_USD_1M},
        {"LIBOR_USD_3M", Index::LIBOR_USD_3M},
        {"LIBOR_USD_6M", Index::LIBOR_USD_6M},
        {"LIBOR_USD_12M", Index::LIBOR_USD_12M},
        {"LIBOR_EUR_1M", Index::LIBOR_EUR_1M},
        {"LIBOR_EUR_3M", Index::LIBOR_EUR_3M},
        {"LIBOR_EUR_6M", Index::LIBOR_EUR_6M},
        {"LIBOR_EUR_12M", Index::LIBOR_EUR_12M},
        {"EURIBOR_1M", Index::EURIBOR_1M},
        {"EURIBOR_3M", Index::EURIBOR_3M},
        {"EURIBOR_6M", Index::EURIBOR_6M},
        {"EURIBOR_12M", Index::EURIBOR_12M},
        {"SOFR", Index::SOFR},
        {"SONIA", Index::SONIA},
        {"BBSW", Index::BBSW},
        {"IRPH", Index::IRPH},
        {"PRIME", Index::PRIME},
        {"TREASURY_1Y", Index::TREASURY_1Y},
        {"TREASURY_2Y", Index::TREASURY_2Y},
        {"TREASURY_5Y", Index::TREASURY_5Y},
        {"TREASURY_10Y", Index::TREASURY_10Y},
        {"TREASURY_30Y", Index::TREASURY_30Y}
    };
    
    auto it = string_map.find(str);
    if (it == string_map.end()) {
        throw std::invalid_argument("Unknown index: " + str);
    }
    return it->second;
}

DayCount stringToDayCount(const std::string& str) {
    static const std::map<std::string, DayCount> string_map = {
        {"30E/360", DayCount::DC_30E_360},
        {"30E+/360", DayCount::DC_30Ep_360},
        {"ACT/360", DayCount::DC_ACT_360},
        {"ACT/365", DayCount::DC_ACT_365},
        {"ACT/365A", DayCount::DC_ACT_365A},
        {"ACT/365L", DayCount::DC_ACT_365L},
        {"NL/365", DayCount::DC_NL_365},
        {"ACT/365F", DayCount::DC_ACT_365F},
        {"ACT/ACT", DayCount::DC_ACT_ACT},
        {"30/360_ISDA", DayCount::DC_30_360_ISDA},
        {"30/360_German", DayCount::DC_30_360_German},
        {"30/360_US", DayCount::DC_30_360_US}
    };
    
    auto it = string_map.find(str);
    if (it == string_map.end()) {
        throw std::invalid_argument("Unknown day count: " + str);
    }
    return it->second;
}

DatePattern stringToDatePattern(const std::string& str) {
    static const std::map<std::string, DatePattern> string_map = {
        {"MONTHLY", DatePattern::MONTHLY},
        {"QUARTERLY", DatePattern::QUARTERLY},
        {"SEMI_ANNUALLY", DatePattern::SEMI_ANNUALLY},
        {"ANNUALLY", DatePattern::ANNUALLY},
        {"CUSTOM", DatePattern::CUSTOM}
    };
    
    auto it = string_map.find(str);
    if (it == string_map.end()) {
        throw std::invalid_argument("Unknown date pattern: " + str);
    }
    return it->second;
}

} // namespace Structura