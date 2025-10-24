#include "date_utils_enhanced.h"
#include <ql/time/calendars/nullcalendar.hpp>
#include <ql/time/businessdayconvention.hpp>
#include <ql/time/daycounters/actual360.hpp>
#include <ql/time/daycounters/actual365fixed.hpp>
#include <ql/time/daycounters/actualactual.hpp>
#include <ql/time/daycounters/thirty360.hpp>
#include <algorithm>
#include <stdexcept>
#include <map>

namespace Structura {
namespace DateUtils {

// Generate serial dates using QuantLib Schedule where possible
std::vector<Date> generateSerialDates(DatePattern pattern, CutoffType cutoff_type, 
                                     Date start_date, int num_dates) {
    std::vector<Date> result;
    
    // Generate the requested number of dates initially
    int dates_to_generate = num_dates;
    
    try {
        switch (pattern) {
            case DatePattern::MONTHLY: {
                QuantLib::Schedule schedule(
                    start_date,
                    start_date + QuantLib::Period(dates_to_generate - 1, QuantLib::Months),
                    QuantLib::Period(1, QuantLib::Months),
                    QuantLib::NullCalendar(),
                    QuantLib::Unadjusted,
                    QuantLib::Unadjusted,
                    QuantLib::DateGeneration::Forward,
                    false
                );
                result = schedule.dates();
                break;
            }
            case DatePattern::QUARTERLY: {
                QuantLib::Schedule schedule(
                    start_date,
                    start_date + QuantLib::Period((dates_to_generate - 1) * 3, QuantLib::Months),
                    QuantLib::Period(3, QuantLib::Months),
                    QuantLib::NullCalendar(),
                    QuantLib::Unadjusted,
                    QuantLib::Unadjusted,
                    QuantLib::DateGeneration::Forward,
                    false
                );
                result = schedule.dates();
                break;
            }
            case DatePattern::SEMI_ANNUALLY: {
                QuantLib::Schedule schedule(
                    start_date,
                    start_date + QuantLib::Period((dates_to_generate - 1) * 6, QuantLib::Months),
                    QuantLib::Period(6, QuantLib::Months),
                    QuantLib::NullCalendar(),
                    QuantLib::Unadjusted,
                    QuantLib::Unadjusted,
                    QuantLib::DateGeneration::Forward,
                    false
                );
                result = schedule.dates();
                break;
            }
            case DatePattern::ANNUALLY: {
                QuantLib::Schedule schedule(
                    start_date,
                    start_date + QuantLib::Period(dates_to_generate - 1, QuantLib::Years),
                    QuantLib::Period(1, QuantLib::Years),
                    QuantLib::NullCalendar(),
                    QuantLib::Unadjusted,
                    QuantLib::Unadjusted,
                    QuantLib::DateGeneration::Forward,
                    false
                );
                result = schedule.dates();
                break;
            }
            case DatePattern::MONTH_END:
            case DatePattern::QUARTER_END:
            case DatePattern::YEAR_END:
            case DatePattern::YEAR_FIRST:
            case DatePattern::MONTH_FIRST:
            case DatePattern::QUARTER_FIRST:
                result = Internal::generateCustomDates(pattern, start_date, dates_to_generate);
                break;
            default:
                throw std::invalid_argument("Unsupported date pattern");
        }
        
        // Apply cutoff logic if needed
        if (cutoff_type == CutoffType::EXCLUDE && !result.empty()) {
            result.erase(result.begin()); // Remove first date
            // After exclusion, we have one less date than requested - this is the expected behavior
        } else {
            // Ensure we have exactly the requested number for INCLUDE case
            if (result.size() > static_cast<size_t>(num_dates)) {
                result.resize(num_dates);
            }
        }
        
    } catch (const std::exception& e) {
        // Fallback to custom generation if QuantLib fails
        result = Internal::generateCustomDates(pattern, start_date, dates_to_generate);
        if (cutoff_type == CutoffType::EXCLUDE && !result.empty()) {
            result.erase(result.begin());
        } else if (result.size() > static_cast<size_t>(num_dates)) {
            result.resize(num_dates);
        }
    }
    
    return result;
}

// Generate dates until end date
std::vector<Date> generateDatesTill(Date start_date, DatePattern pattern, Date end_date) {
    std::vector<Date> result;
    
    try {
        QuantLib::Period period = Internal::datePatternToPeriod(pattern);
        
        QuantLib::Schedule schedule(
            start_date,
            end_date,
            period,
            QuantLib::NullCalendar(),
            QuantLib::Unadjusted,
            QuantLib::Unadjusted,
            QuantLib::DateGeneration::Forward,
            false
        );
        
        result = schedule.dates();
        
        // Filter to ensure dates are <= end_date
        result.erase(std::remove_if(result.begin(), result.end(),
            [end_date](const Date& d) { return d > end_date; }), result.end());
            
    } catch (const std::exception&) {
        // Fallback: generate step by step
        Date current = start_date;
        QuantLib::Period step = Internal::datePatternToPeriod(pattern);
        
        while (current <= end_date) {
            result.push_back(current);
            current = current + step;
        }
    }
    
    return result;
}

// Advanced date generation with range control
std::vector<Date> generateDatesTill2(RangeType range_type, Date start_date, 
                                    DatePattern pattern, Date end_date) {
    std::vector<Date> all_dates = generateDatesTill(start_date, pattern, end_date);
    return sliceDates(all_dates, start_date, end_date, range_type);
}

// Split dates by a specific date
std::pair<std::vector<Date>, std::vector<Date>> splitByDate(const std::vector<Date>& dates, 
                                                           Date split_date, CutoffType cutoff_type) {
    std::vector<Date> before, after;
    
    for (const Date& date : dates) {
        if (cutoff_type == CutoffType::INCLUDE) {
            if (date <= split_date) {
                before.push_back(date);
            } else {
                after.push_back(date);
            }
        } else { // EXCLUDE
            if (date < split_date) {
                before.push_back(date);
            } else if (date > split_date) {
                after.push_back(date);
            }
            // If date == split_date, it's excluded from both arrays
        }
    }
    
    return std::make_pair(before, after);
}

// Project dates by pattern
std::vector<Date> projectDatesByPattern(Date start_date, DatePattern pattern, int num_periods) {
    return generateSerialDates(pattern, CutoffType::INCLUDE, start_date, num_periods);
}

// Add months to date using QuantLib
Date monthsAfter(Date base_date, int months) {
    return base_date + QuantLib::Period(months, QuantLib::Months);
}

// Calculate interval factors between dates
std::vector<double> getIntervalFactors(const std::vector<Date>& dates) {
    std::vector<double> factors;
    if (dates.size() < 2) return factors;
    
    factors.reserve(dates.size() - 1);
    for (size_t i = 1; i < dates.size(); ++i) {
        int days = dates[i] - dates[i-1];
        factors.push_back(days / 365.0); // Simple year fraction
    }
    
    return factors;
}

// Days between dates
int daysBetween(Date start_date, Date end_date) {
    return end_date - start_date;
}

// Enhanced year count fraction using QuantLib DayCounters
double yearCountFraction(Structura::DayCount day_count, Date start_date, Date end_date) {
    QuantLib::DayCounter dc = Internal::dayCountToDayCounter(day_count);
    return dc.yearFraction(start_date, end_date);
}

// Slice dates by range
std::vector<Date> sliceDates(const std::vector<Date>& dates, Date start_date, 
                           Date end_date, RangeType range_type) {
    std::vector<Date> result;
    auto [include_start, include_end] = Internal::rangeTypeToFlags(range_type);
    
    for (const Date& date : dates) {
        bool after_start = include_start ? (date >= start_date) : (date > start_date);
        bool before_end = include_end ? (date <= end_date) : (date < end_date);
        
        if (after_start && before_end) {
            result.push_back(date);
        }
    }
    
    return result;
}

// Internal helper implementations
namespace Internal {

QuantLib::Period datePatternToPeriod(DatePattern pattern) {
    switch (pattern) {
        case DatePattern::MONTHLY:
            return QuantLib::Period(1, QuantLib::Months);
        case DatePattern::QUARTERLY:
            return QuantLib::Period(3, QuantLib::Months);
        case DatePattern::SEMI_ANNUALLY:
            return QuantLib::Period(6, QuantLib::Months);
        case DatePattern::ANNUALLY:
            return QuantLib::Period(1, QuantLib::Years);
        default:
            return QuantLib::Period(1, QuantLib::Months); // Default
    }
}

QuantLib::DateGeneration::Rule datePatternToRule(DatePattern pattern) {
    switch (pattern) {
        case DatePattern::MONTH_END:
        case DatePattern::QUARTER_END:
        case DatePattern::YEAR_END:
            return QuantLib::DateGeneration::Backward;
        case DatePattern::MONTH_FIRST:
        case DatePattern::QUARTER_FIRST:
        case DatePattern::YEAR_FIRST:
            return QuantLib::DateGeneration::Forward;
        default:
            return QuantLib::DateGeneration::Forward;
    }
}

QuantLib::DayCounter dayCountToDayCounter(Structura::DayCount day_count) {
    switch (day_count) {
        case DayCount::DC_ACT_360:
            return QuantLib::Actual360();
        case DayCount::DC_ACT_365:
        case DayCount::DC_ACT_365F:
            return QuantLib::Actual365Fixed();
        case DayCount::DC_ACT_ACT:
            return QuantLib::ActualActual(QuantLib::ActualActual::ISDA);
        case DayCount::DC_30E_360:
            return QuantLib::Thirty360(QuantLib::Thirty360::European);
        case DayCount::DC_30_360_US:
            return QuantLib::Thirty360(QuantLib::Thirty360::USA);
        case DayCount::DC_30_360_ISDA:
            return QuantLib::Thirty360(QuantLib::Thirty360::ISDA);
        default:
            return QuantLib::Actual365Fixed(); // Default fallback
    }
}

std::pair<bool, bool> rangeTypeToFlags(RangeType range_type) {
    switch (range_type) {
        case RangeType::INCLUSIVE_INCLUSIVE:
            return {true, true};
        case RangeType::INCLUSIVE_EXCLUSIVE:
            return {true, false};  
        case RangeType::EXCLUSIVE_INCLUSIVE:
            return {false, true};
        case RangeType::EXCLUSIVE_EXCLUSIVE:
            return {false, false};
        default:
            return {true, true};
    }
}

std::vector<Date> getMonthEnds(int year) {
    std::vector<Date> month_ends;
    for (int month = 1; month <= 12; ++month) {
        Date last_day = getLastDayOfMonth(year, month);
        month_ends.push_back(last_day);
    }
    return month_ends;
}

std::vector<Date> getQuarterEnds(int year) {
    return {
        Date(31, QuantLib::March, year),
        Date(30, QuantLib::June, year),
        Date(30, QuantLib::September, year),
        Date(31, QuantLib::December, year)
    };
}

bool isEndOfMonth(Date date) {
    int year = date.year();
    int month = date.month();
    Date last_day = getLastDayOfMonth(year, month);
    return date == last_day;
}

Date getLastDayOfMonth(int year, int month) {
    // Get first day of next month, then subtract 1 day
    if (month == 12) {
        return Date(31, QuantLib::December, year);
    } else {
        Date first_of_next = Date(1, QuantLib::Month(month + 1), year);
        return first_of_next - 1;
    }
}

std::vector<Date> generateCustomDates(DatePattern pattern, Date start_date, int num_dates, 
                                    int custom_month, int custom_day) {
    std::vector<Date> result;
    int start_year = start_date.year();
    
    switch (pattern) {
        case DatePattern::MONTH_END: {
            for (int i = 0; i < num_dates; ++i) {
                int target_year = start_year + (start_date.month() + i - 1) / 12;
                int target_month = ((start_date.month() + i - 1) % 12) + 1;
                result.push_back(getLastDayOfMonth(target_year, target_month));
            }
            break;
        }
        case DatePattern::QUARTER_END: {
            int start_quarter = (start_date.month() - 1) / 3;
            for (int i = 0; i < num_dates; ++i) {
                int quarter = (start_quarter + i) % 4;
                int year = start_year + (start_quarter + i) / 4;
                int month = (quarter + 1) * 3;  // Q1=3, Q2=6, Q3=9, Q4=12
                result.push_back(getLastDayOfMonth(year, month));
            }
            break;
        }
        case DatePattern::YEAR_END: {
            for (int i = 0; i < num_dates; ++i) {
                result.push_back(Date(31, QuantLib::December, start_year + i));
            }
            break;
        }
        case DatePattern::YEAR_FIRST: {
            for (int i = 0; i < num_dates; ++i) {
                result.push_back(Date(1, QuantLib::January, start_year + i));
            }
            break;
        }
        case DatePattern::MONTH_FIRST: {
            for (int i = 0; i < num_dates; ++i) {
                int target_year = start_year + (start_date.month() + i - 1) / 12;
                int target_month = ((start_date.month() + i - 1) % 12) + 1;
                result.push_back(Date(1, QuantLib::Month(target_month), target_year));
            }
            break;
        }
        case DatePattern::QUARTER_FIRST: {
            int start_quarter = (start_date.month() - 1) / 3;
            for (int i = 0; i < num_dates; ++i) {
                int quarter = (start_quarter + i) % 4;
                int year = start_year + (start_quarter + i) / 4;
                int month = quarter * 3 + 1;  // Q1=1, Q2=4, Q3=7, Q4=10
                result.push_back(Date(1, QuantLib::Month(month), year));
            }
            break;
        }
        default:
            throw std::invalid_argument("Unsupported custom date pattern");
    }
    
    return result;
}

} // namespace Internal

// String conversion utilities
std::string datePatternToString(DatePattern pattern) {
    static const std::map<DatePattern, std::string> pattern_map = {
        {DatePattern::MONTH_END, "MONTH_END"},
        {DatePattern::QUARTER_END, "QUARTER_END"},
        {DatePattern::YEAR_END, "YEAR_END"},
        {DatePattern::YEAR_FIRST, "YEAR_FIRST"},
        {DatePattern::MONTH_FIRST, "MONTH_FIRST"},
        {DatePattern::QUARTER_FIRST, "QUARTER_FIRST"},
        {DatePattern::MONTHLY, "MONTHLY"},
        {DatePattern::QUARTERLY, "QUARTERLY"},
        {DatePattern::SEMI_ANNUALLY, "SEMI_ANNUALLY"},
        {DatePattern::ANNUALLY, "ANNUALLY"},
        {DatePattern::CUSTOM_MONTH_DAY, "CUSTOM_MONTH_DAY"},
        {DatePattern::CUSTOM_DAY_OF_MONTH, "CUSTOM_DAY_OF_MONTH"}
    };
    
    auto it = pattern_map.find(pattern);
    return it != pattern_map.end() ? it->second : "UNKNOWN";
}

std::string rangeTypeToString(RangeType range_type) {
    static const std::map<RangeType, std::string> range_map = {
        {RangeType::INCLUSIVE_INCLUSIVE, "II"},
        {RangeType::INCLUSIVE_EXCLUSIVE, "IE"},
        {RangeType::EXCLUSIVE_INCLUSIVE, "EI"},
        {RangeType::EXCLUSIVE_EXCLUSIVE, "EE"}
    };
    
    auto it = range_map.find(range_type);
    return it != range_map.end() ? it->second : "UNKNOWN";
}

std::string cutoffTypeToString(CutoffType cutoff_type) {
    return (cutoff_type == CutoffType::INCLUDE) ? "INCLUDE" : "EXCLUDE";
}

DatePattern stringToDatePattern(const std::string& str) {
    static const std::map<std::string, DatePattern> string_map = {
        {"MONTH_END", DatePattern::MONTH_END},
        {"QUARTER_END", DatePattern::QUARTER_END},
        {"YEAR_END", DatePattern::YEAR_END},
        {"YEAR_FIRST", DatePattern::YEAR_FIRST},
        {"MONTH_FIRST", DatePattern::MONTH_FIRST},
        {"QUARTER_FIRST", DatePattern::QUARTER_FIRST},
        {"MONTHLY", DatePattern::MONTHLY},
        {"QUARTERLY", DatePattern::QUARTERLY},
        {"SEMI_ANNUALLY", DatePattern::SEMI_ANNUALLY},
        {"ANNUALLY", DatePattern::ANNUALLY},
        {"CUSTOM_MONTH_DAY", DatePattern::CUSTOM_MONTH_DAY},
        {"CUSTOM_DAY_OF_MONTH", DatePattern::CUSTOM_DAY_OF_MONTH}
    };
    
    auto it = string_map.find(str);
    if (it == string_map.end()) {
        throw std::invalid_argument("Unknown date pattern: " + str);
    }
    return it->second;
}

RangeType stringToRangeType(const std::string& str) {
    static const std::map<std::string, RangeType> string_map = {
        {"II", RangeType::INCLUSIVE_INCLUSIVE},
        {"IE", RangeType::INCLUSIVE_EXCLUSIVE},
        {"EI", RangeType::EXCLUSIVE_INCLUSIVE},
        {"EE", RangeType::EXCLUSIVE_EXCLUSIVE}
    };
    
    auto it = string_map.find(str);
    if (it == string_map.end()) {
        throw std::invalid_argument("Unknown range type: " + str);
    }
    return it->second;
}

CutoffType stringToCutoffType(const std::string& str) {
    if (str == "INCLUDE") return CutoffType::INCLUDE;
    if (str == "EXCLUDE") return CutoffType::EXCLUDE;
    throw std::invalid_argument("Unknown cutoff type: " + str);
}

} // namespace DateUtils
} // namespace Structura