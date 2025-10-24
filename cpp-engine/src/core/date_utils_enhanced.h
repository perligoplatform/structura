#pragma once

#include "../core/types.h"
#include "../core/financial_types.h"  // For DayCount enum
#include <ql/time/date.hpp>
#include <ql/time/schedule.hpp>
#include <ql/time/daycounters/all.hpp>
#include <ql/time/calendars/nullcalendar.hpp>
#include <ql/time/period.hpp>
#include <ql/time/dategenerationrule.hpp>
#include <vector>
#include <optional>

namespace Structura {

// Enhanced date utilities that leverage QuantLib's robust date handling
namespace DateUtils {

    using Date = QuantLib::Date;

    // Date pattern enumeration matching Haskell DatePattern
    enum class DatePattern {
        MONTH_END,
        QUARTER_END, 
        YEAR_END,
        YEAR_FIRST,
        MONTH_FIRST,
        QUARTER_FIRST,
        MONTHLY,
        QUARTERLY,
        SEMI_ANNUALLY,
        ANNUALLY,
        CUSTOM_MONTH_DAY,
        CUSTOM_DAY_OF_MONTH
    };

    // Range type for date filtering
    enum class RangeType {
        INCLUSIVE_INCLUSIVE,  // II - include both start and end
        INCLUSIVE_EXCLUSIVE,  // IE - include start, exclude end  
        EXCLUSIVE_INCLUSIVE,  // EI - exclude start, include end
        EXCLUSIVE_EXCLUSIVE   // EE - exclude both start and end
    };

    // Cutoff type for date operations
    enum class CutoffType {
        INCLUDE,    // Inc
        EXCLUDE     // Exc
    };

    // Date generation using QuantLib Schedule
    std::vector<Date> generateSerialDates(DatePattern pattern, CutoffType cutoff_type, 
                                         Date start_date, int num_dates);

    // Generate dates until end date using QuantLib
    std::vector<Date> generateDatesTill(Date start_date, DatePattern pattern, Date end_date);
    
    // Advanced date generation with range control
    std::vector<Date> generateDatesTill2(RangeType range_type, Date start_date, 
                                        DatePattern pattern, Date end_date);

    // Split date vector by a specific date
    std::pair<std::vector<Date>, std::vector<Date>> splitByDate(const std::vector<Date>& dates, 
                                                               Date split_date, CutoffType cutoff_type);

    // Project dates by pattern using QuantLib Period
    std::vector<Date> projectDatesByPattern(Date start_date, DatePattern pattern, int num_periods);

    // Add months to a date (leverages QuantLib Period)
    Date monthsAfter(Date base_date, int months);

    // Calculate interval factors between dates  
    std::vector<double> getIntervalFactors(const std::vector<Date>& dates);

    // Days between dates (simple wrapper around QuantLib)
    int daysBetween(Date start_date, Date end_date);

    // Enhanced year count fraction using QuantLib DayCounters
    double yearCountFraction(Structura::DayCount day_count, Date start_date, Date end_date);

    // Slice dates by range
    std::vector<Date> sliceDates(const std::vector<Date>& dates, Date start_date, 
                               Date end_date, RangeType range_type);

    // Helper functions for QuantLib integration
    namespace Internal {
        // Convert our DatePattern to QuantLib Period
        QuantLib::Period datePatternToPeriod(DatePattern pattern);
        
        // Convert our DatePattern to QuantLib DateGeneration::Rule where applicable
        QuantLib::DateGeneration::Rule datePatternToRule(DatePattern pattern);
        
        // Convert our DayCount to QuantLib DayCounter
        QuantLib::DayCounter dayCountToDayCounter(Structura::DayCount day_count);
        
        // Convert our RangeType to inclusion flags
        std::pair<bool, bool> rangeTypeToFlags(RangeType range_type);
        
        // Generate month-end dates for a year
        std::vector<Date> getMonthEnds(int year);
        
        // Generate quarter-end dates  
        std::vector<Date> getQuarterEnds(int year);
        
        // Check if date is end of month
        bool isEndOfMonth(Date date);
        
        // Get last day of month for given year/month
        Date getLastDayOfMonth(int year, int month);
        
        // Generate dates for custom patterns
        std::vector<Date> generateCustomDates(DatePattern pattern, Date start_date, int num_dates, 
                                            int custom_month = 0, int custom_day = 0);
    }

    // String conversion utilities
    std::string datePatternToString(DatePattern pattern);
    std::string rangeTypeToString(RangeType range_type);
    std::string cutoffTypeToString(CutoffType cutoff_type);
    
    DatePattern stringToDatePattern(const std::string& str);
    RangeType stringToRangeType(const std::string& str);
    CutoffType stringToCutoffType(const std::string& str);

} // namespace DateUtils

} // namespace Structura