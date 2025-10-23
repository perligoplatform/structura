#include "core/types.h"
#include <sstream>
#include <iomanip>

namespace Structura {

// QuantLib DateUtils implementation - clean, no conditionals
namespace DateUtils {
    Date makeDate(int year, int month, int day) {
        return QuantLib::Date(day, static_cast<QuantLib::Month>(month), year);
    }
    
    std::string toString(const Date& date) {
        std::ostringstream oss;
        oss << std::setfill('0') << std::setw(4) << date.year() << "-"
            << std::setw(2) << static_cast<int>(date.month()) << "-"
            << std::setw(2) << date.dayOfMonth();
        return oss.str();
    }
    
    int year(const Date& date) {
        return date.year();
    }
    
    int month(const Date& date) {
        return static_cast<int>(date.month());
    }
    
    int dayOfMonth(const Date& date) {
        return date.dayOfMonth();
    }
}

} // namespace Structura
