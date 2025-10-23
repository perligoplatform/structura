#include "core/types.h"

namespace Structura {

Structura::Date parseDate(const std::string& date_str) {
    // Parse "YYYY-MM-DD" format
    int year = std::stoi(date_str.substr(0, 4));
    int month = std::stoi(date_str.substr(5, 2));
    int day = std::stoi(date_str.substr(8, 2));
    return Structura::DateUtils::makeDate(year, month, day);
}

std::string formatDate(const Structura::Date& date) {
    return Structura::DateUtils::toString(date);
}

} // namespace Structura
