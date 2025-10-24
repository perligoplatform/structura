#include "cashflow.h"
#include <algorithm>
#include <functional>

namespace Structura {

// Get date from TsRow variant
Date getDate(const TsRow& row) {
    return std::visit([](const auto& flow) -> Date {
        return flow.date;
    }, row);
}

// Combine two cash flow frames
CashFlowFrame CashFlowFrame::combine(const CashFlowFrame& other) const {
    // Use the begin status from the first frame (this)
    BeginStatus combined_status = begin_status_;
    
    // Combine the transaction rows
    std::vector<TsRow> combined_rows = ts_rows_;
    combined_rows.insert(combined_rows.end(), other.ts_rows_.begin(), other.ts_rows_.end());
    
    // Sort by date
    std::sort(combined_rows.begin(), combined_rows.end(), 
              [](const TsRow& a, const TsRow& b) {
                  return getDate(a) < getDate(b);
              });
    
    return CashFlowFrame(combined_status, std::move(combined_rows));
}

// Get all dates from the cash flow frame
std::vector<Date> CashFlowFrame::getDates() const {
    std::vector<Date> dates;
    dates.reserve(ts_rows_.size());
    
    for (const auto& row : ts_rows_) {
        dates.push_back(getDate(row));
    }
    
    return dates;
}

// Utility functions for combining statistics
std::optional<CumulativeStat> sumStats(const std::optional<CumulativeStat>& s1,
                                      const std::optional<CumulativeStat>& s2) {
    if (!s1 && !s2) return std::nullopt;
    if (s1 && !s2) return s1;
    if (!s1 && s2) return s2;
    return *s1 + *s2;
}

std::optional<CumulativeStat> subStats(const std::optional<CumulativeStat>& s1,
                                      const std::optional<CumulativeStat>& s2) {
    if (!s1 && !s2) return std::nullopt;
    if (s1 && !s2) return s1;
    if (!s1 && s2) {
        // Return negative of s2
        CumulativeStat zero_stat{};
        return zero_stat - *s2;
    }
    return *s1 - *s2;
}

std::optional<CumulativeStat> maxStats(const std::optional<CumulativeStat>& s1,
                                      const std::optional<CumulativeStat>& s2) {
    if (!s1 && !s2) return std::nullopt;
    if (s1 && !s2) return s1;
    if (!s1 && s2) return s2;
    
    // Take max of each component
    return CumulativeStat(
        std::max(s1->cum_principal, s2->cum_principal),
        std::max(s1->cum_prepay, s2->cum_prepay),
        std::max(s1->cum_delinq, s2->cum_delinq),
        std::max(s1->cum_default, s2->cum_default),
        std::max(s1->cum_recovery, s2->cum_recovery),
        std::max(s1->cum_loss, s2->cum_loss)
    );
}

} // namespace Structura