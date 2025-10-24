#pragma once

#include "../core/types.h"
#include <vector>
#include <optional>
#include <variant>

namespace Structura {

// Type aliases matching Haskell equivalents
using Delinquent = Balance;
using Amounts = std::vector<double>;
using Principals = std::vector<Balance>;
using Interests = std::vector<Balance>;
using Prepayments = std::vector<Balance>;
using Recoveries = std::vector<Balance>;
using Rates = std::vector<Rate>;

// Cumulative statistics tuple
struct CumulativeStat {
    Balance cum_principal;
    Balance cum_prepay;
    Balance cum_delinq;
    Balance cum_default;
    Balance cum_recovery;
    Balance cum_loss;
    
    CumulativeStat() = default;
    CumulativeStat(Balance principal, Balance prepay, Balance delinq,
                   Balance default_amt, Balance recovery, Balance loss)
        : cum_principal(principal), cum_prepay(prepay), cum_delinq(delinq),
          cum_default(default_amt), cum_recovery(recovery), cum_loss(loss) {}
    
    // Addition operator for combining statistics
    CumulativeStat operator+(const CumulativeStat& other) const {
        return CumulativeStat(
            cum_principal + other.cum_principal,
            cum_prepay + other.cum_prepay,
            cum_delinq + other.cum_delinq,
            cum_default + other.cum_default,
            cum_recovery + other.cum_recovery,
            cum_loss + other.cum_loss
        );
    }
    
    // Subtraction operator
    CumulativeStat operator-(const CumulativeStat& other) const {
        return CumulativeStat(
            cum_principal - other.cum_principal,
            cum_prepay - other.cum_prepay,
            cum_delinq - other.cum_delinq,
            cum_default - other.cum_default,
            cum_recovery - other.cum_recovery,
            cum_loss - other.cum_loss
        );
    }
    
    // Scale by ratio
    CumulativeStat scale(double ratio) const {
        return CumulativeStat(
            cum_principal * ratio,
            cum_prepay * ratio,
            cum_delinq * ratio,
            cum_default * ratio,
            cum_recovery * ratio,
            cum_loss * ratio
        );
    }
};

// Cash flow row types - representing different types of cash flows
struct CashFlow {
    Date date;
    Balance amount;
    
    CashFlow(Date d, Balance a) : date(d), amount(a) {}
};

struct BondFlow {
    Date date;
    Balance balance;
    Balance principal;
    Balance interest;
    
    BondFlow(Date d, Balance b, Balance p, Balance i)
        : date(d), balance(b), principal(p), interest(i) {}
};

struct MortgageFlow {
    Date date;
    Balance balance;
    Balance principal;
    Balance interest;
    Balance prepayment;
    Balance default_amount;
    Balance recovery;
    Balance loss;
    Rate interest_rate;
    std::optional<int> borrower_num;
    std::optional<Balance> prepayment_penalty;
    std::optional<CumulativeStat> cumulative_stats;
    
    MortgageFlow(Date d, Balance b, Balance p, Balance i, Balance prep,
                 Balance def, Balance rec, Balance l, Rate rate)
        : date(d), balance(b), principal(p), interest(i), prepayment(prep),
          default_amount(def), recovery(rec), loss(l), interest_rate(rate) {}
};

struct MortgageDelinqFlow {
    Date date;
    Balance balance;
    Balance principal;
    Balance interest;
    Balance prepayment;
    Balance delinquent;
    Balance default_amount;
    Balance recovery;
    Balance loss;
    Rate interest_rate;
    std::optional<int> borrower_num;
    std::optional<Balance> prepayment_penalty;
    std::optional<CumulativeStat> cumulative_stats;
    
    MortgageDelinqFlow(Date d, Balance b, Balance p, Balance i, Balance prep,
                       Balance delinq, Balance def, Balance rec, Balance l, Rate rate)
        : date(d), balance(b), principal(p), interest(i), prepayment(prep),
          delinquent(delinq), default_amount(def), recovery(rec), loss(l), interest_rate(rate) {}
};

struct LoanFlow {
    Date date;
    Balance balance;
    Balance principal;
    Balance interest;
    Balance prepayment;
    Balance default_amount;
    Balance recovery;
    Balance loss;
    Rate interest_rate;
    std::optional<CumulativeStat> cumulative_stats;
    
    LoanFlow(Date d, Balance b, Balance p, Balance i, Balance prep,
             Balance def, Balance rec, Balance l, Rate rate)
        : date(d), balance(b), principal(p), interest(i), prepayment(prep),
          default_amount(def), recovery(rec), loss(l), interest_rate(rate) {}
};

struct LeaseFlow {
    Date date;
    Balance balance;
    Balance rental;
    Balance default_amount;
    
    LeaseFlow(Date d, Balance b, Balance r, Balance def)
        : date(d), balance(b), rental(r), default_amount(def) {}
};

struct FixedFlow {
    Date date;
    Balance balance;
    Balance new_depreciation;
    Balance depreciation;
    Balance cash_flow;
    Balance asset_value;
    
    FixedFlow(Date d, Balance b, Balance new_dep, Balance dep, Balance cash, Balance value)
        : date(d), balance(b), new_depreciation(new_dep), depreciation(dep),
          cash_flow(cash), asset_value(value) {}
};

struct ReceivableFlow {
    Date date;
    Balance balance;
    Balance accrued_fee;
    Balance principal;
    Balance fee_paid;
    Balance default_amount;
    Balance recovery;
    Balance loss;
    std::optional<CumulativeStat> cumulative_stats;
    
    ReceivableFlow(Date d, Balance b, Balance af, Balance p, Balance fp,
                   Balance def, Balance rec, Balance l)
        : date(d), balance(b), accrued_fee(af), principal(p), fee_paid(fp),
          default_amount(def), recovery(rec), loss(l) {}
};

// Variant type to hold different cash flow types
using TsRow = std::variant<CashFlow, BondFlow, MortgageFlow, MortgageDelinqFlow,
                          LoanFlow, LeaseFlow, FixedFlow, ReceivableFlow>;

// Begin status for cash flow frame
struct BeginStatus {
    Balance begin_balance;
    Date begin_date;
    std::optional<Balance> accrued_interest;
    
    BeginStatus(Balance balance, Date date, std::optional<Balance> interest = std::nullopt)
        : begin_balance(balance), begin_date(date), accrued_interest(interest) {}
};

// Main cash flow frame structure
class CashFlowFrame {
private:
    BeginStatus begin_status_;
    std::vector<TsRow> ts_rows_;

public:
    // Constructors
    CashFlowFrame() : begin_status_(0.0, Date(1, QuantLib::Month::January, 1901)) {}
    CashFlowFrame(BeginStatus status, std::vector<TsRow> rows)
        : begin_status_(status), ts_rows_(std::move(rows)) {}
    
    // Accessors
    const BeginStatus& getBeginStatus() const { return begin_status_; }
    const std::vector<TsRow>& getTsRows() const { return ts_rows_; }
    
    void setBeginStatus(const BeginStatus& status) { begin_status_ = status; }
    void addTsRow(const TsRow& row) { ts_rows_.push_back(row); }
    
    // Size and emptiness checks
    size_t size() const { return ts_rows_.size(); }
    bool empty() const { return ts_rows_.empty(); }
    
    // Combine two cash flow frames
    CashFlowFrame combine(const CashFlowFrame& other) const;
    
    // Get all dates from the cash flow frame
    std::vector<Date> getDates() const;
    
    // Static factory methods
    static CashFlowFrame createEmpty() {
        return CashFlowFrame(BeginStatus(0.0, Date(1, QuantLib::Month::January, 1901)), {});
    }
};

// Type aliases for different cash flow contexts
using AssetCashflow = CashFlowFrame;
using PoolCashflow = std::pair<AssetCashflow, std::optional<std::vector<AssetCashflow>>>;

// Utility functions for getting dates from TsRow variants
Date getDate(const TsRow& row);

// Utility functions for combining statistics
std::optional<CumulativeStat> sumStats(const std::optional<CumulativeStat>& s1,
                                      const std::optional<CumulativeStat>& s2);

std::optional<CumulativeStat> subStats(const std::optional<CumulativeStat>& s1,
                                      const std::optional<CumulativeStat>& s2);

std::optional<CumulativeStat> maxStats(const std::optional<CumulativeStat>& s1,
                                      const std::optional<CumulativeStat>& s2);

} // namespace Structura