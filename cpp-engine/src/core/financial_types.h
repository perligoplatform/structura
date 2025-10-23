#pragma once

#include <string>
#include <vector>
#include <memory>
#include <map>
#include <chrono>
#include <sstream>
#include <ql/quantlib.hpp>

namespace Structura {

// Basic Type Aliases from Haskell
using BondName = std::string;
using BondNames = std::vector<std::string>;
using FeeName = std::string;
using FeeNames = std::vector<std::string>;
using AccName = std::string;
using AccountName = std::string;
using AccNames = std::vector<std::string>;
using CeName = std::string;
using Comment = std::string;
using DealName = std::string;

// Numeric Types
using Balance = QuantLib::Real;
using Amount = Balance;
using Principal = Balance;
using Valuation = Balance;
using Interest = Balance;
using Default = Balance;
using Loss = Balance;
using Cash = Balance;
using Recovery = Balance;
using Prepayment = Balance;
using Rental = Balance;
using PrepaymentPenalty = Balance;

using Rate = QuantLib::Real;
using PrepaymentRate = Rate;
using DefaultRate = Rate;
using RecoveryRate = Rate;
using IRate = QuantLib::Real;
using Spread = QuantLib::Real;
using Floor = QuantLib::Real;
using Cap = QuantLib::Real;

using RemainTerms = int;
using BorrowerNum = int;
using Lag = int;

// Date Types - using QuantLib dates
using Date = QuantLib::Date;
using Dates = std::vector<QuantLib::Date>;
using StartDate = Date;
using EndDate = Date;

// Comparison operators
enum class Cmp {
    G,   // Greater than
    GE,  // Greater Equal than
    L,   // Less than
    LE,  // Less Equal than
    E    // Equals to
};

// Day Count conventions
enum class DayCount {
    DC_30E_360,      // ISMA European 30S/360 Special German
    DC_30Ep_360,     // 30E+/360
    DC_ACT_360,      // Actual/360, French
    DC_ACT_365,      // Actual/365
    DC_ACT_365A,     // Actual/365 Actual
    DC_ACT_365L,     // Actual/365 Leap Year
    DC_NL_365,       // Actual/365 No leap year
    DC_ACT_365F,     // Actual/365 Fixed, English
    DC_ACT_ACT,      // Actual/Actual ISDA
    DC_30_360_ISDA,  // ISDA
    DC_30_360_German,// German
    DC_30_360_US     // 30/360 US Municipal, Bond basis
};

// Pool identification
class PoolId {
private:
    enum class Type { PoolName, PoolConsol, DealBondFlow };
    Type type_;
    std::string poolName_;
    std::string dealName_;
    std::string bondName_;
    Date startDate_;
    Rate rate_;

public:
    // Factory methods
    static PoolId createPoolName(const std::string& name) {
        PoolId id;
        id.type_ = Type::PoolName;
        id.poolName_ = name;
        return id;
    }
    
    static PoolId createPoolConsol() {
        PoolId id;
        id.type_ = Type::PoolConsol;
        return id;
    }
    
    static PoolId createDealBondFlow(const std::string& dealName, 
                                   const std::string& bondName,
                                   const Date& startDate,
                                   Rate rate) {
        PoolId id;
        id.type_ = Type::DealBondFlow;
        id.dealName_ = dealName;
        id.bondName_ = bondName;
        id.startDate_ = startDate;
        id.rate_ = rate;
        return id;
    }
    
    std::string toString() const {
        switch (type_) {
            case Type::PoolName:
                return poolName_;
            case Type::PoolConsol:
                return "PoolConsol";
            case Type::DealBondFlow: {
                std::ostringstream oss;
                oss << startDate_;
                return "BondFlow:" + dealName_ + ":" + bondName_ + ":" + 
                       oss.str() + ":" + 
                       std::to_string(rate_);
            }
        }
        return "";
    }
    
    bool operator==(const PoolId& other) const {
        if (type_ != other.type_) return false;
        switch (type_) {
            case Type::PoolName:
                return poolName_ == other.poolName_;
            case Type::PoolConsol:
                return true;
            case Type::DealBondFlow:
                return dealName_ == other.dealName_ && 
                       bondName_ == other.bondName_ &&
                       startDate_ == other.startDate_ &&
                       rate_ == other.rate_;
        }
        return false;
    }
    
    bool operator<(const PoolId& other) const {
        if (type_ != other.type_) return type_ < other.type_;
        switch (type_) {
            case Type::PoolName:
                return poolName_ < other.poolName_;
            case Type::PoolConsol:
                return false;
            case Type::DealBondFlow:
                if (dealName_ != other.dealName_) return dealName_ < other.dealName_;
                if (bondName_ != other.bondName_) return bondName_ < other.bondName_;
                if (startDate_ != other.startDate_) return startDate_ < other.startDate_;
                return rate_ < other.rate_;
        }
        return false;
    }
};

// Interest rate indices
enum class Index {
    LPR5Y, LPR1Y, LIBOR1M, LIBOR3M, LIBOR6M, LIBOR1Y,
    USTSY1Y, USTSY2Y, USTSY3Y, USTSY5Y, USTSY7Y, USTSY10Y, USTSY20Y, USTSY30Y,
    USCMT1Y, PRIME, COFI, SOFR1M, SOFR3M, SOFR6M, SOFR1Y,
    EURIBOR1M, EURIBOR3M, EURIBOR6M, EURIBOR12M,
    BBSW, IRPH, SONIA
};

// Floater is a pair of index and spread
using Floater = std::pair<Index, Spread>;

// Period enumeration  
enum class Period {
    Daily, Weekly, BiWeekly, Monthly, Quarterly, SemiAnnually, Annually
};

// Payment frequency - alias for Period for payment contexts
using PaymentFrequency = Period;

// Pool sources for cashflow collection
enum class PoolSource {
    CollectedInterest,
    CollectedPrincipal,
    CollectedRecoveries,
    CollectedPrepayment,
    CollectedPrepaymentPenalty,
    CollectedRental,
    CollectedFeePaid,
    CollectedCash,
    NewDefaults,
    NewLosses,
    NewDelinquencies,
    CurBalance,
    CurBegBalance
};

// Utility functions for string conversions
inline std::string toString(Cmp cmp) {
    switch (cmp) {
        case Cmp::G: return ">";
        case Cmp::GE: return ">=";
        case Cmp::L: return "<";
        case Cmp::LE: return "<=";
        case Cmp::E: return "==";
    }
    return "";
}

inline std::string toString(DayCount dc) {
    switch (dc) {
        case DayCount::DC_30E_360: return "DC_30E_360";
        case DayCount::DC_30Ep_360: return "DC_30Ep_360";
        case DayCount::DC_ACT_360: return "DC_ACT_360";
        case DayCount::DC_ACT_365: return "DC_ACT_365";
        case DayCount::DC_ACT_365A: return "DC_ACT_365A";
        case DayCount::DC_ACT_365L: return "DC_ACT_365L";
        case DayCount::DC_NL_365: return "DC_NL_365";
        case DayCount::DC_ACT_365F: return "DC_ACT_365F";
        case DayCount::DC_ACT_ACT: return "DC_ACT_ACT";
        case DayCount::DC_30_360_ISDA: return "DC_30_360_ISDA";
        case DayCount::DC_30_360_German: return "DC_30_360_German";
        case DayCount::DC_30_360_US: return "DC_30_360_US";
    }
    return "";
}

inline std::string toString(Index idx) {
    switch (idx) {
        case Index::LPR5Y: return "LPR5Y";
        case Index::LPR1Y: return "LPR1Y";
        case Index::LIBOR1M: return "LIBOR1M";
        case Index::LIBOR3M: return "LIBOR3M";
        case Index::LIBOR6M: return "LIBOR6M";
        case Index::LIBOR1Y: return "LIBOR1Y";
        case Index::USTSY1Y: return "USTSY1Y";
        case Index::USTSY2Y: return "USTSY2Y";
        case Index::USTSY3Y: return "USTSY3Y";
        case Index::USTSY5Y: return "USTSY5Y";
        case Index::USTSY7Y: return "USTSY7Y";
        case Index::USTSY10Y: return "USTSY10Y";
        case Index::USTSY20Y: return "USTSY20Y";
        case Index::USTSY30Y: return "USTSY30Y";
        case Index::USCMT1Y: return "USCMT1Y";
        case Index::PRIME: return "PRIME";
        case Index::COFI: return "COFI";
        case Index::SOFR1M: return "SOFR1M";
        case Index::SOFR3M: return "SOFR3M";
        case Index::SOFR6M: return "SOFR6M";
        case Index::SOFR1Y: return "SOFR1Y";
        case Index::EURIBOR1M: return "EURIBOR1M";
        case Index::EURIBOR3M: return "EURIBOR3M";
        case Index::EURIBOR6M: return "EURIBOR6M";
        case Index::EURIBOR12M: return "EURIBOR12M";
        case Index::BBSW: return "BBSW";
        case Index::IRPH: return "IRPH";
        case Index::SONIA: return "SONIA";
    }
    return "";
}

} // namespace Structura