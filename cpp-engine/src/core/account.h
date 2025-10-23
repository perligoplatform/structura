#pragma once

#include "financial_types.h"
#include "types.h"
#include <optional>
#include <vector>
#include <variant>

namespace Structura {

// Forward declarations
class DealStats;
class Statement;
class Pre;

// Interest Information for accounts
class InterestInfo {
public:
    enum class Type { BankAccount, InvestmentAccount };
    
private:
    Type type_;
    IRate rate_;
    Date lastAccrueDate_;
    
    // Bank account specific
    // (rate and lastAccrueDate already covered)
    
    // Investment account specific  
    Index index_;
    Spread spread_;
    IRate lastResetRate_;
    
public:
    // Factory methods for type-safe construction
    static InterestInfo createBankAccount(IRate rate, const Date& lastAccrueDate) {
        InterestInfo info;
        info.type_ = Type::BankAccount;
        info.rate_ = rate;
        info.lastAccrueDate_ = lastAccrueDate;
        return info;
    }
    
    static InterestInfo createInvestmentAccount(Index index, Spread spread, 
                                              const Date& lastAccrueDate, IRate lastResetRate) {
        InterestInfo info;
        info.type_ = Type::InvestmentAccount;
        info.index_ = index;
        info.spread_ = spread;
        info.lastAccrueDate_ = lastAccrueDate;
        info.lastResetRate_ = lastResetRate;
        return info;
    }
    
    // Accessors
    Type getType() const { return type_; }
    IRate getRate() const { return (type_ == Type::BankAccount) ? rate_ : lastResetRate_; }
    Date getLastAccrueDate() const { return lastAccrueDate_; }
    Index getIndex() const { return index_; }
    Spread getSpread() const { return spread_; }
    IRate getLastResetRate() const { return lastResetRate_; }
    
    // Setters
    void setLastAccrueDate(const Date& date) { lastAccrueDate_ = date; }
    void setLastResetRate(IRate rate) { lastResetRate_ = rate; }
};

// Reserve Amount specifications
class ReserveAmount {
public:
    enum class Type { PctReserve, FixReserve, Either, Max, Min };
    
private:
    Type type_;
    
    // PctReserve: dealStats, rate
    // FixReserve: fixedAmount
    Balance fixedAmount_;
    Rate percentage_;
    
    // Either: condition, first, second
    std::shared_ptr<Pre> condition_;
    std::shared_ptr<ReserveAmount> first_;
    std::shared_ptr<ReserveAmount> second_;
    
    // Max/Min: list of reserve amounts
    std::vector<std::shared_ptr<ReserveAmount>> reserves_;
    
public:
    // Factory methods
    static ReserveAmount createFixReserve(Balance amount) {
        ReserveAmount reserve;
        reserve.type_ = Type::FixReserve;
        reserve.fixedAmount_ = amount;
        return reserve;
    }
    
    static ReserveAmount createPctReserve(Rate percentage) {
        ReserveAmount reserve;
        reserve.type_ = Type::PctReserve;
        reserve.percentage_ = percentage;
        return reserve;
    }
    
    static ReserveAmount createMax(const std::vector<std::shared_ptr<ReserveAmount>>& reserves) {
        ReserveAmount reserve;
        reserve.type_ = Type::Max;
        reserve.reserves_ = reserves;
        return reserve;
    }
    
    static ReserveAmount createMin(const std::vector<std::shared_ptr<ReserveAmount>>& reserves) {
        ReserveAmount reserve;
        reserve.type_ = Type::Min;
        reserve.reserves_ = reserves;
        return reserve;
    }
    
    // Evaluation method (to be implemented with deal stats)
    Balance evaluate() const {
        switch (type_) {
            case Type::FixReserve:
                return fixedAmount_;
            case Type::PctReserve:
                // TODO: Implement with DealStats when available
                return 0.0;
            case Type::Max:
                {
                    Balance maxVal = 0.0;
                    for (const auto& reserve : reserves_) {
                        maxVal = std::max(maxVal, reserve->evaluate());
                    }
                    return maxVal;
                }
            case Type::Min:
                {
                    if (reserves_.empty()) return 0.0;
                    Balance minVal = reserves_[0]->evaluate();
                    for (const auto& reserve : reserves_) {
                        minVal = std::min(minVal, reserve->evaluate());
                    }
                    return minVal;
                }
            default:
                return 0.0;
        }
    }
    
    Type getType() const { return type_; }
    Balance getFixedAmount() const { return fixedAmount_; }
    Rate getPercentage() const { return percentage_; }
};

// Transaction for Account history
struct AccountTransaction {
    Date date;
    Balance newBalance;
    Amount amount;
    std::string comment;
    
    AccountTransaction(const Date& d, Balance newBal, Amount amt, const std::string& cmt)
        : date(d), newBalance(newBal), amount(amt), comment(cmt) {}
};

// Account class - enhanced version of existing account
class Account {
private:
    std::string name_;
    Balance balance_;
    std::optional<InterestInfo> interestInfo_;
    std::optional<ReserveAmount> reserveAmount_;
    std::vector<AccountTransaction> transactions_;
    
public:
    // Constructors
    Account(const std::string& name, Balance initialBalance = 0.0)
        : name_(name), balance_(initialBalance) {}
        
    Account(const std::string& name, Balance initialBalance, const ReserveAmount& reserve)
        : name_(name), balance_(initialBalance), reserveAmount_(reserve) {}
        
    Account(const std::string& name, Balance initialBalance, const InterestInfo& interest)
        : name_(name), balance_(initialBalance), interestInfo_(interest) {}
        
    Account(const std::string& name, Balance initialBalance, 
            const ReserveAmount& reserve, const InterestInfo& interest)
        : name_(name), balance_(initialBalance), reserveAmount_(reserve), interestInfo_(interest) {}
    
    // Basic accessors
    const std::string& getName() const { return name_; }
    Balance getBalance() const { return balance_; }
    const std::optional<InterestInfo>& getInterestInfo() const { return interestInfo_; }
    const std::optional<ReserveAmount>& getReserveAmount() const { return reserveAmount_; }
    const std::vector<AccountTransaction>& getTransactions() const { return transactions_; }
    
    // Balance operations
    void deposit(const Date& date, Amount amount, const std::string& comment = "") {
        balance_ += amount;
        transactions_.emplace_back(date, balance_, amount, 
                                 comment.empty() ? "DEPOSIT" : comment);
    }
    
    bool withdraw(const Date& date, Amount amount, const std::string& comment = "") {
        if (balance_ >= amount) {
            balance_ -= amount;
            transactions_.emplace_back(date, balance_, -amount, 
                                     comment.empty() ? "WITHDRAWAL" : comment);
            return true;
        }
        return false;
    }
    
    // Interest operations
    void setInterestInfo(const InterestInfo& info) {
        interestInfo_ = info;
    }
    
    Balance calculateAccruedInterest(const Date& endDate) const {
        if (!interestInfo_.has_value()) {
            return 0.0;
        }
        
        const auto& info = interestInfo_.value();
        Date lastAccrueDate = info.getLastAccrueDate();
        
        if (endDate <= lastAccrueDate) {
            return 0.0;
        }
        
        // Simple interest calculation for now
        // TODO: Implement proper day count conventions
        QuantLib::Natural days = endDate - lastAccrueDate;
        Rate annualRate = info.getRate();
        
        return balance_ * annualRate * days / 365.0;
    }
    
    void accrueInterest(const Date& date) {
        if (!interestInfo_.has_value()) {
            return;
        }
        
        Balance accruedAmount = calculateAccruedInterest(date);
        if (accruedAmount > 0.0) {
            deposit(date, accruedAmount, "ACCRUED_INTEREST");
            
            // Update last accrue date
            auto info = interestInfo_.value();
            info.setLastAccrueDate(date);
            interestInfo_ = info;
        }
    }
    
    // Reserve operations
    void setReserveAmount(const ReserveAmount& reserve) {
        reserveAmount_ = reserve;
    }
    
    Balance getTargetReserveAmount() const {
        if (reserveAmount_.has_value()) {
            return reserveAmount_.value().evaluate();
        }
        return 0.0;
    }
    
    Balance getReserveGap() const {
        Balance target = getTargetReserveAmount();
        return std::max(0.0, target - balance_);
    }
    
    Balance getReserveExcess() const {
        Balance target = getTargetReserveAmount();
        return std::max(0.0, balance_ - target);
    }
    
    // Transfer operations
    static bool transfer(Account& fromAccount, Account& toAccount, 
                        const Date& date, Amount amount, 
                        const std::string& comment = "") {
        if (fromAccount.getBalance() >= amount) {
            std::string transferComment = comment.empty() ? 
                ("TRANSFER_TO_" + toAccount.getName()) : comment;
            std::string receiveComment = comment.empty() ? 
                ("TRANSFER_FROM_" + fromAccount.getName()) : comment;
                
            fromAccount.withdraw(date, amount, transferComment);
            toAccount.deposit(date, amount, receiveComment);
            return true;
        }
        return false;
    }
    
    // Query transactions by comment pattern
    std::vector<AccountTransaction> queryTransactions(const std::string& commentPattern) const {
        std::vector<AccountTransaction> result;
        for (const auto& txn : transactions_) {
            if (txn.comment.find(commentPattern) != std::string::npos) {
                result.push_back(txn);
            }
        }
        return result;
    }
    
    // Get transactions in date range
    std::vector<AccountTransaction> getTransactionsInRange(const Date& startDate, 
                                                          const Date& endDate) const {
        std::vector<AccountTransaction> result;
        for (const auto& txn : transactions_) {
            if (txn.date >= startDate && txn.date <= endDate) {
                result.push_back(txn);
            }
        }
        return result;
    }
    
    // Statistics
    Balance getAverageBalance(const Date& startDate, const Date& endDate) const {
        auto rangeTxns = getTransactionsInRange(startDate, endDate);
        if (rangeTxns.empty()) {
            return balance_;
        }
        
        // Simple average for now - could be improved with proper time-weighting
        Balance sum = 0.0;
        for (const auto& txn : rangeTxns) {
            sum += txn.newBalance;
        }
        return sum / rangeTxns.size();
    }
};

} // namespace Structura