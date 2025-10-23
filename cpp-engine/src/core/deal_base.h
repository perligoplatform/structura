#ifndef STRUCTURA_DEAL_BASE_H
#define STRUCTURA_DEAL_BASE_H

#include "types.h"
#include "account.h"
#include "../assets/pool.h"
#include <string>
#include <map>
#include <vector>
#include <memory>

namespace Structura {

/**
 * @brief Information about a structured finance deal
 */
struct DealInfo {
    std::string dealName;
    std::string dealType;  // "MBS", "ABS", "CLO", etc.
    std::string trustee;
    std::string underwriter;
    Date issueDate;
    Date maturityDate;
    Date firstPaymentDate;
    std::string currency;
    std::string description;
    
    DealInfo(const std::string& name, const std::string& type, 
             const Date& issue, const Date& maturity)
        : dealName(name), dealType(type), issueDate(issue), maturityDate(maturity),
          currency("USD") {}
};

/**
 * @brief Abstract base class for all structured finance deals
 * 
 * Provides common functionality shared by all deal types including:
 * - Deal information management
 * - Account management 
 * - Date scheduling
 * - Basic validation
 * - Status tracking
 */
class DealBase {
public:
    enum class DealStatus {
        Pending,      // Deal created but not yet active
        Active,       // Deal is actively paying
        Defaulted,    // Deal has defaulted
        Accelerated,  // Deal has been accelerated
        Matured,      // Deal has reached final maturity
        Called        // Deal has been called/redeemed early
    };

protected:
    DealInfo dealInfo_;
    std::map<std::string, std::unique_ptr<Account>> accounts_;
    std::vector<Date> paymentDates_;
    DealStatus status_;
    Date currentDate_;

public:
    /**
     * @brief Construct a new Deal Base object
     */
    DealBase(const DealInfo& info);
    
    /**
     * @brief Virtual destructor for proper inheritance
     */
    virtual ~DealBase() = default;

    // Deal Information
    const DealInfo& getDealInfo() const { return dealInfo_; }
    const std::string& getDealName() const { return dealInfo_.dealName; }
    const std::string& getDealType() const { return dealInfo_.dealType; }
    DealStatus getStatus() const { return status_; }
    Date getCurrentDate() const { return currentDate_; }
    
    // Account Management
    void addAccount(const std::string& name, std::unique_ptr<Account> account);
    Account* getAccount(const std::string& name);
    const Account* getAccount(const std::string& name) const;
    std::vector<std::string> getAccountNames() const;
    Balance getTotalAccountBalance() const;
    
    // Date Management
    void setPaymentDates(const std::vector<Date>& dates);
    const std::vector<Date>& getPaymentDates() const { return paymentDates_; }
    std::vector<Date> getPaymentDatesInRange(const Date& start, const Date& end) const;
    Date getNextPaymentDate(const Date& asOfDate) const;
    
    // Status Management
    void setStatus(DealStatus status) { status_ = status; }
    void setCurrentDate(const Date& date) { currentDate_ = date; }
    
    // Pure virtual methods that must be implemented by derived classes
    virtual void runWaterfall(const Date& paymentDate) = 0;
    virtual Balance calculateDealValue(const Date& valuationDate) const = 0;
    virtual std::vector<std::string> validate() const = 0;
    virtual std::string getDealSummary() const = 0;
    
    // Virtual methods with default implementations that can be overridden
    virtual void initialize();
    virtual void processPaymentDate(const Date& paymentDate);
    virtual bool isMatured() const;
    virtual bool canAccelerate() const;
    virtual void accelerate();
    
    // Utility methods
    static std::string statusToString(DealStatus status);
    static DealStatus stringToStatus(const std::string& statusStr);

protected:
    // Helper methods for derived classes
    void transferBetweenAccounts(const std::string& fromAccount, 
                               const std::string& toAccount, 
                               Balance amount);
    Balance getAvailableAccountBalance(const std::string& accountName) const;
    void accrueInterestOnAllAccounts(const Date& accrualDate);
    std::vector<std::string> validateBasicDealStructure() const;
};

} // namespace Structura

#endif // STRUCTURA_DEAL_BASE_H