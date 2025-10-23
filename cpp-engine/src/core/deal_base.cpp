#include "deal_base.h"
#include <algorithm>
#include <numeric>
#include <stdexcept>

namespace Structura {

DealBase::DealBase(const DealInfo& info) 
    : dealInfo_(info), status_(DealStatus::Pending), currentDate_(info.issueDate) {
}

void DealBase::addAccount(const std::string& name, std::unique_ptr<Account> account) {
    if (!account) {
        throw std::invalid_argument("Cannot add null account");
    }
    accounts_[name] = std::move(account);
}

Account* DealBase::getAccount(const std::string& name) {
    auto it = accounts_.find(name);
    return (it != accounts_.end()) ? it->second.get() : nullptr;
}

const Account* DealBase::getAccount(const std::string& name) const {
    auto it = accounts_.find(name);
    return (it != accounts_.end()) ? it->second.get() : nullptr;
}

std::vector<std::string> DealBase::getAccountNames() const {
    std::vector<std::string> names;
    names.reserve(accounts_.size());
    for (const auto& pair : accounts_) {
        names.push_back(pair.first);
    }
    return names;
}

Balance DealBase::getTotalAccountBalance() const {
    Balance total = 0.0;
    for (const auto& pair : accounts_) {
        total += pair.second->getBalance();
    }
    return total;
}

void DealBase::setPaymentDates(const std::vector<Date>& dates) {
    paymentDates_ = dates;
    // Ensure dates are sorted
    std::sort(paymentDates_.begin(), paymentDates_.end());
}

std::vector<Date> DealBase::getPaymentDatesInRange(const Date& start, const Date& end) const {
    std::vector<Date> datesInRange;
    for (const auto& date : paymentDates_) {
        if (date >= start && date <= end) {
            datesInRange.push_back(date);
        }
    }
    return datesInRange;
}

Date DealBase::getNextPaymentDate(const Date& asOfDate) const {
    auto it = std::upper_bound(paymentDates_.begin(), paymentDates_.end(), asOfDate);
    if (it != paymentDates_.end()) {
        return *it;
    }
    // Return a null date if no future payment dates
    return Date();
}

void DealBase::initialize() {
    // Default initialization - derived classes can override
    status_ = DealStatus::Active;
}

void DealBase::processPaymentDate(const Date& paymentDate) {
    // Update current date
    currentDate_ = paymentDate;
    
    // Accrue interest on all accounts
    accrueInterestOnAllAccounts(paymentDate);
    
    // Run the waterfall (implemented by derived classes)
    runWaterfall(paymentDate);
    
    // Check if deal has matured
    if (paymentDate >= dealInfo_.maturityDate) {
        status_ = DealStatus::Matured;
    }
}

bool DealBase::isMatured() const {
    return status_ == DealStatus::Matured || 
           currentDate_ >= dealInfo_.maturityDate;
}

bool DealBase::canAccelerate() const {
    return status_ == DealStatus::Active && !isMatured();
}

void DealBase::accelerate() {
    if (canAccelerate()) {
        status_ = DealStatus::Accelerated;
    }
}

std::string DealBase::statusToString(DealStatus status) {
    switch (status) {
        case DealStatus::Pending:     return "Pending";
        case DealStatus::Active:      return "Active";
        case DealStatus::Defaulted:   return "Defaulted";
        case DealStatus::Accelerated: return "Accelerated";
        case DealStatus::Matured:     return "Matured";
        case DealStatus::Called:      return "Called";
        default:                      return "Unknown";
    }
}

DealBase::DealStatus DealBase::stringToStatus(const std::string& statusStr) {
    if (statusStr == "Pending")     return DealStatus::Pending;
    if (statusStr == "Active")      return DealStatus::Active;
    if (statusStr == "Defaulted")   return DealStatus::Defaulted;
    if (statusStr == "Accelerated") return DealStatus::Accelerated;
    if (statusStr == "Matured")     return DealStatus::Matured;
    if (statusStr == "Called")      return DealStatus::Called;
    throw std::invalid_argument("Invalid deal status string: " + statusStr);
}

void DealBase::transferBetweenAccounts(const std::string& fromAccount, 
                                     const std::string& toAccount, 
                                     Balance amount) {
    Account* from = getAccount(fromAccount);
    Account* to = getAccount(toAccount);
    
    if (!from) {
        throw std::runtime_error("Source account not found: " + fromAccount);
    }
    if (!to) {
        throw std::runtime_error("Destination account not found: " + toAccount);
    }
    
    // Attempt to transfer between accounts
    if (from->getBalance() >= amount) {
        std::string transferComment = "Transfer to " + toAccount;
        if (from->withdraw(currentDate_, amount, transferComment)) {
            to->deposit(currentDate_, amount, "Transfer from " + fromAccount);
        }
    }
}

Balance DealBase::getAvailableAccountBalance(const std::string& accountName) const {
    const Account* account = getAccount(accountName);
    if (!account) {
        return 0.0;
    }
    return account->getBalance();
}

void DealBase::accrueInterestOnAllAccounts(const Date& accrualDate) {
    for (const auto& pair : accounts_) {
        pair.second->accrueInterest(accrualDate);
    }
}

std::vector<std::string> DealBase::validateBasicDealStructure() const {
    std::vector<std::string> errors;
    
    // Check basic deal info
    if (dealInfo_.dealName.empty()) {
        errors.push_back("Deal name cannot be empty");
    }
    
    if (dealInfo_.maturityDate <= dealInfo_.issueDate) {
        errors.push_back("Maturity date must be after issue date");
    }
    
    if (paymentDates_.empty()) {
        errors.push_back("Deal must have at least one payment date");
    }
    
    // Check that first payment date is after issue date
    if (!paymentDates_.empty() && paymentDates_[0] <= dealInfo_.issueDate) {
        errors.push_back("First payment date must be after issue date");
    }
    
    // Check that last payment date is not after maturity
    if (!paymentDates_.empty() && paymentDates_.back() > dealInfo_.maturityDate) {
        errors.push_back("Payment dates cannot extend beyond maturity date");
    }
    
    return errors;
}

} // namespace Structura