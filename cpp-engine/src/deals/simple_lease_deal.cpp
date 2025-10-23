#include "simple_lease_deal.h"
#include <sstream>

namespace Structura {

SimpleLeaseDeal::SimpleLeaseDeal(const DealInfo& deal_info)
    : DealBase(deal_info), lease_pool_(nullptr) {
    
    // Create simple collection account
    collection_account_id_ = "collection_" + deal_info.dealName;
    accounts_[collection_account_id_] = std::make_unique<Account>(collection_account_id_);
}

void SimpleLeaseDeal::setLeasePool(Pool<Lease>* pool) {
    lease_pool_ = pool;
}

void SimpleLeaseDeal::runWaterfall(const Date& paymentDate) {
    if (!lease_pool_) return;
    
    processLeasePayments(paymentDate);
}

Balance SimpleLeaseDeal::calculateDealValue(const Date& valuationDate) const {
    if (!lease_pool_) return 0.0;
    
    return lease_pool_->getCurrentBalance();
}

std::vector<std::string> SimpleLeaseDeal::validate() const {
    std::vector<std::string> errors;
    
    if (!lease_pool_) {
        errors.push_back("No lease pool assigned");
    }
    
    if (accounts_.empty()) {
        errors.push_back("No accounts configured");
    }
    
    return errors;
}

std::string SimpleLeaseDeal::getDealSummary() const {
    std::ostringstream oss;
    
    oss << "Simple Lease Deal: " << getDealInfo().dealName << "\\n";
    
    if (lease_pool_) {
        oss << "Pool Balance: $" << lease_pool_->getCurrentBalance() << "\\n";
        oss << "Number of Leases: " << lease_pool_->getAssetCount() << "\\n";
    }
    
    oss << "Accounts: " << accounts_.size() << "\\n";
    
    return oss.str();
}

void SimpleLeaseDeal::processPaymentDate(const Date& paymentDate) {
    runWaterfall(paymentDate);
}

void SimpleLeaseDeal::processLeasePayments(const Date& paymentDate) {
    if (!lease_pool_) return;
    
    auto collection_account = accounts_.find(collection_account_id_);
    if (collection_account == accounts_.end()) return;
    
    // Simple payment processing - collect lease payments
    Balance total_collections = 0.0;
    
    auto& leases = lease_pool_->getAssets();
    for (auto& lease : leases) {
        if (lease.getStatus() == Status::Current) {
            // Calculate lease payment for this period
            Balance payment = lease.getCurrentBalance() * 0.01; // Simple 1% payment
            total_collections += payment;
        }
    }
    
    // Deposit collections into account
    if (total_collections > 0) {
        collection_account->second->deposit(paymentDate, total_collections);
    }
}

} // namespace Structura