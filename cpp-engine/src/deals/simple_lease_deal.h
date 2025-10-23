#pragma once

#include "core/deal_base.h"
#include "assets/lease.h"
#include "assets/pool.h"

namespace Structura {

/**
 * Simple Lease Deal - minimal working implementation
 */
class SimpleLeaseDeal : public DealBase {
private:
    Pool<Lease>* lease_pool_;
    std::string collection_account_id_;

public:
    explicit SimpleLeaseDeal(const DealInfo& deal_info);
    
    // DealBase interface
    void runWaterfall(const Date& paymentDate) override;
    Balance calculateDealValue(const Date& valuationDate) const override;
    std::vector<std::string> validate() const override;
    std::string getDealSummary() const override;
    void processPaymentDate(const Date& paymentDate) override;
    
    // Simple lease operations
    void setLeasePool(Pool<Lease>* pool);
    void processLeasePayments(const Date& paymentDate);
};

} // namespace Structura