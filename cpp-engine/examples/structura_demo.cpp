#include <iostream>
#include "core/types.h"
#include "assets/mortgage.h"
#include "assets/pool.h"
#include "deals/mortgage_deal.h"
#include "deals/simple_lease_deal.h"
#include "assets/lease.h"

using namespace Structura;

int main() {
    std::cout << "=== Structura Financial Engineering Demo ===" << std::endl;
    
    // 1. Create a mortgage pool
    std::cout << "\\n1. Creating Mortgage Pool..." << std::endl;
    
    Pool<Mortgage> mortgage_pool("MTG_POOL_001");
    
    // Add some sample mortgages
    Mortgage m1("MTG001", 350000.0, 0.045, Date(2024, 1, 1), Date(2054, 1, 1), PaymentFrequency::Monthly);
    Mortgage m2("MTG002", 275000.0, 0.038, Date(2024, 2, 1), Date(2054, 2, 1), PaymentFrequency::Monthly);
    
    mortgage_pool.addAsset(std::make_shared<Mortgage>(m1));
    mortgage_pool.addAsset(std::make_shared<Mortgage>(m2));
    
    std::cout << "Pool Balance: $" << mortgage_pool.getCurrentBalance() << std::endl;
    std::cout << "Asset Count: " << mortgage_pool.getAssetCount() << std::endl;
    
    // 2. Create a mortgage deal
    std::cout << "\\n2. Creating Mortgage Deal..." << std::endl;
    
    DealInfo deal_info = {
        "DEAL_001",
        "Sample Mortgage Deal",
        DealType::MORTGAGE_DEAL,
        Date(2024, 1, 15),
        Date(2054, 1, 15),
        DealStatus::Active
    };
    
    MortgageDeal mortgage_deal(deal_info);
    mortgage_deal.setMortgagePool(&mortgage_pool);
    
    std::cout << mortgage_deal.getDealSummary() << std::endl;
    
    // 3. Create a simple lease pool and deal
    std::cout << "\\n3. Creating Simple Lease Deal..." << std::endl;
    
    Pool<Lease> lease_pool("LEASE_POOL_001");
    
    // Create sample leases
    Equipment eq1("EQ001", "Excavator", 45000.0, 0.15);
    Lessee lessee1("LESSEE001", "Construction Corp", CreditScore::GOOD);
    Lease lease1("LEASE001", eq1, lessee1, 48, 1250.0, 0.06);
    
    Equipment eq2("EQ002", "Forklift", 28000.0, 0.12);
    Lessee lessee2("LESSEE002", "Warehouse Inc", CreditScore::EXCELLENT);
    Lease lease2("LEASE002", eq2, lessee2, 36, 950.0, 0.055);
    
    lease_pool.addAsset(std::make_shared<Lease>(lease1));
    lease_pool.addAsset(std::make_shared<Lease>(lease2));
    
    DealInfo lease_deal_info = {
        "LEASE_DEAL_001", 
        "Simple Lease Deal",
        DealType::LEASE_ABS_DEAL,
        Date(2024, 1, 15),
        Date(2028, 1, 15),
        DealStatus::Active
    };
    
    SimpleLeaseDeal lease_deal(lease_deal_info);
    lease_deal.setLeasePool(&lease_pool);
    
    std::cout << lease_deal.getDealSummary() << std::endl;
    
    // 4. Process payments
    std::cout << "\\n4. Processing Payments..." << std::endl;
    Date payment_date(2024, 2, 15);
    
    mortgage_deal.processPaymentDate(payment_date);
    lease_deal.processPaymentDate(payment_date);
    
    std::cout << "Payments processed for " << payment_date << std::endl;
    
    // 5. Validation
    std::cout << "\\n5. Validation..." << std::endl;
    
    auto mortgage_errors = mortgage_deal.validate();
    auto lease_errors = lease_deal.validate();
    
    std::cout << "Mortgage Deal Validation: " << 
        (mortgage_errors.empty() ? "PASSED" : "FAILED") << std::endl;
    std::cout << "Lease Deal Validation: " << 
        (lease_errors.empty() ? "PASSED" : "FAILED") << std::endl;
    
    std::cout << "\\n=== Demo Complete ===" << std::endl;
    
    return 0;
}