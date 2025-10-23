#include "cpp-engine/src/assets/pool.h"
#include "cpp-engine/src/core/date_utils.h"
#include <iostream>

using namespace Structura;

int main() {
    BalanceFactorPricing pricing(1.0, 0.5);
    
    PoolCashflow cashflow;
    Date testDate = DateUtils::makeDate(2024, 7, 15);
    cashflow.dates.push_back(testDate);
    cashflow.remainingBalances.push_back(100000.0);
    cashflow.defaults.push_back(10000.0);
    cashflow.recoveries.push_back(2000.0);
    
    std::cout << "Cashflow dates size: " << cashflow.dates.size() << std::endl;
    std::cout << "Remaining balances size: " << cashflow.remainingBalances.size() << std::endl;
    std::cout << "First date: " << cashflow.dates[0] << std::endl;
    std::cout << "Test date: " << testDate << std::endl;
    
    Balance value = pricing.calculateValue(cashflow, testDate);
    std::cout << "Calculated value: " << value << std::endl;
    
    return 0;
}
