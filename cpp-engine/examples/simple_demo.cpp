#include "entities/deal.h"
#include <iostream>
#include <iomanip>

using namespace Structura;

int main() {
    std::cout << "Structura C++ Demo - Clean Version\n";
    std::cout << "===================================\n\n";
    
    try {
        // Create a simple deal programmatically
        Date closing = DateUtils::makeDate(2024, 1, 15);
        Date maturity = DateUtils::makeDate(2026, 1, 15);
        
        auto deal = std::make_unique<Deal>("DEMO_DEAL", "Demo Structured Deal", closing, maturity);
        
        // Add accounts
        auto cash_account = std::make_shared<Account>("CASH", 1000000.0);
        auto reserve_account = std::make_shared<Account>("RESERVE", 50000.0);
        deal->addAccount(cash_account);
        deal->addAccount(reserve_account);
        
        // Add bonds
        auto bond_a = std::make_shared<Bond>("BOND_A", "Class A Notes", 5000000.0, 0.045, maturity);
        auto bond_b = std::make_shared<Bond>("BOND_B", "Class B Notes", 1000000.0, 0.065, maturity);
        deal->addBond(bond_a);
        deal->addBond(bond_b);
        
        // Add fees
        auto servicer_fee = std::make_shared<Fee>("SERVICER", "Servicer Fee", 0.005);
        deal->addFee(servicer_fee);
        
        // Display deal information
        std::cout << "Deal: " << deal->getName() << " (" << deal->getId() << ")\n";
        std::cout << "Closing: " << DateUtils::toString(deal->getClosingDate()) << "\n";
        std::cout << "Maturity: " << DateUtils::toString(deal->getMaturityDate()) << "\n\n";
        
        // Display accounts
        std::cout << "ACCOUNTS:\n";
        for (const auto& [id, account] : deal->getAccounts()) {
            std::cout << "  " << account->getName() << ": $" 
                      << std::fixed << std::setprecision(2) 
                      << account->getBalance() << "\n";
        }
        std::cout << "\n";
        
        // Display bonds
        std::cout << "BONDS:\n";
        for (const auto& [id, bond] : deal->getBonds()) {
            std::cout << "  " << bond->getName() << ": $" 
                      << std::fixed << std::setprecision(2) 
                      << bond->getCurrentBalance() 
                      << " (Rate: " << std::setprecision(3) << bond->getCurrentRate() * 100 << "%)\n";
        }
        std::cout << "\n";
        
        // Display fees
        std::cout << "FEES:\n";
        for (const auto& [id, fee] : deal->getFees()) {
            std::cout << "  " << fee->getName() << ": Rate " 
                      << std::setprecision(3) << fee->getFeeRate() * 100 << "%, Due $"
                      << std::fixed << std::setprecision(2) << fee->getDueAmount() << "\n";
        }
        std::cout << "\n";
        
        // Simple operations demonstration
        std::cout << "PERFORMING SIMPLE OPERATIONS:\n";
        
        // Calculate and pay servicer fee
        servicer_fee->calculateFee(deal->getTotalBondBalance());
        std::cout << "Calculated servicer fee: $" << servicer_fee->getDueAmount() << "\n";
        
        // Pay fee from cash account
        Amount requested_amount = servicer_fee->getDueAmount();
        bool success = Account::transfer(*cash_account, *reserve_account, closing, requested_amount);
        Amount fee_payment = success ? requested_amount : 0.0;
        servicer_fee->payFee(fee_payment);
        std::cout << "Paid servicer fee: $" << fee_payment << "\n";
        
        // Calculate interest for bonds
        Date interest_start = DateUtils::makeDate(2024, 1, 15);
        Date interest_end = DateUtils::makeDate(2024, 2, 15);
        
        Balance bond_a_interest = bond_a->calculateInterest(interest_start, interest_end);
        Balance bond_b_interest = bond_b->calculateInterest(interest_start, interest_end);
        
        std::cout << "Bond A interest (1 month): $" << bond_a_interest << "\n";
        std::cout << "Bond B interest (1 month): $" << bond_b_interest << "\n";
        
        // Show final balances
        std::cout << "\nFINAL BALANCES:\n";
        std::cout << "Cash Account: $" << cash_account->getBalance() << "\n";
        std::cout << "Reserve Account: $" << reserve_account->getBalance() << "\n";
        std::cout << "Total Bond Balance: $" << deal->getTotalBondBalance() << "\n";
        std::cout << "Total Account Balance: $" << deal->getTotalAccountBalance() << "\n";
        
        std::cout << "\n✅ Demo completed successfully!\n";
        
    } catch (const std::exception& e) {
        std::cerr << "❌ Error: " << e.what() << "\n";
        return 1;
    }
    
    return 0;
}