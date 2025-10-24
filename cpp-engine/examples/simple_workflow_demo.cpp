#include <iostream>
#include <memory>
#include <vector>
#include <string>

// Core components
#include "../src/core/types.h"
#include "../src/core/deal_base.h"
#include "../src/core/account.h"

// Deal components
#include "../src/deals/clo_deal.h"

// Analytics, Validation, and Reporting
#include "../src/analytics/analytics_engine.h"
#include "../src/validation/validation_engine.h"
#include "../src/reporting/reporting_engine.h"

using namespace Structura;

class SimpleWorkflowDemo {
private:
    std::unique_ptr<CLODeal> cloDeal_;
    std::unique_ptr<AnalyticsEngine> analyticsEngine_;
    std::unique_ptr<ValidationEngine> validationEngine_;
    std::unique_ptr<ReportingEngine> reportingEngine_;
    
    Date closingDate_;
    Date maturityDate_;

public:
    SimpleWorkflowDemo() {
        closingDate_ = DateUtils::makeDate(2024, 1, 15);
        maturityDate_ = DateUtils::makeDate(2029, 1, 15);
        
        // Initialize engines  
        analyticsEngine_ = std::make_unique<AnalyticsEngine>();
        validationEngine_ = std::make_unique<ValidationEngine>(std::make_unique<AnalyticsEngine>());
        reportingEngine_ = std::make_unique<ReportingEngine>();
    }
    
    void run() {
        std::cout << "🏗️  STRUCTURA WORKFLOW DEMO\n";
        std::cout << "============================\n\n";
        
        try {
            // Step 1: Create Deal
            std::cout << "Step 1: Creating CLO Deal...\n";
            createCLODeal();
            
            // Step 2: Add Basic Accounts
            std::cout << "Step 2: Setting up basic accounts...\n";
            setupBasicAccounts();
            
            // Step 3: Setup CLO Tranches
            std::cout << "Step 3: Setting up CLO tranches...\n";
            setupBasicTranches();
            
            // Step 4: Run Analytics
            std::cout << "Step 4: Running basic analytics...\n";
            runBasicAnalytics();
            
            // Step 5: Validate Deal
            std::cout << "Step 5: Validating deal structure...\n";
            validateDeal();
            
            // Step 6: Generate Reports
            std::cout << "Step 6: Generating basic reports...\n";
            generateBasicReports();
            
            std::cout << "\n✅ Demo completed successfully!\n";
            std::cout << "\nThis demonstrates the core Structura system components:\n";
            std::cout << "• Deal creation and management\n";
            std::cout << "• Account setup and tracking\n";
            std::cout << "• CLO tranche structuring\n";
            std::cout << "• Analytics engine integration\n";
            std::cout << "• Validation framework\n";
            std::cout << "• Reporting system\n\n";
            std::cout << "For full functionality, see the markdown documentation!\n";
            
        } catch (const std::exception& e) {
            std::cerr << "❌ Error: " << e.what() << "\n";
        }
    }

private:
    void createCLODeal() {
        // Create deal information
        DealInfo dealInfo("STRUCT_2024_1", "CLO", closingDate_, maturityDate_);
        dealInfo.trustee = "Bank of New York Mellon";
        dealInfo.underwriter = "Goldman Sachs";
        dealInfo.currency = "USD";
        dealInfo.description = "Sample CLO Deal for Demo";
        
        // Create CLO deal
        cloDeal_ = std::make_unique<CLODeal>(dealInfo);
        
        std::cout << "   ✓ Created CLO: " << dealInfo.dealName << "\n";
        std::cout << "   ✓ Closing Date: " << DateUtils::toString(closingDate_) << "\n";
        std::cout << "   ✓ Maturity Date: " << DateUtils::toString(maturityDate_) << "\n\n";
    }
    
    void setupBasicAccounts() {
        // Create basic accounts
        auto cashAccount = std::make_unique<Account>("CASH", 5000000.0);
        InterestInfo reserveInterest = InterestInfo::createBankAccount(0.02, closingDate_);
        auto reserveAccount = std::make_unique<Account>("RESERVE", 10000000.0, reserveInterest);
        
        // Add accounts to deal
        cloDeal_->addAccount("CASH", std::move(cashAccount));
        cloDeal_->addAccount("RESERVE", std::move(reserveAccount));
        
        std::cout << "   ✓ Cash Account: $5,000,000\n";
        std::cout << "   ✓ Reserve Account: $10,000,000 (with 2% interest)\n\n";
    }
    
    void setupBasicTranches() {
        // Create basic CLO tranches using proper constructor
        CLOTranche trancheAAA("CLASS_A", CLOTrancheType::AAA, "AAA", 400000000.0, 0.035, false);
        CLOTranche trancheAA("CLASS_B", CLOTrancheType::AA, "AA", 150000000.0, 0.045, false);
        CLOTranche trancheBBB("CLASS_C", CLOTrancheType::BBB, "BBB", 100000000.0, 0.075, false);
        CLOTranche equity("EQUITY", CLOTrancheType::EQUITY, "Unrated", 50000000.0, 0.0, false);
        
        // Add tranches to deal
        cloDeal_->addTranche(trancheAAA);
        cloDeal_->addTranche(trancheAA);
        cloDeal_->addTranche(trancheBBB);
        cloDeal_->addTranche(equity);
        
        std::cout << "   ✓ CLASS_A (AAA): $400,000,000 @ 3.5%\n";
        std::cout << "   ✓ CLASS_B (AA): $150,000,000 @ 4.5%\n";
        std::cout << "   ✓ CLASS_C (BBB): $100,000,000 @ 7.5%\n";
        std::cout << "   ✓ EQUITY: $50,000,000\n";
        std::cout << "   ✓ Total CLO Size: $700,000,000\n\n";
    }
    
    void runBasicAnalytics() {
        // Calculate basic metrics using the analytics engine
        auto performanceMetrics = analyticsEngine_->calculatePerformanceMetrics(*cloDeal_, closingDate_);
        auto riskMetrics = analyticsEngine_->calculateRiskMetrics(*cloDeal_, closingDate_);
        
        std::cout << "   ✓ Performance Metrics: " << performanceMetrics.size() << " metrics calculated\n";
        std::cout << "   ✓ Risk Metrics: " << riskMetrics.size() << " metrics calculated\n";
        
        // Display sample metrics if available
        if (performanceMetrics.find("total_return") != performanceMetrics.end()) {
            std::cout << "   📊 Total Return: " << std::fixed << std::setprecision(2) 
                      << (performanceMetrics.at("total_return") * 100) << "%\n";
        }
        if (riskMetrics.find("value_at_risk") != riskMetrics.end()) {
            std::cout << "   📊 Value at Risk: $" << std::fixed << std::setprecision(0) 
                      << riskMetrics.at("value_at_risk") << "\n";
        }
        
        std::cout << "\n";
    }
    
    void validateDeal() {
        // Run validation on the deal
        ValidationSummary validation = validationEngine_->validateDeal(*cloDeal_);
        
        std::cout << "   🔍 Validation Results:\n";
        std::cout << "   ✓ Rules Executed: " << validation.total_rules_executed << "\n";
        std::cout << "   ✓ Rules Passed: " << validation.rules_passed << "\n";
        std::cout << "   ✓ Rules Failed: " << validation.rules_failed << "\n";
        std::cout << "   ✓ Warnings: " << validation.rules_warning << "\n";
        std::cout << "   📊 Validation Score: " << std::fixed << std::setprecision(1) 
                  << validation.validation_score << "%\n";
        std::cout << "   📊 Overall Status: " << validation.overall_status << "\n";
        
        if (!validation.critical_issues.empty()) {
            std::cout << "   ⚠️  Critical Issues Found: " << validation.critical_issues.size() << "\n";
        }
        
        std::cout << "\n";
    }
    
    void generateBasicReports() {
        // Generate basic reports
        ReportConfig config;
        config.include_charts = true;
        
        ReportDocument performanceReport = reportingEngine_->generateReport(*cloDeal_, config, closingDate_);
        
        std::cout << "   📄 Generated Report: " << performanceReport.title << "\n";
        std::cout << "   ✓ Report Sections: " << performanceReport.sections.size() << "\n";
        std::cout << "   ✓ Generated: " << DateUtils::toString(performanceReport.generation_date) << "\n";
        
        // Export to different formats
        std::string htmlExport = reportingEngine_->exportReport(performanceReport, ReportFormat::HTML);
        std::string jsonExport = reportingEngine_->exportReport(performanceReport, ReportFormat::JSON);
        
        std::cout << "   ✓ HTML Export: " << htmlExport.length() << " characters\n";
        std::cout << "   ✓ JSON Export: " << jsonExport.length() << " characters\n";
        
        std::cout << "\n";
    }
};

int main() {
    std::cout << "🚀 Starting Structura System Demo...\n\n";
    
    SimpleWorkflowDemo demo;
    demo.run();
    
    std::cout << "For a complete workflow including:\n";
    std::cout << "• Asset pool creation with 150+ loans\n";
    std::cout << "• Complex tranche structures\n";
    std::cout << "• Cashflow projections\n";
    std::cout << "• Advanced analytics and stress testing\n";
    std::cout << "• Comprehensive reporting\n\n";
    std::cout << "See the COMPREHENSIVE_WORKFLOW_DEMO.md documentation!\n";
    
    return 0;
}