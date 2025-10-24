#include <iostream>
#include <iomanip>
#include <memory>
#include <vector>
#include <string>

// Core components
#include "../src/core/types.h"
#include "../src/core/deal_base.h"
#include "../src/core/account.h"

// Asset components  
#include "../src/assets/asset_base.h"
#include "../src/assets/corporate_loan.h"
#include "../src/assets/pool.h"

// Deal components
#include "../src/deals/clo_deal.h"

// Analytics, Validation, and Reporting
#include "../src/analytics/analytics_engine.h"
#include "../src/validation/validation_engine.h"
#include "../src/reporting/reporting_engine.h"

using namespace Structura;

class StructuraWorkflowDemo {
private:
    std::unique_ptr<CLODeal> cloDeal_;
    std::unique_ptr<AnalyticsEngine> analyticsEngine_;
    std::unique_ptr<ValidationEngine> validationEngine_;
    std::unique_ptr<ReportingEngine> reportingEngine_;
    
    std::vector<Date> paymentDates_;
    Date closingDate_;
    Date maturityDate_;

public:
    StructuraWorkflowDemo() {
        closingDate_ = DateUtils::makeDate(2024, 1, 15);
        maturityDate_ = DateUtils::makeDate(2029, 1, 15);
        
        // Initialize engines
        analyticsEngine_ = std::make_unique<AnalyticsEngine>();
        validationEngine_ = std::make_unique<ValidationEngine>(std::move(std::make_unique<AnalyticsEngine>()));
        reportingEngine_ = std::make_unique<ReportingEngine>();
    }
    
    void run() {
        std::cout << "🏗️  STRUCTURA COMPREHENSIVE WORKFLOW DEMO\n";
        std::cout << "==========================================\n\n";
        
        try {
            // Step 1: Create Deal
            std::cout << "Step 1: Creating CLO Deal...\n";
            createCLODeal();
            
            // Step 2: Add Accounts
            std::cout << "Step 2: Setting up deal accounts...\n";
            setupAccounts();
            
            // Step 3: Create Asset Pool
            std::cout << "Step 3: Creating collateral pool...\n";
            createCollateralPool();
            
            // Step 4: Add Tranches
            std::cout << "Step 4: Setting up CLO tranches...\n";
            setupTranches();
            
            // Step 5: Set Deal Parameters
            std::cout << "Step 5: Configuring deal parameters...\n";
            setDealParameters();
            
            // Step 6: Run Analytics
            std::cout << "Step 6: Running analytics...\n";
            runAnalytics();
            
            // Step 7: Validate Deal
            std::cout << "Step 7: Validating deal structure...\n";
            validateDeal();
            
            // Step 8: Generate Cashflows
            std::cout << "Step 8: Generating cashflow projections...\n";
            generateCashflows();
            
            // Step 9: Create Reports
            std::cout << "Step 9: Generating comprehensive reports...\n";
            generateReports();
            
            std::cout << "\n✅ Demo completed successfully!\n";
            
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
        dealInfo.description = "Commercial Real Estate CLO";
        
        // Create CLO deal
        cloDeal_ = std::make_unique<CLODeal>(dealInfo);
        
        std::cout << "   ✓ Created CLO: " << dealInfo.dealName << "\n";
        std::cout << "   ✓ Closing Date: " << DateUtils::toString(closingDate_) << "\n";
        std::cout << "   ✓ Maturity Date: " << DateUtils::toString(maturityDate_) << "\n\n";
    }
    
    void setupAccounts() {
        // Create cash account
        auto cashAccount = std::make_unique<Account>("CASH", 5000000.0);  // $5M initial
        
        // Create reserve account with interest
        InterestInfo reserveInterestInfo = InterestInfo::createBankAccount(0.02, closingDate_);
        auto reserveAccount = std::make_unique<Account>("RESERVE", 10000000.0, reserveInterestInfo);
        
        // Create interest collection account
        auto interestAccount = std::make_unique<Account>("INTEREST_COLLECTION", 0.0);
        
        // Create principal collection account
        auto principalAccount = std::make_unique<Account>("PRINCIPAL_COLLECTION", 0.0);
        
        // Add accounts to deal
        cloDeal_->addAccount("CASH", std::move(cashAccount));
        cloDeal_->addAccount("RESERVE", std::move(reserveAccount));
        cloDeal_->addAccount("INTEREST_COLLECTION", std::move(interestAccount));
        cloDeal_->addAccount("PRINCIPAL_COLLECTION", std::move(principalAccount));
        
        std::cout << "   ✓ Setup 4 deal accounts\n";
        std::cout << "   ✓ Cash Account: $5,000,000\n";
        std::cout << "   ✓ Reserve Account: $10,000,000 (with 2% interest)\n\n";
    }
    
    void createCollateralPool() {
        std::vector<CorporateLoan> loans;
        Balance totalCollateral = 0.0;
        
        // Create diverse loan portfolio
        std::vector<std::string> industries = {
            "Commercial Real Estate", "Healthcare", "Technology", 
            "Manufacturing", "Retail", "Energy", "Transportation"
        };
        
        std::vector<std::string> ratings = {"AAA", "AA", "A", "BBB", "BB", "B"};
        
        for (int i = 0; i < 150; ++i) {  // 150 loans
            // Vary loan characteristics
            Balance originalBalance = 500000.0 + (i * 25000.0);  // $500K - $4.25M
            Rate interestRate = 0.055 + (i * 0.0005);            // 5.5% - 13.0%
            int termMonths = 60 + (i % 24);                       // 60-84 months
            
            OriginalInfo loanInfo(originalBalance, interestRate, termMonths,
                                 closingDate_, Period::Monthly, AmortPlan::Level);
            
            // Create corporate loan
            CorporateLoan loan(loanInfo, originalBalance, interestRate, termMonths);
            loan.setBorrowerName("Borrower_" + std::to_string(i + 1));
            loan.setIndustry(industries[i % industries.size()]);
            loan.setCreditRating(ratings[i % ratings.size()]);
            
            totalCollateral += originalBalance;
            loans.push_back(std::move(loan));
        }
        
        // Create pool from loans
        auto loanPool = std::make_unique<Pool<CorporateLoan>>(loans, closingDate_);
        
        // Set pool in CLO deal
        cloDeal_->setCollateralPool(std::move(loanPool));
        
        std::cout << "   ✓ Created portfolio of " << loans.size() << " loans\n";
        std::cout << "   ✓ Total Collateral: $" << std::fixed << std::setprecision(0) 
                  << totalCollateral << "\n";
        std::cout << "   ✓ Industries: " << industries.size() << " sectors\n";
        std::cout << "   ✓ Credit Ratings: AAA to B\n\n";
    }
    
    void setupTranches() {
        // Create CLO tranche structure (typical CLO sizing)
        std::vector<CLOTranche> tranches = {
            {"CLASS_A_1", CLOTrancheType::AAA, 400000000.0, 0.035, "AAA"},    // $400M @ 3.5%
            {"CLASS_A_2", CLOTrancheType::AA,  150000000.0, 0.042, "AA"},     // $150M @ 4.2%
            {"CLASS_B",   CLOTrancheType::A,   100000000.0, 0.055, "A"},      // $100M @ 5.5%
            {"CLASS_C",   CLOTrancheType::BBB,  75000000.0, 0.075, "BBB"},    // $75M @ 7.5%
            {"CLASS_D",   CLOTrancheType::BB,   50000000.0, 0.095, "BB"},     // $50M @ 9.5%
            {"CLASS_E",   CLOTrancheType::B,    25000000.0, 0.125, "B"},      // $25M @ 12.5%
            {"EQUITY",    CLOTrancheType::EQUITY, 50000000.0, 0.0, "Unrated"} // $50M equity
        };
        
        Balance totalTranches = 0.0;
        for (const auto& tranche : tranches) {
            cloDeal_->addTranche(tranche);
            totalTranches += tranche.notionalAmount;
            
            std::cout << "   ✓ " << tranche.name << ": $" 
                      << std::fixed << std::setprecision(0) << tranche.notionalAmount
                      << " @ " << std::fixed << std::setprecision(2) << (tranche.couponRate * 100) 
                      << "% (" << tranche.rating << ")\n";
        }
        
        std::cout << "   ✓ Total CLO Size: $" << std::fixed << std::setprecision(0) 
                  << totalTranches << "\n\n";
    }
    
    void setDealParameters() {
        // Generate quarterly payment dates for 5 years
        Date currentPaymentDate = closingDate_;
        for (int i = 0; i < 20; ++i) {  // 20 quarterly payments
            currentPaymentDate = DateUtils::addMonths(currentPaymentDate, 3);
            paymentDates_.push_back(currentPaymentDate);
        }
        
        cloDeal_->setPaymentDates(paymentDates_);
        
        // Set forecasting assumptions
        ForecastAssumptions assumptions;
        assumptions.defaultRate = 0.015;           // 1.5% annual default rate
        assumptions.recoveryRate = 0.45;           // 45% recovery rate
        assumptions.prepaymentRate = 0.12;         // 12% annual prepayment rate
        assumptions.interestRateScenario = "Base Case";
        assumptions.stressScenario = false;
        
        cloDeal_->setForecastAssumptions(assumptions);
        
        std::cout << "   ✓ Payment Schedule: " << paymentDates_.size() << " quarterly payments\n";
        std::cout << "   ✓ Default Rate: " << (assumptions.defaultRate * 100) << "%\n";
        std::cout << "   ✓ Recovery Rate: " << (assumptions.recoveryRate * 100) << "%\n";
        std::cout << "   ✓ Prepayment Rate: " << (assumptions.prepaymentRate * 100) << "%\n\n";
    }
    
    void runAnalytics() {
        // Add historical data for analytics
        std::vector<Amount> defaultRateHistory = {0.01, 0.012, 0.015, 0.018, 0.015, 0.013};
        std::vector<Amount> recoveryRateHistory = {0.42, 0.45, 0.43, 0.47, 0.45, 0.44};
        std::vector<Date> historicalDates;
        
        Date histDate = DateUtils::addMonths(closingDate_, -12);
        for (size_t i = 0; i < defaultRateHistory.size(); ++i) {
            historicalDates.push_back(histDate);
            histDate = DateUtils::addMonths(histDate, 2);
        }
        
        analyticsEngine_->addTimeSeries("default_rate", historicalDates, defaultRateHistory);
        analyticsEngine_->addTimeSeries("recovery_rate", historicalDates, recoveryRateHistory);
        
        // Calculate comprehensive metrics
        auto performanceMetrics = analyticsEngine_->calculatePerformanceMetrics(*cloDeal_, closingDate_);
        auto riskMetrics = analyticsEngine_->calculateRiskMetrics(*cloDeal_, closingDate_);
        auto creditMetrics = analyticsEngine_->calculateCreditMetrics(*cloDeal_, closingDate_);
        
        std::cout << "   ✓ Calculated Performance Metrics: " << performanceMetrics.size() << " metrics\n";
        std::cout << "   ✓ Calculated Risk Metrics: " << riskMetrics.size() << " metrics\n";
        std::cout << "   ✓ Calculated Credit Metrics: " << creditMetrics.size() << " metrics\n";
        
        // Display some key metrics
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
        // Run comprehensive validation
        ValidationSummary validation = validationEngine_->validateDeal(*cloDeal_);
        
        std::cout << "   🔍 Validation Results:\n";
        std::cout << "   ✓ Rules Executed: " << validation.total_rules_executed << "\n";
        std::cout << "   ✓ Rules Passed: " << validation.rules_passed << "\n";
        std::cout << "   ✓ Rules Failed: " << validation.rules_failed << "\n";
        std::cout << "   ✓ Warnings: " << validation.rules_warning << "\n";
        std::cout << "   📊 Validation Score: " << std::fixed << std::setprecision(1) 
                  << validation.validation_score << "%\n";
        std::cout << "   📊 Overall Status: " << validation.overall_status << "\n";
        
        // Show critical issues if any
        if (!validation.critical_issues.empty()) {
            std::cout << "   ⚠️  Critical Issues:\n";
            for (const auto& issue : validation.critical_issues) {
                std::cout << "      - " << issue.rule_name << ": " << issue.message << "\n";
            }
        }
        
        // Show some errors/warnings
        if (!validation.errors.empty()) {
            std::cout << "   ⚠️  Errors (" << validation.errors.size() << "):\n";
            for (size_t i = 0; i < std::min(size_t(3), validation.errors.size()); ++i) {
                std::cout << "      - " << validation.errors[i].rule_name << "\n";
            }
            if (validation.errors.size() > 3) {
                std::cout << "      ... and " << (validation.errors.size() - 3) << " more\n";
            }
        }
        
        std::cout << "\n";
    }
    
    void generateCashflows() {
        std::cout << "   💰 Cashflow Projections:\n";
        std::cout << "   " << std::string(80, '-') << "\n";
        std::cout << "   Date          Principal    Interest     Total      Cum. Principal\n";
        std::cout << "   " << std::string(80, '-') << "\n";
        
        Balance cumulativePrincipal = 0.0;
        
        // Run waterfall for first 5 payment dates (demo purposes)
        for (size_t i = 0; i < std::min(size_t(5), paymentDates_.size()); ++i) {
            const Date& paymentDate = paymentDates_[i];
            
            // Simulate waterfall calculation
            Balance principalPayment = 15000000.0 * (1 + i * 0.1);  // Increasing over time
            Balance interestPayment = 8000000.0 * (1 - i * 0.05);   // Decreasing over time  
            Balance totalPayment = principalPayment + interestPayment;
            cumulativePrincipal += principalPayment;
            
            std::cout << "   " << DateUtils::toString(paymentDate) << "  "
                      << "$" << std::setw(10) << std::fixed << std::setprecision(0) << principalPayment << "  "
                      << "$" << std::setw(10) << interestPayment << "  "
                      << "$" << std::setw(10) << totalPayment << "  "
                      << "$" << std::setw(12) << cumulativePrincipal << "\n";
        }
        
        if (paymentDates_.size() > 5) {
            std::cout << "   ... and " << (paymentDates_.size() - 5) << " more payment dates\n";
        }
        
        std::cout << "   " << std::string(80, '-') << "\n\n";
    }
    
    void generateReports() {
        // Generate Deal Performance Report
        ReportConfig performanceConfig;
        performanceConfig.includeCharts = true;
        performanceConfig.detailLevel = "COMPREHENSIVE";
        
        ReportDocument performanceReport = reportingEngine_->generateReport(
            *cloDeal_, ReportType::DEAL_PERFORMANCE, performanceConfig, closingDate_);
        
        std::cout << "   📄 Generated Reports:\n";
        std::cout << "   ✓ Deal Performance Report: " << performanceReport.sections.size() 
                  << " sections\n";
        
        // Generate Risk Analytics Report
        ReportDocument riskReport = reportingEngine_->generateReport(
            *cloDeal_, ReportType::RISK_ANALYTICS, performanceConfig, closingDate_);
        
        std::cout << "   ✓ Risk Analytics Report: " << riskReport.sections.size() 
                  << " sections\n";
        
        // Generate Cashflow Projection Report
        ReportDocument cashflowReport = reportingEngine_->generateReport(
            *cloDeal_, ReportType::CASHFLOW_PROJECTION, performanceConfig, closingDate_);
        
        std::cout << "   ✓ Cashflow Projection Report: " << cashflowReport.sections.size() 
                  << " sections\n";
        
        // Export to different formats
        std::string htmlExport = reportingEngine_->exportReport(performanceReport, ReportFormat::HTML);
        std::string jsonExport = reportingEngine_->exportReport(performanceReport, ReportFormat::JSON);
        std::string csvExport = reportingEngine_->exportReport(performanceReport, ReportFormat::CSV);
        
        std::cout << "   ✓ HTML Export: " << htmlExport.length() << " characters\n";
        std::cout << "   ✓ JSON Export: " << jsonExport.length() << " characters\n";
        std::cout << "   ✓ CSV Export: " << csvExport.length() << " characters\n";
        
        // Show sample report content
        std::cout << "\n   📋 Sample Report Content (Performance Report):\n";
        std::cout << "   Title: " << performanceReport.title << "\n";
        std::cout << "   Generated: " << DateUtils::toString(performanceReport.generated_date) << "\n";
        std::cout << "   Sections: ";
        for (size_t i = 0; i < std::min(size_t(3), performanceReport.sections.size()); ++i) {
            std::cout << performanceReport.sections[i].title;
            if (i < std::min(size_t(3), performanceReport.sections.size()) - 1) std::cout << ", ";
        }
        if (performanceReport.sections.size() > 3) {
            std::cout << "...";
        }
        std::cout << "\n";
        
        std::cout << "\n";
    }
};

int main() {
    StructuraWorkflowDemo demo;
    demo.run();
    return 0;
}