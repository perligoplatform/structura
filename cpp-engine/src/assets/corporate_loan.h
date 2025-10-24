#pragma once

#include "assets/asset_base.h"
#include <memory>
#include <optional>
#include <vector>
#include <map>
#include <set>

namespace Structura {

/**
 * Corporate loan facility types
 */
enum class CorporateLoanType {
    TERM_LOAN_A,              // Amortizing term loan (bank market)
    TERM_LOAN_B,              // Institutional term loan (non-bank market)
    REVOLVING_CREDIT,         // Revolving credit facility
    BRIDGE_LOAN,              // Bridge/temporary financing
    ACQUISITION_FINANCING,    // LBO/acquisition financing
    WORKING_CAPITAL,          // Working capital facility
    CAPITAL_EXPENDITURE,      // CapEx financing
    REAL_ESTATE_LOAN,         // Commercial real estate
    EQUIPMENT_FINANCING,      // Equipment-specific financing
    CONSTRUCTION_LOAN,        // Construction/development loan
    LETTER_OF_CREDIT,         // Standby letter of credit
    SWING_LINE,               // Swing line/same-day funding
    DELAYED_DRAW,             // Delayed draw term loan
    MULTICURRENCY            // Multi-currency facility
};

/**
 * Loan syndication structure
 */
enum class SyndicationRole {
    ADMINISTRATIVE_AGENT,     // Administrative agent (lead)
    COLLATERAL_AGENT,        // Collateral agent
    LEAD_ARRANGER,           // Lead arranger/bookrunner
    CO_AGENT,                // Co-administrative agent
    DOCUMENTATION_AGENT,      // Documentation agent
    SYNDICATION_AGENT,       // Syndication agent
    LENDER,                  // Participating lender
    PARTICIPANT              // Sub-participant
};

/**
 * Industry classification for corporate borrowers
 */
enum class IndustryClassification {
    AEROSPACE_DEFENSE,        // Aerospace & Defense
    AUTOMOTIVE,              // Automotive & Transportation
    BUSINESS_SERVICES,       // Business Services
    CHEMICALS,               // Chemicals, Plastics & Rubber
    CONSUMER_GOODS,          // Consumer Goods
    CONSTRUCTION,            // Construction & Building
    ENERGY,                  // Energy: Oil & Gas
    ENVIRONMENTAL,           // Environmental Industries
    FINANCIAL_SERVICES,      // Financial Services
    FOOD_BEVERAGE,          // Food, Beverage & Tobacco
    FORESTRY_PAPER,         // Forestry & Paper
    HEALTHCARE,             // Healthcare & Pharmaceuticals
    HIGH_TECH,              // High Tech Industries
    HOTEL_GAMING,           // Hotel, Gaming & Leisure
    MANUFACTURING,          // Manufacturing
    MEDIA_TELECOM,          // Media & Telecommunications
    METALS_MINING,          // Metals & Mining
    REAL_ESTATE,            // Real Estate
    RETAIL,                 // Retail
    SERVICES,               // Services
    UTILITIES,              // Utilities
    OTHER                   // Other/Unclassified
};

/**
 * Corporate credit rating information
 */
struct CreditRating {
    std::string agency;              // S&P, Moody's, Fitch
    std::string rating;              // AAA, Baa1, BB+, etc.
    std::string outlook;             // Stable, Positive, Negative, Watch
    Date rating_date;                // Date of rating assignment
    bool is_solicited;               // Solicited vs unsolicited rating
    
    CreditRating(const std::string& agency, const std::string& rating)
        : agency(agency), rating(rating), outlook("Stable"), is_solicited(true) {}
};

/**
 * Financial covenant tracking
 */
struct FinancialCovenant {
    enum class CovenantType {
        DEBT_TO_EBITDA,              // Total Debt/EBITDA
        EBITDA_TO_INTEREST,          // Interest Coverage Ratio
        FIXED_CHARGE_COVERAGE,       // Fixed Charge Coverage
        DEBT_TO_EQUITY,              // Debt-to-Equity Ratio
        TANGIBLE_NET_WORTH,          // Minimum TNW
        CURRENT_RATIO,               // Current Ratio
        DEBT_SERVICE_COVERAGE,       // DSCR
        LOAN_TO_VALUE,               // LTV for real estate
        CASH_FLOW_COVERAGE,          // Cash Flow Coverage
        CAPEX_LIMIT,                 // Capital Expenditure Limit
        DIVIDEND_RESTRICTION,        // Dividend Payment Restriction
        ACQUISITION_RESTRICTION,     // Acquisition Size Limit
        ASSET_SALE_RESTRICTION       // Asset Sale Restriction
    };
    
    CovenantType type;
    double threshold_value;          // Covenant threshold
    double current_value;            // Current actual value
    std::string measurement_period;  // Quarterly, Annual, TTM
    Date test_date;                  // Last test date
    bool is_maintenance;             // Maintenance vs incurrence covenant
    bool is_in_compliance;           // Current compliance status
    double cushion;                  // Cushion to covenant threshold
    
    FinancialCovenant(CovenantType type, double threshold, bool maintenance = true)
        : type(type), threshold_value(threshold), current_value(0.0),
          measurement_period("Quarterly"), is_maintenance(maintenance),
          is_in_compliance(true), cushion(0.0) {}
};

/**
 * Syndicate member information
 */
struct SyndicateMember {
    std::string institution_id;      // Unique institution identifier
    std::string institution_name;    // Institution name
    SyndicationRole role;            // Role in syndication
    Amount commitment_amount;        // Committed amount
    Amount funded_amount;            // Currently funded amount
    Rate all_in_yield;              // All-in yield/spread
    bool is_primary_market;          // Primary vs secondary market
    Date participation_date;         // Date joined syndication
    
    SyndicateMember(const std::string& id, const std::string& name, 
                   SyndicationRole role, Amount commitment)
        : institution_id(id), institution_name(name), role(role),
          commitment_amount(commitment), funded_amount(0.0),
          all_in_yield(0.0), is_primary_market(true) {}
};

/**
 * Interest rate benchmark information
 */
struct BenchmarkRate {
    enum class BenchmarkType {
        SOFR,                        // Secured Overnight Financing Rate
        LIBOR,                       // London Interbank Offered Rate (legacy)
        EURIBOR,                     // Euro Interbank Offered Rate
        PRIME_RATE,                  // Bank Prime Rate
        FEDERAL_FUNDS,               // Federal Funds Rate
        TREASURY_RATE,               // Treasury Rate
        SARON,                       // Swiss Average Rate Overnight
        SONIA,                       // Sterling Overnight Index Average
        TONAR,                       // Tokyo Overnight Average Rate
        CUSTOM                       // Custom benchmark
    };
    
    BenchmarkType type;
    std::string tenor;               // 1M, 3M, 6M, 12M
    Rate current_rate;               // Current benchmark rate
    Rate margin;                     // Credit spread/margin
    Rate floor_rate;                 // Interest rate floor
    Rate cap_rate;                   // Interest rate cap
    Date reset_date;                 // Next rate reset date
    std::string reset_frequency;     // Monthly, Quarterly, etc.
    
    BenchmarkRate(BenchmarkType type, const std::string& tenor, Rate margin)
        : type(type), tenor(tenor), current_rate(0.0), margin(margin),
          floor_rate(0.0), cap_rate(999.0), reset_frequency("Quarterly") {}
};

/**
 * Corporate borrower information
 */
struct CorporateBorrower {
    Obligor base_obligor;            // Base obligor information
    std::string legal_entity_id;     // LEI or similar identifier
    IndustryClassification industry; // Industry classification
    std::string country_of_risk;     // Country of incorporation/risk
    
    // Financial information
    Amount total_assets;             // Total assets
    Amount total_debt;               // Total debt outstanding
    Amount ebitda;                   // EBITDA (trailing twelve months)
    Amount revenue;                  // Total revenue (TTM)
    Amount cash_and_equivalents;     // Cash and cash equivalents
    Date financial_statement_date;   // Date of latest financials
    
    // Credit information
    std::vector<CreditRating> ratings; // Credit ratings from agencies
    std::string internal_risk_rating;  // Internal bank rating
    double probability_of_default;     // PD estimate
    Amount exposure_at_default;        // EAD
    double loss_given_default;         // LGD estimate
    
    // Corporate structure
    bool is_public_company;          // Publicly traded
    std::string ticker_symbol;       // Stock ticker (if public)
    std::string parent_company;      // Ultimate parent entity
    std::vector<std::string> subsidiaries; // Key subsidiaries
    
    CorporateBorrower(const Obligor& obligor, const std::string& lei, 
                     IndustryClassification industry)
        : base_obligor(obligor), legal_entity_id(lei), industry(industry),
          total_assets(0.0), total_debt(0.0), ebitda(0.0), revenue(0.0),
          cash_and_equivalents(0.0), probability_of_default(0.0),
          exposure_at_default(0.0), loss_given_default(0.0),
          is_public_company(false) {}
};

/**
 * Corporate loan implementation with syndication and covenant features
 */
class CorporateLoan {
private:
    std::string loan_id_;
    CorporateLoanType facility_type_;
    CorporateBorrower borrower_;
    Status status_;
    
    // Loan terms
    Amount facility_size_;           // Total facility commitment
    Amount outstanding_balance_;     // Currently outstanding
    Amount available_amount_;        // Available for drawdown
    BenchmarkRate benchmark_;        // Interest rate benchmark
    Date facility_agreement_date_;
    Date maturity_date_;
    Date commitment_expiry_date_;    // For revolving facilities
    
    // Syndication information
    std::vector<SyndicateMember> syndicate_;
    std::string administrative_agent_;
    Amount total_commitments_;
    bool is_syndicated_;
    
    // Financial covenants
    std::vector<FinancialCovenant> financial_covenants_;
    Date last_covenant_test_date_;
    bool is_covenant_compliant_;     // Overall compliance status
    std::optional<Date> covenant_cure_deadline_;
    
    // Fees and pricing
    Rate unused_fee_rate_;           // Fee on unused commitment
    Rate utilization_fee_rate_;      // Fee based on utilization level
    Amount arrangement_fee_;         // Upfront arrangement fee
    Amount commitment_fee_;          // Ongoing commitment fee
    std::vector<Amount> fee_payments_; // Historical fee payments
    
    // Drawdown and repayment tracking  
    struct DrawdownRecord {
        Date drawdown_date;
        Amount drawdown_amount;
        std::string purpose;         // Purpose of drawdown
        Rate applicable_margin;      // Margin at drawdown
    };
    std::vector<DrawdownRecord> drawdown_history_;
    
    struct RepaymentRecord {
        Date repayment_date;
        Amount repayment_amount;
        bool is_mandatory;           // Mandatory vs optional prepayment
        Amount prepayment_penalty;   // Penalty if applicable
    };
    std::vector<RepaymentRecord> repayment_history_;
    
    // Security and guarantees
    enum class SecurityType {
        UNSECURED,
        SENIOR_SECURED,
        SUBORDINATED,
        MEZZANINE
    };
    SecurityType security_type_;
    std::vector<std::string> collateral_description_;
    std::vector<std::string> guarantors_;
    
    // Performance and risk metrics
    int days_past_due_;
    Amount accrued_interest_;
    Amount accrued_fees_;
    Date last_payment_date_;
    double current_risk_rating_;

public:
    CorporateLoan(const std::string& id, CorporateLoanType type,
                  const CorporateBorrower& borrower, Amount facility_size,
                  const BenchmarkRate& benchmark, Date agreement_date, Date maturity);
    
    // Core asset interface
    void makePayment(Balance payment_amount, Date payment_date);
    void applyInterestAccrual(Date accrual_date);
    void applyDefault(Date default_date);
    void applyPrepayment(Balance amount, Date payment_date);
    Balance getCurrentBalance() const;
    Rate getCurrentRate() const;
    void resetToOriginal();
    
    // Corporate loan specific operations
    void drawdown(Amount amount, Date drawdown_date, const std::string& purpose = "");
    void repay(Amount amount, Date repayment_date, bool is_mandatory = false);
    void revolve(Amount repay_amount, Amount redraw_amount, Date transaction_date);
    
    // Syndication management
    void addSyndicateMember(const SyndicateMember& member);
    void removeSyndicateMember(const std::string& institution_id);
    void updateMemberCommitment(const std::string& institution_id, Amount new_commitment);
    Amount calculateMemberAllocation(const std::string& institution_id, Amount transaction_amount) const;
    
    // Covenant tracking and testing
    void addFinancialCovenant(const FinancialCovenant& covenant);
    void updateCovenantValue(FinancialCovenant::CovenantType type, double new_value, Date test_date);
    bool testAllCovenants(Date test_date);
    std::vector<FinancialCovenant> getBreachedCovenants() const;
    void waiveCovenantBreach(FinancialCovenant::CovenantType type, Date waiver_date);
    
    // Interest rate management
    void resetInterestRate(Date reset_date);
    void updateBenchmarkRate(Rate new_benchmark_rate, Date effective_date);
    Rate calculateAllInRate() const;
    
    // Fee calculations and processing
    Amount calculateUnusedFee(Date calculation_date) const;
    Amount calculateUtilizationFee(Date calculation_date) const;
    void chargeCommitmentFee(Date fee_date);
    void payArrangementFee(Amount fee_amount, Date payment_date);
    
    // Financial reporting and analytics
    double getUtilizationRate() const;
    Amount getAvailableCommitment() const;
    double getCurrentMargin() const;
    Amount calculatePresentValue(Rate discount_rate) const;
    double calculateDuration() const;
    
    // Risk management
    void updateRiskRating(double new_rating, Date rating_date);
    double calculateEconomicCapital() const;
    Amount calculateExpectedLoss() const;
    bool isWatchListCredit() const;
    bool isNonPerformingLoan() const;
    
    // Documentation and legal
    void amendFacility(const std::string& amendment_description, Date amendment_date);
    void extendMaturity(Date new_maturity_date);
    void increaseCommitment(Amount additional_commitment, Date effective_date);
    
    // Getters
    const std::string& getLoanId() const { return loan_id_; }
    CorporateLoanType getFacilityType() const { return facility_type_; }
    const CorporateBorrower& getBorrower() const { return borrower_; }
    Status getStatus() const { return status_; }
    
    Amount getFacilitySize() const { return facility_size_; }
    Amount getOriginBalance() const { return facility_size_; }  // For Pool template compatibility
    Amount getOutstandingBalance() const { return outstanding_balance_; }
    Amount getAvailableAmount() const { return available_amount_; }
    const BenchmarkRate& getBenchmark() const { return benchmark_; }
    Date getMaturityDate() const { return maturity_date_; }
    
    const std::vector<SyndicateMember>& getSyndicate() const { return syndicate_; }
    const std::string& getAdministrativeAgent() const { return administrative_agent_; }
    bool isSyndicated() const { return is_syndicated_; }
    
    const std::vector<FinancialCovenant>& getCovenants() const { return financial_covenants_; }
    bool isCovenantCompliant() const { return is_covenant_compliant_; }
    
    Rate getUnusedFeeRate() const { return unused_fee_rate_; }
    Rate getUtilizationFeeRate() const { return utilization_fee_rate_; }
    Amount getArrangementFee() const { return arrangement_fee_; }
    
    int getDaysPastDue() const { return days_past_due_; }
    Amount getAccruedInterest() const { return accrued_interest_; }
    Amount getAccruedFees() const { return accrued_fees_; }
    Date getLastPaymentDate() const { return last_payment_date_; }
    double getCurrentRiskRating() const { return current_risk_rating_; }
    
    // Setters
    void setStatus(Status status) { status_ = status; }
    void setAdministrativeAgent(const std::string& agent) { administrative_agent_ = agent; }
    void setUnusedFeeRate(Rate rate) { unused_fee_rate_ = rate; }
    void setUtilizationFeeRate(Rate rate) { utilization_fee_rate_ = rate; }
    void updateBorrowerFinancials(Amount assets, Amount debt, Amount ebitda, Date as_of_date);
};

// Utility functions
std::string corporateLoanTypeToString(CorporateLoanType type);
std::string syndicationRoleToString(SyndicationRole role);
std::string industryClassificationToString(IndustryClassification industry);
std::string benchmarkTypeToString(BenchmarkRate::BenchmarkType type);
std::string covenantTypeToString(FinancialCovenant::CovenantType type);

// Industry analysis functions
std::vector<IndustryClassification> getHighRiskIndustries();
std::vector<IndustryClassification> getCyclicalIndustries();
double getIndustryDefaultRate(IndustryClassification industry, int year);

// Covenant calculation helpers
double calculateDebtToEBITDA(Amount total_debt, Amount ebitda);
double calculateInterestCoverage(Amount ebitda, Amount interest_expense);
double calculateFixedChargeCoverage(Amount ebitda, Amount fixed_charges);

} // namespace Structura