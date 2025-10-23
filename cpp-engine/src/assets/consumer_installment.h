#pragma once

#include "assets/asset_base.h"
#include <memory>
#include <optional>
#include <vector>
#include <map>

namespace Structura {

/**
 * Consumer installment loan types
 */
enum class ConsumerInstallmentType {
    PERSONAL_LOAN,            // Unsecured personal loan
    DEBT_CONSOLIDATION,       // Debt consolidation loan
    HOME_IMPROVEMENT,         // Home improvement financing
    MEDICAL_FINANCING,        // Medical/healthcare financing
    WEDDING_LOAN,             // Wedding financing
    VACATION_LOAN,            // Travel/vacation financing
    MAJOR_PURCHASE,           // Large purchase financing
    EDUCATIONAL_EXPENSES,     // Non-federal education expenses
    APPLIANCE_FINANCING,      // Appliance/electronics financing
    FURNITURE_FINANCING,      // Furniture/home goods financing
    PAYDAY_LOAN,              // Short-term payday loan
    TITLE_LOAN,               // Auto title loan
    PEER_TO_PEER,             // P2P marketplace loan
    POINT_OF_SALE,            // Point-of-sale financing (BNPL)
    CRYPTOCURRENCY_BACKED     // Crypto-collateralized loan
};

/**
 * Credit score ranges for risk assessment
 */
enum class CreditScoreRange {
    SUPER_PRIME,              // 781-850
    PRIME,                    // 661-780
    NEAR_PRIME,               // 601-660
    SUBPRIME,                 // 501-600
    DEEP_SUBPRIME,            // 300-500
    NO_SCORE,                 // No credit history
    THIN_FILE                 // Limited credit history
};

/**
 * Employment verification levels
 */
enum class EmploymentVerification {
    FULL_VERIFICATION,        // Income and employment verified
    STATED_INCOME_VERIFIED_ASSETS, // SIVA
    STATED_INCOME_STATED_ASSETS,   // SISA
    NO_INCOME_VERIFICATION,   // NIV
    BANK_STATEMENT_ONLY,      // Bank statement verification
    ALTERNATIVE_DOCUMENTATION, // Alternative income documentation
    SELF_EMPLOYED,            // Self-employed borrower
    RETIRED,                  // Retired borrower
    STUDENT,                  // Student borrower
    UNEMPLOYED                // Currently unemployed
};

/**
 * Loan purpose categories for compliance and risk
 */
enum class LoanPurpose {
    DEBT_CONSOLIDATION_PURPOSE,  // Consolidating existing debt
    HOME_IMPROVEMENT_PURPOSE,    // Home renovations/improvements
    MAJOR_PURCHASE_PURPOSE,      // Large consumer purchase
    EMERGENCY_EXPENSES,          // Emergency/unexpected expenses
    BUSINESS_EXPENSES,           // Business-related expenses
    INVESTMENT,                  // Investment purposes
    EDUCATION,                   // Educational expenses
    MEDICAL_EXPENSES,            // Medical/healthcare costs
    VACATION_TRAVEL,             // Vacation/travel expenses
    WEDDING_EXPENSES,            // Wedding-related costs
    MOVING_RELOCATION,           // Moving/relocation costs
    OTHER_PURPOSE                // Other specified purpose
};

/**
 * Consumer borrower information with enhanced profiling
 */
struct ConsumerBorrower {
    Obligor base_obligor;        // Base obligor information
    
    // Personal information
    Date date_of_birth;          // Date of birth
    std::string ssn_last_four;   // Last 4 digits of SSN
    std::string phone_number;    // Primary phone number
    std::string email_address;   // Email address
    
    // Address information
    std::string current_address; // Current residential address
    Date address_since;          // How long at current address
    std::string housing_status;  // Own, Rent, Live with family
    Amount monthly_housing_payment; // Rent/mortgage payment
    
    // Employment information
    std::string employer_name;   // Current employer
    std::string job_title;       // Job title/position
    Date employment_start_date;  // Employment start date
    EmploymentVerification employment_verification; // Verification level
    Amount monthly_income;       // Monthly gross income
    Amount other_income;         // Other income sources
    
    // Credit information
    int credit_score;            // Primary credit score (FICO)
    CreditScoreRange score_range; // Credit score category
    Date credit_report_date;     // Date of credit report
    int number_of_inquiries;     // Recent credit inquiries
    int number_of_accounts;      // Total number of accounts
    Amount total_debt;           // Total outstanding debt
    Amount monthly_debt_payments; // Monthly debt service
    int months_since_most_recent_inq; // Months since last inquiry
    
    // Banking information
    std::string bank_name;       // Primary bank
    std::string account_type;    // Checking, Savings, etc.
    Date account_open_date;      // Account opening date
    Amount average_balance;      // Average account balance
    
    ConsumerBorrower(const Obligor& obligor, Date dob, const std::string& ssn)
        : base_obligor(obligor), date_of_birth(dob), ssn_last_four(ssn),
          housing_status("Rent"), monthly_housing_payment(0.0),
          employment_verification(EmploymentVerification::FULL_VERIFICATION),
          monthly_income(0.0), other_income(0.0), credit_score(700),
          score_range(CreditScoreRange::PRIME), number_of_inquiries(0),
          number_of_accounts(0), total_debt(0.0), monthly_debt_payments(0.0),
          months_since_most_recent_inq(0), account_type("Checking"),
          average_balance(0.0) {}
};

/**
 * Promotional rate information for point-of-sale financing
 */
struct PromotionalRateInfo {
    Rate promotional_rate;       // Promotional interest rate (often 0%)
    int promotional_term_months; // Length of promotional period
    Rate regular_rate;           // Rate after promotional period
    bool is_deferred_interest;   // Deferred vs reduced interest
    std::string promotion_type;  // "0% APR", "No Interest if Paid in Full"
    Date promotion_start_date;   // Promotion start date
    Date promotion_end_date;     // Promotion end date
    
    PromotionalRateInfo(Rate promo_rate, int promo_months, Rate regular_rate)
        : promotional_rate(promo_rate), promotional_term_months(promo_months),
          regular_rate(regular_rate), is_deferred_interest(false),
          promotion_type("Promotional Rate") {}
};

/**
 * Underwriting decision tracking
 */
struct UnderwritingDecision {
    enum class DecisionType {
        APPROVED,                // Fully approved
        APPROVED_WITH_CONDITIONS, // Approved with conditions
        COUNTEROFFER,            // Counteroffer (different terms)
        DECLINED,                // Application declined
        PENDING,                 // Decision pending
        WITHDRAWN                // Application withdrawn
    };
    
    DecisionType decision;       // Underwriting decision
    std::string decision_reason; // Reason for decision
    Date decision_date;          // Date of decision
    std::string underwriter_id;  // Underwriter identifier
    double risk_score;           // Internal risk score
    std::vector<std::string> conditions; // Approval conditions
    Amount approved_amount;      // Approved loan amount
    Rate approved_rate;          // Approved interest rate
    int approved_term;           // Approved term in months
    
    UnderwritingDecision(DecisionType decision, const std::string& reason)
        : decision(decision), decision_reason(reason), risk_score(0.0),
          approved_amount(0.0), approved_rate(0.0), approved_term(0) {}
};

/**
 * Consumer installment loan implementation
 */
class ConsumerInstallment {
private:
    std::string loan_id_;
    ConsumerInstallmentType loan_type_;
    LoanPurpose loan_purpose_;
    ConsumerBorrower borrower_;
    Status status_;
    
    // Loan terms
    Amount original_principal_;
    Balance current_principal_;
    Balance accrued_interest_;
    Rate original_interest_rate_;
    Rate current_interest_rate_;
    int original_term_months_;
    int remaining_term_months_;
    Amount scheduled_payment_;
    
    // Dates
    Date origination_date_;
    Date first_payment_date_;
    Date maturity_date_;
    Date next_payment_date_;
    
    // Promotional terms (if applicable)
    std::optional<PromotionalRateInfo> promotional_info_;
    bool is_in_promotional_period_;
    
    // Payment tracking
    int payments_made_;
    int consecutive_on_time_payments_;
    int missed_payments_;
    Amount total_payments_made_;
    Date last_payment_date_;
    Amount last_payment_amount_;
    
    // Delinquency and default
    int days_delinquent_;
    Date first_delinquent_date_;
    Amount late_fees_assessed_;
    Amount collection_costs_;
    bool is_in_hardship_program_;
    
    // Underwriting information
    UnderwritingDecision underwriting_decision_;
    double debt_to_income_ratio_;
    double payment_to_income_ratio_;
    Amount monthly_disposable_income_;
    
    // Servicing information
    std::string servicer_name_;
    std::string loan_officer_id_;
    Date last_contact_date_;
    std::string contact_method_;   // Phone, Email, Mail, etc.
    
    // Refinancing and modification
    bool is_refinanced_;
    std::string original_loan_id_; // If this is a refinance
    bool is_modified_;
    Date modification_date_;
    std::string modification_reason_;
    
    // Point-of-sale specific
    std::string merchant_name_;    // For POS financing
    std::string purchase_category_; // Electronics, Furniture, etc.
    Amount down_payment_;          // Down payment amount
    
public:
    ConsumerInstallment(const std::string& id, ConsumerInstallmentType type,
                       LoanPurpose purpose, const ConsumerBorrower& borrower,
                       Amount principal, Rate interest_rate, int term_months,
                       Date origination_date);
    
    // Core asset interface
    void makePayment(Balance payment_amount, Date payment_date);
    void applyInterestAccrual(Date accrual_date);
    void applyDefault(Date default_date);
    void applyPrepayment(Balance amount, Date payment_date);
    Balance getCurrentBalance() const;
    Rate getCurrentRate() const;
    void resetToOriginal();
    
    // Consumer installment specific operations
    void processRegularPayment(Amount payment_amount, Date payment_date);
    void processPartialPayment(Amount payment_amount, Date payment_date);
    void applyLateFee(Amount fee_amount, Date fee_date);
    void enterHardshipProgram(Date start_date, const std::string& program_type);
    void exitHardshipProgram(Date exit_date);
    
    // Promotional rate management
    void setPromotionalRate(const PromotionalRateInfo& promo_info);
    void endPromotionalPeriod(Date end_date);
    bool isInPromotionalPeriod() const { return is_in_promotional_period_; }
    Amount calculateDeferredInterest() const;
    
    // Refinancing and modification
    void refinanceLoan(const std::string& new_loan_id, Amount new_principal,
                      Rate new_rate, int new_term, Date refinance_date);
    void modifyLoan(Amount new_payment, Rate new_rate, Date modification_date,
                   const std::string& reason);
    
    // Risk assessment and scoring
    double calculateRiskScore() const;
    double calculateProbabilityOfDefault() const;
    bool isHighRiskLoan() const;
    double getDebtToIncomeRatio() const { return debt_to_income_ratio_; }
    double getPaymentToIncomeRatio() const { return payment_to_income_ratio_; }
    
    // Delinquency management
    void updateDelinquencyStatus(Date as_of_date);
    void enterDefault(Date default_date);
    void chargeOff(Amount charge_off_amount, Date charge_off_date);
    void processRecovery(Amount recovery_amount, Date recovery_date);
    
    // Payment calculations
    Amount calculateMonthlyPayment() const;
    Amount calculatePayoffAmount(Date payoff_date) const;
    Amount calculateRemainingInterest() const;
    std::vector<Amount> generateAmortizationSchedule() const;
    Date calculatePayoffDate() const;
    
    // Customer service and communication
    void recordCustomerContact(Date contact_date, const std::string& method,
                              const std::string& notes);
    void updateContactInformation(const std::string& phone, const std::string& email,
                                 const std::string& address);
    void sendNotification(const std::string& notification_type, Date send_date);
    
    // Compliance and reporting
    bool isQualifiedMortgage() const; // For certain home improvement loans
    Amount calculateAPR() const;
    std::vector<std::string> getComplianceFlags() const;
    bool requiresTILADisclosure() const;
    
    // Performance metrics
    double getSeasoningMonths() const;
    bool isEarlyPayoffCandidate() const;
    Amount getLifetimeInterestPaid() const;
    double getYieldToMaturity() const;
    
    // Getters
    const std::string& getLoanId() const { return loan_id_; }
    ConsumerInstallmentType getLoanType() const { return loan_type_; }
    LoanPurpose getLoanPurpose() const { return loan_purpose_; }
    const ConsumerBorrower& getBorrower() const { return borrower_; }
    Status getStatus() const { return status_; }
    
    Amount getOriginalPrincipal() const { return original_principal_; }
    Balance getCurrentPrincipal() const { return current_principal_; }
    Balance getAccruedInterest() const { return accrued_interest_; }
    Rate getOriginalInterestRate() const { return original_interest_rate_; }
    Rate getCurrentInterestRate() const { return current_interest_rate_; }
    int getOriginalTermMonths() const { return original_term_months_; }
    int getRemainingTermMonths() const { return remaining_term_months_; }
    Amount getScheduledPayment() const { return scheduled_payment_; }
    
    Date getOriginationDate() const { return origination_date_; }
    Date getFirstPaymentDate() const { return first_payment_date_; }
    Date getMaturityDate() const { return maturity_date_; }
    Date getNextPaymentDate() const { return next_payment_date_; }
    
    const std::optional<PromotionalRateInfo>& getPromotionalInfo() const { return promotional_info_; }
    
    int getPaymentsMade() const { return payments_made_; }
    int getConsecutiveOnTimePayments() const { return consecutive_on_time_payments_; }
    int getMissedPayments() const { return missed_payments_; }
    Amount getTotalPaymentsMade() const { return total_payments_made_; }
    Date getLastPaymentDate() const { return last_payment_date_; }
    
    int getDaysDelinquent() const { return days_delinquent_; }
    Amount getLateFees() const { return late_fees_assessed_; }
    Amount getCollectionCosts() const { return collection_costs_; }
    bool isInHardshipProgram() const { return is_in_hardship_program_; }
    
    const UnderwritingDecision& getUnderwritingDecision() const { return underwriting_decision_; }
    Amount getMonthlyDisposableIncome() const { return monthly_disposable_income_; }
    
    const std::string& getServicerName() const { return servicer_name_; }
    const std::string& getLoanOfficerId() const { return loan_officer_id_; }
    
    bool isRefinanced() const { return is_refinanced_; }
    const std::string& getOriginalLoanId() const { return original_loan_id_; }
    bool isModified() const { return is_modified_; }
    
    const std::string& getMerchantName() const { return merchant_name_; }
    const std::string& getPurchaseCategory() const { return purchase_category_; }
    Amount getDownPayment() const { return down_payment_; }
    
    // Setters
    void setStatus(Status status) { status_ = status; }
    void setCurrentRate(Rate rate) { current_interest_rate_ = rate; }
    void setServicer(const std::string& servicer_name) { servicer_name_ = servicer_name; }
    void setMerchantInfo(const std::string& merchant_name, const std::string& category);
    void updateBorrowerIncome(Amount monthly_income, Amount other_income);
    void updateCreditScore(int new_score, Date score_date);
};

// Utility functions
std::string consumerInstallmentTypeToString(ConsumerInstallmentType type);
std::string creditScoreRangeToString(CreditScoreRange range);
std::string employmentVerificationToString(EmploymentVerification verification);
std::string loanPurposeToString(LoanPurpose purpose);
std::string underwritingDecisionToString(UnderwritingDecision::DecisionType decision);

// Credit scoring and risk assessment functions
CreditScoreRange categorizeScore(int credit_score);
double calculateBasePricingRate(CreditScoreRange score_range, ConsumerInstallmentType loan_type);
double estimateDefaultProbability(int credit_score, double debt_to_income, int term_months);

// Regulatory and compliance functions
bool requiresCFPBCompliance(ConsumerInstallmentType loan_type, Amount loan_amount);
std::vector<std::string> getRequiredDisclosures(ConsumerInstallmentType loan_type, LoanPurpose purpose);
bool isAbilityToRepayCompliant(const ConsumerBorrower& borrower, Amount payment, Amount income);

} // namespace Structura