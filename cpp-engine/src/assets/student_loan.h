#pragma once

#include "assets/asset_base.h"
#include <memory>
#include <optional>
#include <vector>
#include <chrono>

namespace Structura {

/**
 * Student loan program types
 */
enum class StudentLoanProgram {
    FEDERAL_DIRECT_SUBSIDIZED,     // Federal Direct Subsidized Loans
    FEDERAL_DIRECT_UNSUBSIDIZED,   // Federal Direct Unsubsidized Loans
    FEDERAL_DIRECT_PLUS_PARENT,    // Federal Direct PLUS Loans (Parent)
    FEDERAL_DIRECT_PLUS_GRAD,      // Federal Direct PLUS Loans (Graduate)
    FEDERAL_PERKINS,               // Federal Perkins Loans (discontinued)
    FEDERAL_FFEL,                  // Federal Family Education Loan (discontinued)
    PRIVATE_FIXED_RATE,            // Private loans with fixed rates
    PRIVATE_VARIABLE_RATE,         // Private loans with variable rates
    STATE_SPONSORED,               // State-sponsored loan programs
    INSTITUTIONAL                  // Institutional/school-based loans
};

/**
 * Student loan repayment plans
 */
enum class RepaymentPlan {
    STANDARD,                      // Fixed 10-year payments
    EXTENDED,                      // Extended payments up to 25 years
    GRADUATED,                     // Payments start low and increase
    INCOME_DRIVEN_IBR,            // Income-Based Repayment
    INCOME_DRIVEN_PAYE,           // Pay As You Earn
    INCOME_DRIVEN_REPAYE,         // Revised Pay As You Earn
    INCOME_DRIVEN_ICR,            // Income-Contingent Repayment
    INTEREST_ONLY,                // Interest-only payments (temporary)
    CUSTOM                        // Custom payment arrangement
};

/**
 * Student loan status types
 */
enum class StudentLoanStatus {
    IN_SCHOOL,                    // Student is enrolled in school
    GRACE_PERIOD,                 // Grace period after graduation
    REPAYMENT,                    // Standard repayment period
    DEFERMENT,                    // Payment temporarily postponed
    FORBEARANCE,                  // Payment reduced or postponed
    DEFAULT,                      // Loan is in default
    PAID_OFF,                     // Loan fully paid
    DISCHARGED,                   // Loan discharged (bankruptcy, disability, etc.)
    FORGIVEN                      // Loan forgiven (PSLF, teacher forgiveness, etc.)
};

/**
 * Deferment/Forbearance reasons
 */
enum class DefermentReason {
    IN_SCHOOL_DEFERMENT,          // Student enrolled at least half-time
    UNEMPLOYMENT_DEFERMENT,       // Unemployment or unable to find work
    ECONOMIC_HARDSHIP_DEFERMENT,  // Economic hardship
    MILITARY_DEFERMENT,           // Military service
    POST_BACCALAUREATE_DEFERMENT, // Post-baccalaureate internship
    REHABILITATION_DEFERMENT,     // Rehabilitation training
    OTHER_DEFERMENT,              // Other qualified deferment
    GENERAL_FORBEARANCE,          // General forbearance (discretionary)
    MANDATORY_FORBEARANCE,        // Mandatory forbearance
    MILITARY_FORBEARANCE,         // Military forbearance
    DISASTER_FORBEARANCE          // Disaster-related forbearance
};

/**
 * Educational institution information
 */
struct EducationalInstitution {
    std::string school_code;      // Federal school code
    std::string school_name;      // Institution name
    std::string school_type;      // Public, Private, For-profit
    std::string degree_type;      // Associate, Bachelor's, Master's, Doctoral, etc.
    std::string program_of_study; // Major/program of study
    Date enrollment_begin;        // Enrollment start date
    Date enrollment_end;          // Enrollment end date (actual or expected)
    bool is_eligible_institution; // Title IV eligible institution
    
    EducationalInstitution(const std::string& code, const std::string& name, 
                          const std::string& type, const std::string& degree)
        : school_code(code), school_name(name), school_type(type), 
          degree_type(degree), is_eligible_institution(true) {}
};

/**
 * Student borrower information
 */
struct StudentBorrower {
    Obligor base_obligor;         // Base obligor information
    Date date_of_birth;           // Date of birth
    std::string ssn_last_four;    // Last 4 digits of SSN
    std::string enrollment_status; // Full-time, Half-time, etc.
    double expected_family_contribution; // EFC from FAFSA
    bool is_dependent_student;    // Dependency status
    std::optional<Obligor> parent_borrower; // Parent information for PLUS loans
    
    // Financial information
    Amount annual_income;         // Current annual income
    Amount discretionary_income;  // Discretionary income for IDR plans
    int family_size;             // Family size for IDR calculations
    Date income_certification_date; // Last income certification
    
    StudentBorrower(const Obligor& obligor, Date dob, const std::string& ssn)
        : base_obligor(obligor), date_of_birth(dob), ssn_last_four(ssn),
          is_dependent_student(true), annual_income(0.0), 
          discretionary_income(0.0), family_size(1) {}
};

/**
 * Loan servicer information
 */
struct LoanServicer {
    std::string servicer_id;      // Unique servicer identifier
    std::string servicer_name;    // Servicer company name
    std::string contact_info;     // Contact information
    Date servicing_start_date;    // When servicing began
    Date last_contact_date;       // Last borrower contact
    std::vector<std::string> services_provided; // Payment processing, counseling, etc.
    
    LoanServicer(const std::string& id, const std::string& name)
        : servicer_id(id), servicer_name(name) {}
};

/**
 * Income-driven repayment calculation helper
 */
struct IDRCalculation {
    Amount monthly_payment;       // Calculated monthly payment
    Amount annual_payment_cap;    // Maximum annual payment
    Date recertification_due;     // Next income recertification due
    double payment_cap_percentage; // Percentage of discretionary income
    bool is_partial_financial_hardship; // PFH status
    
    IDRCalculation() : monthly_payment(0.0), annual_payment_cap(0.0), 
                      payment_cap_percentage(0.0), is_partial_financial_hardship(false) {}
};

/**
 * Student loan implementation with comprehensive federal and private loan features
 */
class StudentLoan {
private:
    std::string loan_id_;
    StudentLoanProgram program_type_;
    StudentBorrower borrower_;
    EducationalInstitution institution_;
    LoanServicer servicer_;
    StudentLoanStatus status_;
    RepaymentPlan repayment_plan_;
    
    // Loan financial terms
    Amount original_principal_;
    Balance current_principal_;
    Balance accrued_interest_;
    Balance capitalized_interest_;
    Rate current_interest_rate_;
    Rate original_interest_rate_;
    
    // Federal loan specific
    bool is_subsidized_;          // Government pays interest during school
    Amount origination_fee_;      // Loan origination fee
    Amount loan_fee_;            // Other fees
    
    // Repayment terms
    Date first_disbursement_date_;
    Date grace_period_end_;
    Date repayment_begin_date_;
    Date scheduled_payoff_date_;
    int original_repayment_term_months_;
    int remaining_term_months_;
    Amount scheduled_monthly_payment_;
    
    // Payment history
    int payments_made_;
    int consecutive_on_time_payments_;
    Amount total_payments_made_;
    Date last_payment_date_;
    Amount last_payment_amount_;
    
    // Deferment/Forbearance tracking
    std::optional<DefermentReason> current_deferment_reason_;
    Date deferment_forbearance_start_;
    Date deferment_forbearance_end_;
    int total_deferment_months_;
    int total_forbearance_months_;
    bool interest_accrues_during_deferment_;
    
    // Income-driven repayment
    std::optional<IDRCalculation> idr_calculation_;
    Date last_income_certification_;
    bool is_eligible_for_forgiveness_;
    int qualifying_payments_for_forgiveness_;
    
    // Default and collections
    Date first_delinquent_date_;
    int days_delinquent_;
    Amount collection_costs_;
    bool is_in_rehabilitation_;
    int rehabilitation_payments_made_;
    
    // Public Service Loan Forgiveness (PSLF)
    bool is_pslf_eligible_;
    int pslf_qualifying_payments_;
    std::string employer_type_;   // Government, Non-profit, etc.

public:
    StudentLoan(const std::string& id, StudentLoanProgram program,
                const StudentBorrower& borrower, const EducationalInstitution& institution,
                const LoanServicer& servicer, Amount principal, Rate interest_rate,
                Date disbursement_date);
    
    // Core asset interface methods
    void makePayment(Balance payment_amount, Date payment_date);
    void applyInterestAccrual(Date accrual_date);
    void applyDefault(Date default_date);
    void applyPrepayment(Balance amount, Date payment_date);
    Balance getCurrentBalance() const;
    Rate getCurrentRate() const;
    void resetToOriginal();
    
    // Student loan specific payment processing
    void processStandardPayment(Amount payment_amount, Date payment_date);
    void processIDRPayment(Date payment_date);
    void processInterestOnlyPayment(Date payment_date);
    void capitalizeAccruedInterest(Date capitalization_date);
    
    // Deferment and forbearance
    void enterDeferment(DefermentReason reason, Date start_date, Date end_date);
    void enterForbearance(DefermentReason reason, Date start_date, Date end_date);
    void exitDefermentForbearance(Date exit_date);
    bool isInDefermentOrForbearance() const;
    bool canEnterDeferment(DefermentReason reason) const;
    
    // Repayment plan management
    void changeRepaymentPlan(RepaymentPlan new_plan, Date effective_date);
    Amount calculateMonthlyPayment(RepaymentPlan plan) const;
    void recertifyIncome(Amount new_annual_income, int new_family_size, Date cert_date);
    IDRCalculation calculateIDRPayment() const;
    
    // Default and rehabilitation
    void enterDefault(Date default_date);
    void enterRehabilitation(Date start_date);
    void makeRehabilitationPayment(Amount payment_amount, Date payment_date);
    void completeRehabilitation(Date completion_date);
    bool isEligibleForRehabilitation() const;
    
    // Forgiveness and discharge
    void applyLoanForgiveness(Amount forgiven_amount, Date forgiveness_date, 
                             const std::string& forgiveness_program);
    void applyLoanDischarge(Amount discharged_amount, Date discharge_date,
                           const std::string& discharge_reason);
    bool isEligibleForPSLF() const;
    void updatePSLFStatus(const std::string& employer_type, Date employment_start);
    
    // Interest rate changes (for variable rate loans)
    void updateInterestRate(Rate new_rate, Date effective_date);
    std::vector<Rate> getInterestRateHistory() const;
    
    // Servicer management
    void transferToNewServicer(const LoanServicer& new_servicer, Date transfer_date);
    void updateServicerContact(Date contact_date, const std::string& contact_type);
    
    // Financial calculations
    Amount calculateRemainingInterest() const;
    Amount calculateTotalCostOfLoan() const;
    Date calculatePayoffDate() const;
    Amount calculatePayoffAmount(Date payoff_date) const;
    
    // Qualification and eligibility checks
    bool isEligibleForConsolidation() const;
    bool isEligibleForIDR() const;
    bool qualifiesForSubsidyBenefits() const;
    
    // Performance and risk metrics
    double getPaymentToIncomeRatio() const;
    int getCurrentDelinquencyDays() const;
    double getProbabilityOfDefault() const;
    bool isPerformingLoan() const;
    
    // Getters
    const std::string& getLoanId() const { return loan_id_; }
    StudentLoanProgram getProgramType() const { return program_type_; }
    const StudentBorrower& getBorrower() const { return borrower_; }
    const EducationalInstitution& getInstitution() const { return institution_; }
    const LoanServicer& getServicer() const { return servicer_; }
    StudentLoanStatus getStatus() const { return status_; }
    RepaymentPlan getRepaymentPlan() const { return repayment_plan_; }
    
    Amount getOriginalPrincipal() const { return original_principal_; }
    Balance getCurrentPrincipal() const { return current_principal_; }
    Balance getAccruedInterest() const { return accrued_interest_; }
    Balance getCapitalizedInterest() const { return capitalized_interest_; }
    Rate getCurrentInterestRate() const { return current_interest_rate_; }
    Rate getOriginalInterestRate() const { return original_interest_rate_; }
    
    bool isSubsidized() const { return is_subsidized_; }
    Amount getOriginationFee() const { return origination_fee_; }
    Date getFirstDisbursementDate() const { return first_disbursement_date_; }
    Date getGracePeriodEnd() const { return grace_period_end_; }
    Date getRepaymentBeginDate() const { return repayment_begin_date_; }
    Amount getScheduledMonthlyPayment() const { return scheduled_monthly_payment_; }
    
    int getPaymentsMade() const { return payments_made_; }
    int getConsecutiveOnTimePayments() const { return consecutive_on_time_payments_; }
    Date getLastPaymentDate() const { return last_payment_date_; }
    
    int getTotalDefermentMonths() const { return total_deferment_months_; }
    int getTotalForbearanceMonths() const { return total_forbearance_months_; }
    
    bool isPSLFEligible() const { return is_pslf_eligible_; }
    int getPSLFQualifyingPayments() const { return pslf_qualifying_payments_; }
    const std::string& getEmployerType() const { return employer_type_; }
    
    // Setters
    void setStatus(StudentLoanStatus status) { status_ = status; }
    void setCurrentRate(Rate rate) { current_interest_rate_ = rate; }
    void updateBorrowerIncome(Amount annual_income, int family_size);
    void setServicer(const LoanServicer& servicer) { servicer_ = servicer; }
};

// Utility functions
std::string studentLoanProgramToString(StudentLoanProgram program);
std::string repaymentPlanToString(RepaymentPlan plan);
std::string studentLoanStatusToString(StudentLoanStatus status);
std::string defermentReasonToString(DefermentReason reason);

// Federal loan limits and calculations
Amount getFederalLoanLimit(StudentLoanProgram program, int academic_year, 
                          bool is_dependent, int grade_level);
Rate getCurrentFederalInterestRate(StudentLoanProgram program, int academic_year);
Amount calculateOriginationFee(StudentLoanProgram program, Amount loan_amount, 
                              int academic_year);

} // namespace Structura