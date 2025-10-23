#pragma once

#include "assets/asset_base.h"
#include <memory>
#include <optional>
#include <vector>
#include <map>

namespace Structura {

/**
 * Receivable types for different trade finance scenarios
 */
enum class ReceivableType {
    TRADE_RECEIVABLE,         // Standard trade receivables
    INVOICE_FACTORING,        // Factored invoices
    ACCOUNTS_RECEIVABLE,      // General A/R financing
    SUPPLY_CHAIN_FINANCE,     // Supply chain financing
    EXPORT_RECEIVABLES,       // Export trade receivables
    GOVERNMENT_RECEIVABLES,   // Government contract receivables
    MEDICAL_RECEIVABLES,      // Healthcare receivables
    LEGAL_RECEIVABLES,        // Legal services receivables
    CONSTRUCTION_RECEIVABLES, // Construction progress billing
    FREIGHT_RECEIVABLES,      // Transportation/logistics
    ENERGY_RECEIVABLES,       // Utility/energy receivables
    RETAIL_RECEIVABLES,       // Retail/consumer receivables
    ROYALTY_RECEIVABLES,      // Intellectual property royalties
    OTHER_RECEIVABLES         // Other specialized receivables
};

/**
 * Collection terms and payment patterns
 */
enum class PaymentTerms {
    NET_10,                   // Payment due in 10 days
    NET_15,                   // Payment due in 15 days
    NET_30,                   // Payment due in 30 days
    NET_45,                   // Payment due in 45 days
    NET_60,                   // Payment due in 60 days
    NET_90,                   // Payment due in 90 days
    NET_120,                  // Payment due in 120 days
    CASH_ON_DELIVERY,         // COD terms
    PAYMENT_IN_ADVANCE,       // Prepayment required
    PROGRESS_BILLING,         // Milestone-based payments
    SEASONAL_TERMS,           // Seasonal payment terms
    CUSTOM_TERMS              // Custom payment arrangement
};

/**
 * Receivable risk categories
 */
enum class ReceivableRiskCategory {
    INVESTMENT_GRADE,         // High-quality, low-risk receivables
    COMMERCIAL_GRADE,         // Standard commercial risk
    SUB_INVESTMENT_GRADE,     // Higher risk, sub-investment grade
    DISTRESSED,               // Distressed or troubled accounts
    GOVERNMENT_BACKED,        // Government or agency backing
    INSURED,                  // Credit insurance coverage
    GUARANTEED,               // Third-party guarantee
    SECURED,                  // Secured by collateral
    UNSECURED                 // Unsecured receivables
};

/**
 * Fee structure for receivable financing
 */
struct ReceivableFeeStructure {
    enum class FeeType {
        FACTORING_FEE,           // Fee for factoring services
        DISCOUNT_FEE,            // Discount from face value
        SERVICING_FEE,           // Ongoing servicing fee
        COLLECTION_FEE,          // Collection-related fees
        DUE_DILIGENCE_FEE,       // Due diligence and verification
        WIRE_TRANSFER_FEE,       // Wire transfer costs
        LEGAL_FEE,               // Legal and documentation
        INSURANCE_FEE,           // Credit insurance premium
        RESERVE_FEE              // Reserve account maintenance
    };
    
    FeeType fee_type;
    Amount fee_amount;            // Fixed fee amount
    Rate fee_rate;                // Fee as percentage of receivable
    bool is_percentage;           // Whether fee is percentage-based
    std::string fee_description;  // Description of fee
    
    // Constructor for fixed amount fee
    static ReceivableFeeStructure createFixedFee(FeeType type, Amount amount, const std::string& description) {
        ReceivableFeeStructure fee;
        fee.fee_type = type;
        fee.fee_amount = amount;
        fee.fee_rate = 0.0;
        fee.is_percentage = false;
        fee.fee_description = description;
        return fee;
    }
    
    // Constructor for percentage-based fee
    static ReceivableFeeStructure createPercentageFee(FeeType type, Rate rate, const std::string& description) {
        ReceivableFeeStructure fee;
        fee.fee_type = type;
        fee.fee_amount = 0.0;
        fee.fee_rate = rate;
        fee.is_percentage = true;
        fee.fee_description = description;
        return fee;
    }
    
private:
    ReceivableFeeStructure() = default;
};

/**
 * Debtor (account debtor) information
 */
struct AccountDebtor {
    std::string debtor_id;           // Unique debtor identifier
    std::string debtor_name;         // Company/individual name
    std::string industry;            // Industry classification
    std::string country;             // Country of incorporation/residence
    
    // Credit information
    std::string credit_rating;       // External credit rating
    int internal_score;              // Internal credit score (1-100)
    Amount credit_limit;             // Approved credit limit
    Amount outstanding_balance;      // Total outstanding balance
    
    // Payment history
    int average_days_to_pay;         // Historical payment timing
    double payment_reliability_score; // Payment reliability (0-1)
    int disputed_invoices_count;     // Number of disputed invoices
    Amount largest_single_invoice;   // Largest invoice amount
    
    // Relationship information
    Date relationship_start_date;    // Start of business relationship
    Amount total_lifetime_sales;     // Total sales to this debtor
    bool is_related_party;           // Related party transaction
    
    AccountDebtor(const std::string& id, const std::string& name)
        : debtor_id(id), debtor_name(name), internal_score(75),
          credit_limit(0.0), outstanding_balance(0.0), average_days_to_pay(30),
          payment_reliability_score(0.85), disputed_invoices_count(0),
          largest_single_invoice(0.0), total_lifetime_sales(0.0),
          is_related_party(false) {}
};

/**
 * Invoice details for receivable backing
 */
struct InvoiceDetails {
    std::string invoice_number;      // Invoice identifier
    Date invoice_date;               // Date invoice was issued
    Date due_date;                   // Payment due date
    Amount invoice_amount;           // Original invoice amount
    Amount eligible_amount;          // Amount eligible for financing
    
    // Invoice specifics
    std::string goods_services_description; // Description of goods/services
    std::string purchase_order_number;      // Related PO number
    std::vector<std::string> supporting_documents; // List of supporting docs
    
    // Verification status
    bool is_verified;                // Invoice verification status
    Date verification_date;          // Date of verification
    std::string verification_method; // How invoice was verified
    
    // Dispute information
    bool is_disputed;                // Whether invoice is disputed
    Amount disputed_amount;          // Amount in dispute
    std::string dispute_reason;      // Reason for dispute
    Date dispute_date;               // Date dispute was raised
    
    InvoiceDetails(const std::string& number, Date invoice_date, Date due_date, Amount amount)
        : invoice_number(number), invoice_date(invoice_date), due_date(due_date),
          invoice_amount(amount), eligible_amount(amount), is_verified(false),
          is_disputed(false), disputed_amount(0.0) {}
};

/**
 * Collection and recovery information
 */
struct CollectionInfo {
    enum class CollectionStatus {
        CURRENT,                     // Payment current/on time
        PAST_DUE_1_30,              // 1-30 days past due
        PAST_DUE_31_60,             // 31-60 days past due
        PAST_DUE_61_90,             // 61-90 days past due
        PAST_DUE_91_120,            // 91-120 days past due
        PAST_DUE_OVER_120,          // Over 120 days past due
        IN_COLLECTION,               // Sent to collection agency
        LEGAL_ACTION,               // Legal action initiated
        WRITTEN_OFF,                // Written off as uncollectible
        RECOVERED                   // Recovered after write-off
    };
    
    CollectionStatus status;
    int days_past_due;               // Current days past due
    Date last_contact_date;          // Last contact with debtor
    std::string collection_agency;   // Collection agency (if applicable)
    Amount collection_costs;         // Costs incurred for collection
    
    // Recovery tracking
    Amount recovered_amount;         // Amount recovered to date
    Date recovery_date;              // Date of recovery
    std::string recovery_method;     // How payment was recovered
    
    CollectionInfo() : status(CollectionStatus::CURRENT), days_past_due(0),
                      collection_costs(0.0), recovered_amount(0.0) {}
};

/**
 * Receivable implementation for trade finance and factoring
 */
class Receivable {
private:
    std::string receivable_id_;
    ReceivableType receivable_type_;
    Status status_;
    
    // Receivable characteristics
    Amount face_value_;                    // Original face value
    Amount advance_amount_;                // Amount advanced (factoring)
    Amount reserve_amount_;                // Reserve held back
    Rate advance_rate_;                    // Advance rate (% of face value)
    
    // Dates
    Date origination_date_;                // Date receivable was created
    Date purchase_date_;                   // Date receivable was purchased
    Date expected_collection_date_;        // Expected collection date
    Date actual_collection_date_;          // Actual collection date (if paid)
    
    // Terms and conditions
    PaymentTerms payment_terms_;           // Payment terms
    ReceivableRiskCategory risk_category_; // Risk assessment category
    bool is_recourse_;                     // Recourse vs non-recourse
    bool has_credit_insurance_;            // Credit insurance coverage
    Amount insurance_coverage_;            // Insurance coverage amount
    
    // Parties
    AccountDebtor account_debtor_;         // The debtor/customer
    std::optional<Obligor> seller_;        // Original seller (if factored)
    InvoiceDetails invoice_details_;       // Underlying invoice information
    
    // Fees and pricing
    std::vector<ReceivableFeeStructure> fee_structure_; // Fee schedule
    Rate discount_rate_;                   // Discount rate applied
    Amount total_fees_charged_;            // Total fees charged
    Amount net_purchase_price_;            // Net amount paid for receivable
    
    // Collection and recovery
    CollectionInfo collection_info_;       // Collection status and history
    Amount partial_payments_received_;     // Partial payments received
    Date last_payment_date_;               // Date of last payment
    Amount last_payment_amount_;           // Amount of last payment
    
    // Performance tracking
    bool is_diluted_;                      // Dilution occurred (credits, returns, etc.)
    Amount dilution_amount_;               // Amount of dilution
    std::string dilution_reason_;          // Reason for dilution
    
    // Legal and compliance
    std::string governing_law_;            // Governing law jurisdiction
    bool notification_required_;           // Debtor notification required
    bool is_notification_sent_;            // Notification sent to debtor
    Date notification_date_;               // Date notification was sent

public:
    Receivable(const std::string& id, ReceivableType type, Amount face_value,
               const AccountDebtor& debtor, const InvoiceDetails& invoice,
               PaymentTerms terms, Date origination_date);
    
    // Core asset interface
    void processPayment(Amount payment_amount, Date payment_date);
    void applyDefault(Date default_date);
    void applyPartialPayment(Amount amount, Date payment_date);
    Balance getCurrentBalance() const;
    Amount calculateExpectedValue() const;
    void resetToOriginal();
    
    // Receivable-specific operations
    void purchaseReceivable(Amount purchase_price, Rate advance_rate, Date purchase_date);
    void setReserveAmount(Amount reserve_amount);
    void releaseReserve(Amount release_amount, Date release_date);
    void applyDilution(Amount dilution_amount, const std::string& reason, Date dilution_date);
    
    // Collection management
    void updateCollectionStatus(CollectionInfo::CollectionStatus status, Date status_date);
    void recordCollectionActivity(const std::string& activity, Date activity_date);
    void sendToCollection(const std::string& collection_agency, Date referral_date);
    void initiateLegalAction(Date action_date, const std::string& legal_firm);
    
    // Fee management
    void addFee(const ReceivableFeeStructure& fee);
    Amount calculateTotalFees() const;
    void chargeFee(ReceivableFeeStructure::FeeType fee_type, Amount amount, Date charge_date);
    
    // Risk assessment and monitoring
    double calculateCollectionProbability() const;
    int calculateDaysToCollection() const;  
    double assessDilutionRisk() const;
    Amount calculateExpectedLoss() const;
    ReceivableRiskCategory reassessRiskCategory() const;
    
    // Verification and validation
    bool verifyInvoice(const std::string& verification_method, Date verification_date);
    void flagDispute(Amount disputed_amount, const std::string& reason, Date dispute_date);
    void resolveDispute(Amount resolution_amount, Date resolution_date);
    
    // Notification and legal compliance
    void sendDebtorNotification(Date notification_date);
    void recordLegalCompliance(const std::string& compliance_type, bool is_compliant);
    bool isEligibleForFinancing() const;
    
    // Analytics and reporting
    double calculateYieldToMaturity() const;
    int getDaysOutstanding() const;
    double getCollectionEfficiencyRatio() const;
    Amount calculateNetRealizedValue() const;
    
    // Concentration risk management
    double calculateDebtorConcentration(const std::vector<Receivable>& portfolio) const;
    bool exceedsConcentrationLimit(Amount concentration_limit) const;
    
    // Getters
    const std::string& getReceivableId() const { return receivable_id_; }
    ReceivableType getReceivableType() const { return receivable_type_; }
    Status getStatus() const { return status_; }
    
    Amount getFaceValue() const { return face_value_; }
    Amount getAdvanceAmount() const { return advance_amount_; }
    Amount getReserveAmount() const { return reserve_amount_; }
    Rate getAdvanceRate() const { return advance_rate_; }
    
    Date getOriginationDate() const { return origination_date_; }
    Date getPurchaseDate() const { return purchase_date_; }
    Date getExpectedCollectionDate() const { return expected_collection_date_; }
    Date getActualCollectionDate() const { return actual_collection_date_; }
    
    PaymentTerms getPaymentTerms() const { return payment_terms_; }
    ReceivableRiskCategory getRiskCategory() const { return risk_category_; }
    bool isRecourse() const { return is_recourse_; }
    bool hasCreditInsurance() const { return has_credit_insurance_; }
    Amount getInsuranceCoverage() const { return insurance_coverage_; }
    
    const AccountDebtor& getAccountDebtor() const { return account_debtor_; }
    const std::optional<Obligor>& getSeller() const { return seller_; }
    const InvoiceDetails& getInvoiceDetails() const { return invoice_details_; }
    
    const std::vector<ReceivableFeeStructure>& getFeeStructure() const { return fee_structure_; }
    Rate getDiscountRate() const { return discount_rate_; }
    Amount getTotalFeesCharged() const { return total_fees_charged_; }
    Amount getNetPurchasePrice() const { return net_purchase_price_; }
    
    const CollectionInfo& getCollectionInfo() const { return collection_info_; }
    Amount getPartialPaymentsReceived() const { return partial_payments_received_; }
    Date getLastPaymentDate() const { return last_payment_date_; }
    Amount getLastPaymentAmount() const { return last_payment_amount_; }
    
    bool isDiluted() const { return is_diluted_; }
    Amount getDilutionAmount() const { return dilution_amount_; }
    const std::string& getDilutionReason() const { return dilution_reason_; }
    
    const std::string& getGoverningLaw() const { return governing_law_; }
    bool isNotificationRequired() const { return notification_required_; }
    bool isNotificationSent() const { return is_notification_sent_; }
    
    // Setters
    void setStatus(Status status) { status_ = status; }
    void setRiskCategory(ReceivableRiskCategory category) { risk_category_ = category; }
    void setRecourse(bool is_recourse) { is_recourse_ = is_recourse; }
    void setCreditInsurance(bool has_insurance, Amount coverage);
    void setSeller(const Obligor& seller) { seller_ = seller; }
    void setGoverningLaw(const std::string& governing_law) { governing_law_ = governing_law; }
    void setNotificationRequired(bool required) { notification_required_ = required; }
};

// Utility functions
std::string receivableTypeToString(ReceivableType type);
std::string paymentTermsToString(PaymentTerms terms);
std::string receivableRiskCategoryToString(ReceivableRiskCategory category);
std::string collectionStatusToString(CollectionInfo::CollectionStatus status);
std::string feeTypeToString(ReceivableFeeStructure::FeeType fee_type);

// Payment terms conversion functions
int paymentTermsToDays(PaymentTerms terms);
PaymentTerms daysToPaymentTerms(int days);

// Risk assessment functions
ReceivableRiskCategory assessReceivableRisk(const AccountDebtor& debtor, Amount invoice_amount,
                                          PaymentTerms terms, bool has_insurance);
double calculateDefaultProbability(ReceivableRiskCategory risk_category, int days_outstanding);

// Portfolio analysis functions
Amount calculatePortfolioConcentration(const std::vector<Receivable>& portfolio, 
                                     const std::string& debtor_id);
double calculatePortfolioWeightedAverageLife(const std::vector<Receivable>& portfolio);
Amount calculatePortfolioExpectedLoss(const std::vector<Receivable>& portfolio);

} // namespace Structura