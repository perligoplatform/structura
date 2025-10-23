#pragma once

#include "core/deal_base.h"
#include "entities/bond.h"
#include "assets/pool.h"
#include <map>
#include <string>

namespace Structura {

/**
 * Bond types for different market segments
 */
enum class BondType {
    CORPORATE,
    GOVERNMENT,
    MUNICIPAL,
    HIGH_YIELD,
    INVESTMENT_GRADE
};

std::string bondTypeToString(BondType type);

/**
 * Credit rating system
 */
enum class CreditRating {
    AAA, AA_PLUS, AA, AA_MINUS,
    A_PLUS, A, A_MINUS,
    BBB_PLUS, BBB, BBB_MINUS,
    BB_PLUS, BB, BB_MINUS,
    B_PLUS, B, B_MINUS,
    CCC_PLUS, CCC, CCC_MINUS,
    CC, C, D,
    NOT_RATED
};

std::string creditRatingToString(CreditRating rating);
bool isInvestmentGrade(CreditRating rating);

/**
 * Structured bond for bond deals
 */
struct StructuredBond {
    std::string bond_id;
    BondType bond_type;
    CreditRating rating;
    Amount notional;
    Rate coupon_rate;
    Date maturity_date;
    PaymentFrequency payment_freq;
    
    // Credit metrics
    double probability_of_default;
    double loss_given_default;
    double recovery_rate;
    
    // Current state
    Amount outstanding_balance;
    Date last_payment_date;
    int payments_missed;
    bool is_defaulted;
    
    StructuredBond(const std::string& id, BondType type, CreditRating rating,
                   Amount notional, Rate coupon, Date maturity, PaymentFrequency freq);
    
    // Payment calculations
    Amount calculateCouponPayment() const;
    Amount calculateAccruedInterest(Date as_of_date) const;
    double calculateYieldToMaturity(Amount market_price) const;
    double calculateModifiedDuration(Rate yield) const;
    
    // Credit methods
    void updateCreditRating(CreditRating new_rating);
    void markDefault();
    Amount calculateExpectedLoss() const;
};

/**
 * Bond waterfall structure for payments
 */
struct BondWaterfall {
    std::vector<std::string> payment_sequence;
    std::map<std::string, Rate> subordination_levels;
    std::map<std::string, Amount> reserves_required;
    
    // Waterfall execution
    std::map<std::string, Amount> distributeFunds(Amount available_funds,
                                                  const std::map<std::string, StructuredBond>& bonds) const;
    
    // Credit enhancement
    Amount calculateRequiredReserves(const std::map<std::string, StructuredBond>& bonds) const;
    bool checkCoverageRatios(Amount pool_cashflow, Amount bond_payments) const;
};

/**
 * Concrete deal implementation for bond structures
 */
class BondDeal : public DealBase {
private:
    std::map<std::string, StructuredBond> bonds_;
    BondWaterfall waterfall_;
    Pool<Bond>* underlying_pool_;
    
    // Deal-specific accounts
    std::string reserve_account_id_;
    std::string excess_spread_account_id_;
    std::string trustee_account_id_;
    
    // Performance tracking
    std::map<Date, Amount> historical_losses_;
    std::map<Date, double> portfolio_ratings_;
    double weighted_average_rating_;
    double portfolio_duration_;

public:
    explicit BondDeal(const DealInfo& deal_info);
    ~BondDeal() override = default;
    
    // Deal setup
    void setUnderlyingPool(Pool<Bond>* pool);
    void addBond(const StructuredBond& bond);
    void setupWaterfall(const BondWaterfall& waterfall);
    void setupCreditEnhancement(Amount reserve_target, Rate excess_spread_target);
    
    // DealBase interface implementation
    bool validateDeal() const override;
    void processPaymentDate(Date payment_date) override;
    std::map<std::string, Amount> calculateMetrics(Date as_of_date) const override;
    std::string getStatus() const override;
    void updateAssumptions(const std::map<std::string, double>& new_assumptions) override;
    std::vector<Date> getPaymentDates(Date start_date, Date end_date) const override;
    void generateReports(Date report_date) const override;
    
    // Bond-specific methods
    std::map<std::string, StructuredBond> getBonds() const { return bonds_; }
    BondWaterfall getWaterfall() const { return waterfall_; }
    
    // Portfolio analytics
    double calculatePortfolioDuration() const;
    double calculateWeightedAverageRating() const;
    Amount calculateTotalNotional() const;
    std::map<CreditRating, Amount> getRatingDistribution() const;
    
    // Credit risk analysis
    Amount calculateExpectedLoss() const;
    Amount calculateUnexpectedLoss(double confidence_level = 0.99) const;
    std::map<std::string, double> calculateBondProbabilities() const;
    
    // Performance metrics
    double calculatePortfolioYield() const;
    double calculateExcessSpread() const;
    std::map<std::string, Amount> getCreditLosses(Date start_date, Date end_date) const;
    
    // Stress testing
    void applyRatingDowngrade(int notches);
    void applyDefaultScenario(double default_rate_multiplier);
    std::map<std::string, Amount> stressTestWaterfall(double loss_scenario) const;

private:
    void processBondPayments(Date payment_date);
    void updateCreditMetrics(Date as_of_date);
    void checkCoverageTests(Date test_date);
    Amount collectPoolCashflow(Date collection_date);
    void distributeCashflow(Amount available_funds, Date distribution_date);
    void updateReserves(Date update_date);
    
    // Risk calculations
    double calculateCorrelationAdjustment() const;
    Amount calculateRiskAdjustedReserves() const;
    void updatePortfolioMetrics();
};

} // namespace Structura