#include "assets/projected_cash_flow.h"
#include <algorithm>
#include <cmath>
#include <stdexcept>
#include <numeric>
#include <random>

namespace Structura {

ProjectedCashFlow::ProjectedCashFlow(const std::string& id, ProjectedCashFlowType type,
                                   Amount total_face_value, const CashFlowProjection& projection,
                                   Date acquisition_date)
    : asset_id_(id), cash_flow_type_(type), status_(Status::Current),
      total_face_value_(total_face_value), purchased_amount_(0.0),
      purchase_yield_(0.0), acquisition_date_(acquisition_date),
      cash_flow_start_date_(acquisition_date), payment_frequency_(CashFlowFrequency::MONTHLY),
      total_payment_count_(0), payments_received_count_(0),
      projection_model_(projection), has_guarantee_(false), 
      guarantee_coverage_(0.0), cumulative_payments_received_(0.0),
      cumulative_shortfalls_(0.0), payment_reliability_ratio_(1.0),
      current_market_value_(total_face_value), book_value_(total_face_value),
      current_yield_(0.06), yield_to_maturity_(0.06), volatility_measure_(0.1),
      value_at_risk_(0.0), expected_shortfall_(0.0), is_transferable_(true),
      servicing_fee_rate_(0.0) {
    
    if (total_face_value <= 0) {
        throw std::invalid_argument("Total face value must be positive");
    }
    
    // Set default cash flow end date
    cash_flow_start_date_ = acquisition_date;
    cash_flow_end_date_ = acquisition_date + (projection.projection_horizon_years * 365);
    
    // Initialize performance metrics
    payment_reliability_ratio_ = 1.0;
    
    // Set up basic payment schedule if not provided
    if (payment_schedule_.empty()) {
        generateBasicPaymentSchedule();
    }
}

void ProjectedCashFlow::processPayment(Amount payment_amount, Date payment_date) {
    if (payment_amount <= 0) {
        throw std::invalid_argument("Payment amount must be positive");
    }
    
    if (status_ == Status::PaidOff || status_ == Status::Defaulted) {
        throw std::runtime_error("Cannot process payment on completed asset");
    }
    
    cumulative_payments_received_ += payment_amount;
    
    // Find corresponding scheduled payment
    auto it = std::find_if(payment_schedule_.begin(), payment_schedule_.end(),
        [payment_date](const ProjectedPayment& payment) {
            return !payment.is_received && 
                   std::abs((payment.payment_date - payment_date)) <= 7; // Within 7 days
        });
    
    if (it != payment_schedule_.end()) {
        it->actual_amount = payment_amount;
        it->actual_payment_date = payment_date;
        it->is_received = true;
        payments_received_count_++;
        
        // Calculate shortfall if any
        if (payment_amount < it->scheduled_amount) {
            cumulative_shortfalls_ += (it->scheduled_amount - payment_amount);
        }
    }
    
    // Recalculate payment reliability ratio
    payment_reliability_ratio_ = calculatePaymentReliabilityRatio();
    
    // Check if all payments received
    if (payments_received_count_ >= total_payment_count_) {
        status_ = Status::PaidOff;
    }
}

void ProjectedCashFlow::applyDefault(Date default_date) {
    status_ = Status::Defaulted;
    
    // Mark all future payments as not received
    for (auto& payment : payment_schedule_) {
        if (payment.payment_date > default_date && !payment.is_received) {
            cumulative_shortfalls_ += payment.scheduled_amount;
        }
    }
    
    // Recalculate metrics
    payment_reliability_ratio_ = calculatePaymentReliabilityRatio();
}

void ProjectedCashFlow::applyPartialPayment(Amount amount, Date payment_date) {
    processPayment(amount, payment_date);
}

Balance ProjectedCashFlow::getCurrentBalance() const {
    Amount remaining_value = total_face_value_ - cumulative_payments_received_;
    
    if (status_ == Status::PaidOff) {
        remaining_value = 0.0;
    } else if (status_ == Status::Defaulted) {
        // Calculate recovery value
        remaining_value = std::max(0.0, calculateExpectedValue() - cumulative_payments_received_);
    }
    
    return remaining_value + calculateServicingFees();
}

Amount ProjectedCashFlow::calculateExpectedValue() const {
    if (status_ == Status::PaidOff) {
        return cumulative_payments_received_;
    }
    
    Amount expected_value = cumulative_payments_received_;
    
    // Add expected value of remaining payments
    for (const auto& payment : payment_schedule_) {
        if (!payment.is_received) {
            expected_value += payment.scheduled_amount * payment.certainty_factor;
        }
    }
    
    return expected_value;
}

void ProjectedCashFlow::resetToOriginal() {
    status_ = Status::Current;
    cumulative_payments_received_ = 0.0;
    cumulative_shortfalls_ = 0.0;
    payments_received_count_ = 0;
    payment_reliability_ratio_ = 1.0;
    
    // Reset all payments
    for (auto& payment : payment_schedule_) {
        payment.is_received = false;
        payment.actual_amount = 0.0;
        payment.actual_payment_date = Date{};
    }
}

void ProjectedCashFlow::setPaymentSchedule(const std::vector<ProjectedPayment>& schedule) {
    payment_schedule_ = schedule;
    total_payment_count_ = static_cast<int>(schedule.size());
    payments_received_count_ = 0;
    
    // Recalculate totals
    total_face_value_ = 0.0;
    for (const auto& payment : payment_schedule_) {
        total_face_value_ += payment.scheduled_amount;
        if (payment.is_received) {
            payments_received_count_++;
        }
    }
}

void ProjectedCashFlow::addProjectedPayment(const ProjectedPayment& payment) {
    payment_schedule_.push_back(payment);
    total_payment_count_++;
    total_face_value_ += payment.scheduled_amount;
    
    // Sort by payment date
    std::sort(payment_schedule_.begin(), payment_schedule_.end(),
        [](const ProjectedPayment& a, const ProjectedPayment& b) {
            return a.payment_date < b.payment_date;
        });
}

void ProjectedCashFlow::updateProjectedPayment(size_t payment_index, const ProjectedPayment& updated_payment) {
    if (payment_index >= payment_schedule_.size()) {
        throw std::out_of_range("Payment index out of range");
    }
    
    Amount old_amount = payment_schedule_[payment_index].scheduled_amount;
    payment_schedule_[payment_index] = updated_payment;
    
    // Update total face value
    total_face_value_ = total_face_value_ - old_amount + updated_payment.scheduled_amount;
}

void ProjectedCashFlow::receiveScheduledPayment(size_t payment_index, Amount actual_amount, Date payment_date) {
    if (payment_index >= payment_schedule_.size()) {
        throw std::out_of_range("Payment index out of range");
    }
    
    auto& payment = payment_schedule_[payment_index];
    payment.actual_amount = actual_amount;
    payment.actual_payment_date = payment_date;
    payment.is_received = true;
    
    payments_received_count_++;
    cumulative_payments_received_ += actual_amount;
    
    // Track shortfall
    if (actual_amount < payment.scheduled_amount) {
        cumulative_shortfalls_ += (payment.scheduled_amount - actual_amount);
    }
    
    payment_reliability_ratio_ = calculatePaymentReliabilityRatio();
}

void ProjectedCashFlow::updateProjection(const CashFlowProjection& new_projection) {
    projection_model_ = new_projection;
    
    // Recalculate dependent values
    current_yield_ = projection_model_.discount_rate;
    yield_to_maturity_ = calculateInternalRateOfReturn();
}

void ProjectedCashFlow::recalculateProjection(Rate new_discount_rate) {
    projection_model_.discount_rate = new_discount_rate;
    projection_model_.net_present_value = calculateNetPresentValue(new_discount_rate);
    current_yield_ = new_discount_rate;
}

Amount ProjectedCashFlow::calculateNetPresentValue(Rate discount_rate) const {
    Amount npv = 0.0;
    Date current_date = Date(); // Today's date in QuantLib
    
    for (const auto& payment : payment_schedule_) {
        if (!payment.is_received && payment.payment_date > current_date) {
            int days_to_payment = static_cast<int>((payment.payment_date - current_date));
            double years_to_payment = days_to_payment / 365.0;
            double discount_factor = std::pow(1.0 + discount_rate, -years_to_payment);
            
            npv += payment.scheduled_amount * payment.certainty_factor * discount_factor;
        }
    }
    
    return npv;
}

Amount ProjectedCashFlow::calculateFutureValue(Date future_date, Rate growth_rate) const {
    Amount fv = 0.0;
    
    for (const auto& payment : payment_schedule_) {
        if (payment.payment_date <= future_date) {
            int days_from_payment = static_cast<int>((future_date - payment.payment_date));
            double years_from_payment = days_from_payment / 365.0;
            double growth_factor = std::pow(1.0 + growth_rate, years_from_payment);
            
            Amount payment_amount = payment.is_received ? payment.actual_amount : 
                                  payment.scheduled_amount * payment.certainty_factor;
            fv += payment_amount * growth_factor;
        }
    }
    
    return fv;
}

void ProjectedCashFlow::addPerformanceMetric(const PerformanceMetrics& metric) {
    performance_metrics_.push_back(metric);
}

void ProjectedCashFlow::updatePerformanceMetric(const std::string& metric_name, Amount new_value, Date measurement_date) {
    auto it = std::find_if(performance_metrics_.begin(), performance_metrics_.end(),
        [&metric_name](const PerformanceMetrics& metric) {
            return metric.metric_name == metric_name;
        });
    
    if (it != performance_metrics_.end()) {
        it->historical_values.push_back(it->current_value);
        it->measurement_dates.push_back(measurement_date);
        it->current_value = new_value;
    }
}

double ProjectedCashFlow::calculatePaymentReliabilityRatio() const {
    if (payment_schedule_.empty()) return 1.0;
    
    Amount total_scheduled = 0.0;
    Amount total_actual = 0.0;
    
    for (const auto& payment : payment_schedule_) {
        if (payment.is_received) {
            total_scheduled += payment.scheduled_amount;
            total_actual += payment.actual_amount;
        }
    }
    
    return (total_scheduled > 0) ? (total_actual / total_scheduled) : 1.0;
}

Amount ProjectedCashFlow::calculateAveragePaymentShortfall() const {
    if (payments_received_count_ == 0) return 0.0;
    
    Amount total_shortfall = 0.0;
    int shortfall_count = 0;
    
    for (const auto& payment : payment_schedule_) {
        if (payment.is_received && payment.actual_amount < payment.scheduled_amount) {
            total_shortfall += (payment.scheduled_amount - payment.actual_amount);
            shortfall_count++;
        }
    }
    
    return (shortfall_count > 0) ? (total_shortfall / shortfall_count) : 0.0;
}

void ProjectedCashFlow::addRiskFactor(CashFlowRiskFactor risk_factor) {
    if (std::find(risk_factors_.begin(), risk_factors_.end(), risk_factor) == risk_factors_.end()) {
        risk_factors_.push_back(risk_factor);
    }
}

void ProjectedCashFlow::removeRiskFactor(CashFlowRiskFactor risk_factor) {
    risk_factors_.erase(
        std::remove(risk_factors_.begin(), risk_factors_.end(), risk_factor),
        risk_factors_.end());
}

double ProjectedCashFlow::assessOverallRisk() const {
    double base_risk = 0.05; // 5% base risk
    
    // Add risk based on risk factors
    for (const auto& risk_factor : risk_factors_) {
        switch (risk_factor) {
            case CashFlowRiskFactor::COUNTERPARTY_RISK:
                base_risk += 0.03;
                break;
            case CashFlowRiskFactor::REGULATORY_RISK:
                base_risk += 0.02;
                break;
            case CashFlowRiskFactor::MARKET_RISK:
                base_risk += 0.04;
                break;
            case CashFlowRiskFactor::OPERATIONAL_RISK:
                base_risk += 0.02;
                break;
            case CashFlowRiskFactor::TECHNOLOGY_RISK:
                base_risk += 0.05;
                break;
            case CashFlowRiskFactor::LEGAL_RISK:
                base_risk += 0.03;
                break;
            case CashFlowRiskFactor::CONCENTRATION_RISK:
                base_risk += 0.04;
                break;
            case CashFlowRiskFactor::LIQUIDITY_RISK:
                base_risk += 0.02;
                break;
            case CashFlowRiskFactor::INFLATION_RISK:
                base_risk += 0.01;
                break;
            case CashFlowRiskFactor::CURRENCY_RISK:
                base_risk += 0.03;
                break;
            case CashFlowRiskFactor::FORCE_MAJEURE_RISK:
                base_risk += 0.01;
                break;
            case CashFlowRiskFactor::PERFORMANCE_RISK:
                base_risk += 0.03;
                break;
        }
    }
    
    // Adjust for payment reliability
    base_risk *= (2.0 - payment_reliability_ratio_);
    
    return std::min(0.5, base_risk); // Cap at 50%
}

Amount ProjectedCashFlow::calculateValueAtRisk(double confidence_level) const {
    // Simplified VaR calculation based on volatility
    double z_score = 1.645; // 95% confidence level
    if (confidence_level >= 0.99) z_score = 2.326;
    else if (confidence_level >= 0.95) z_score = 1.645;
    else if (confidence_level >= 0.90) z_score = 1.282;
    
    Amount expected_value = calculateExpectedValue();
    Amount var = expected_value * volatility_measure_ * z_score;
    
    return var;
}

void ProjectedCashFlow::performSensitivityAnalysis(const std::map<std::string, double>& scenarios) {
    sensitivity_analysis_.clear();
    
    Amount base_value = calculateExpectedValue();
    
    for (const auto& scenario : scenarios) {
        // Apply stress factor to payments
        Amount stressed_value = 0.0;
        for (const auto& payment : payment_schedule_) {
            if (!payment.is_received) {
                Amount stressed_payment = payment.scheduled_amount * (1.0 + scenario.second);
                stressed_value += stressed_payment * payment.certainty_factor;
            }
        }
        stressed_value += cumulative_payments_received_;
        
        double sensitivity = (stressed_value - base_value) / base_value;
        sensitivity_analysis_[scenario.first] = sensitivity;
    }
}

Amount ProjectedCashFlow::calculateDiscountedCashFlowValue(Rate discount_rate) const {
    return calculateNetPresentValue(discount_rate);
}

Amount ProjectedCashFlow::calculateComparableAssetValue() const {
    // Simplified comparable valuation using yield
    return total_face_value_ * (current_yield_ + 0.02); // Add risk premium
}

Amount ProjectedCashFlow::calculateLiquidationValue() const {
    // Assume 60-80% of expected value in liquidation
    double liquidation_discount = 0.7;
    
    // Adjust based on asset type
    switch (cash_flow_type_) {
        case ProjectedCashFlowType::GOVERNMENT_CONTRACTS:
        case ProjectedCashFlowType::UTILITY_PAYMENTS:
            liquidation_discount = 0.8; // Higher recovery for government/utility
            break;
        case ProjectedCashFlowType::LOTTERY_PAYMENTS:
        case ProjectedCashFlowType::STRUCTURED_SETTLEMENT:
            liquidation_discount = 0.75; // Established secondary market
            break;
        case ProjectedCashFlowType::ROYALTY_STREAM:
        case ProjectedCashFlowType::PATENT_ROYALTIES:
            liquidation_discount = 0.6; // More volatile/uncertain
            break;
        default:
            liquidation_discount = 0.7;
            break;
    }
    
    return calculateExpectedValue() * liquidation_discount;
}

void ProjectedCashFlow::updateMarketValue(Amount new_market_value, Date valuation_date) {
    current_market_value_ = new_market_value;
    
    // Update current yield based on market value
    if (new_market_value > 0) {
        current_yield_ = calculateExpectedValue() / new_market_value - 1.0;
    }
}

Amount ProjectedCashFlow::stressTestScenario(const std::string& scenario_name, 
                                           const std::map<std::string, double>& stress_factors) const {
    Amount stressed_value = cumulative_payments_received_;
    
    for (const auto& payment : payment_schedule_) {
        if (!payment.is_received) {
            Amount base_payment = payment.scheduled_amount;
            
            // Apply stress factors
            for (const auto& factor : stress_factors) {
                if (factor.first == "payment_reduction") {
                    base_payment *= (1.0 - factor.second);
                } else if (factor.first == "timing_delay") {
                    // Discount for timing delay
                    double delay_discount = std::pow(1.0 + current_yield_, -factor.second);
                    base_payment *= delay_discount;
                } else if (factor.first == "default_probability") {
                    base_payment *= (1.0 - factor.second);
                }
            }
            
            stressed_value += base_payment * payment.certainty_factor;
        }
    }
    
    return stressed_value;
}

void ProjectedCashFlow::performMonteCarloAnalysis(int simulation_count) {
    std::vector<Amount> simulation_results;
    std::random_device rd;
    std::mt19937 gen(rd());
    std::normal_distribution<double> payment_variation(1.0, volatility_measure_);
    
    for (int i = 0; i < simulation_count; ++i) {
        Amount simulated_value = cumulative_payments_received_;
        
        for (const auto& payment : payment_schedule_) {
            if (!payment.is_received) {
                double payment_multiplier = std::max(0.0, payment_variation(gen));
                Amount simulated_payment = payment.scheduled_amount * payment_multiplier;
                simulated_value += simulated_payment * payment.certainty_factor;
            }
        }
        
        simulation_results.push_back(simulated_value);
    }
    
    // Calculate statistics
    std::sort(simulation_results.begin(), simulation_results.end());
    
    projection_model_.worst_case_value = simulation_results[static_cast<size_t>(simulation_count * 0.05)];
    projection_model_.base_case_value = simulation_results[simulation_count / 2];
    projection_model_.best_case_value = simulation_results[static_cast<size_t>(simulation_count * 0.95)];
    
    // Update expected shortfall
    size_t var_index = static_cast<size_t>(simulation_count * 0.05);
    Amount sum_tail = 0.0;
    for (size_t i = 0; i < var_index; ++i) {
        sum_tail += simulation_results[i];
    }
    expected_shortfall_ = calculateExpectedValue() - (sum_tail / var_index);
}

Amount ProjectedCashFlow::calculateWorstCaseScenario() const {
    return projection_model_.worst_case_value;
}

Amount ProjectedCashFlow::calculateBestCaseScenario() const {
    return projection_model_.best_case_value;
}

void ProjectedCashFlow::setPrimaryObligor(const Obligor& obligor) {
    primary_obligor_ = obligor;
}

void ProjectedCashFlow::addSecondaryObligor(const Obligor& obligor) {
    secondary_obligors_.push_back(obligor);
}

void ProjectedCashFlow::setGuarantee(const std::string& provider, Amount coverage) {
    has_guarantee_ = true;
    guarantee_provider_ = provider;
    guarantee_coverage_ = coverage;
}

double ProjectedCashFlow::assessCounterpartyRisk() const {
    if (!primary_obligor_.has_value()) return 0.15; // Default risk if no obligor info
    
    // Simplified counterparty risk assessment
    double base_risk = 0.05;
    
    // Adjust based on obligor credit rating (simplified)
    const auto& obligor = primary_obligor_.value();
    auto credit_rating = obligor.getStringField("credit_rating");
    if (credit_rating.has_value()) {
        if (credit_rating->find("AAA") != std::string::npos) {
            base_risk = 0.01;
        } else if (credit_rating->find("AA") != std::string::npos) {
            base_risk = 0.02;
        } else if (credit_rating->find("A") != std::string::npos) {
            base_risk = 0.03;
        } else if (credit_rating->find("BBB") != std::string::npos) {
            base_risk = 0.05;
        } else if (credit_rating->find("BB") != std::string::npos) {
            base_risk = 0.10;
        } else {
            base_risk = 0.15;
        }
    }
    
    // Reduce risk if guaranteed
    if (has_guarantee_ && guarantee_coverage_ >= total_face_value_ * 0.8) {
        base_risk *= 0.3; // 70% risk reduction for substantial guarantee
    }
    
    return base_risk;
}

void ProjectedCashFlow::setServicer(const std::string& servicer, Amount fee_rate) {
    servicer_ = servicer;
    servicing_fee_rate_ = fee_rate;
}

void ProjectedCashFlow::processServicerReport(const std::string& report_data, Date report_date) {
    last_servicer_report_date_ = report_date;
    // In a full implementation, this would parse and process servicer report data
}

Amount ProjectedCashFlow::calculateServicingFees() const {
    return total_face_value_ * servicing_fee_rate_;
}

void ProjectedCashFlow::setTransferability(bool is_transferable, const std::vector<std::string>& restrictions) {
    is_transferable_ = is_transferable;
    transfer_restrictions_ = restrictions;
}

bool ProjectedCashFlow::validateLegalStructure() const {
    // Basic validation checks
    if (underlying_agreement_.empty()) return false;
    if (!primary_obligor_.has_value()) return false;
    if (total_face_value_ <= 0) return false;
    if (payment_schedule_.empty()) return false;
    
    return true;
}

void ProjectedCashFlow::recordComplianceEvent(const std::string& event_type, Date event_date) {
    // In a full implementation, this would maintain a compliance event log
}

double ProjectedCashFlow::calculateInternalRateOfReturn() const {
    if (purchased_amount_ <= 0) return current_yield_;
    
    // Simplified IRR calculation using Newton-Raphson method
    double irr_guess = current_yield_;
    const double tolerance = 1e-8;
    const int max_iterations = 100;
    
    for (int i = 0; i < max_iterations; ++i) {
        double npv = -purchased_amount_;
        double npv_derivative = 0.0;
        
        Date current_date = Date(); // Today's date in QuantLib
        
        for (const auto& payment : payment_schedule_) {
            double years = static_cast<double>((payment.payment_date - current_date)) / 365.0;
            Amount payment_amount = payment.is_received ? payment.actual_amount : 
                                  payment.scheduled_amount * payment.certainty_factor;
            
            double discount_factor = std::pow(1.0 + irr_guess, -years);
            npv += payment_amount * discount_factor;
            npv_derivative -= payment_amount * years * discount_factor / (1.0 + irr_guess);
        }
        
        if (std::abs(npv) < tolerance) break;
        if (std::abs(npv_derivative) < tolerance) break;
        
        irr_guess -= npv / npv_derivative;
    }
    
    return irr_guess;
}

double ProjectedCashFlow::calculateModifiedDuration() const {
    // Simplified duration calculation
    double duration = 0.0;
    Amount total_pv = 0.0;
    Date current_date = Date(); // Today's date in QuantLib
    
    for (const auto& payment : payment_schedule_) {
        if (!payment.is_received && payment.payment_date > current_date) {
            double years = static_cast<double>((payment.payment_date - current_date)) / 365.0;
            double discount_factor = std::pow(1.0 + current_yield_, -years);
            Amount payment_pv = payment.scheduled_amount * payment.certainty_factor * discount_factor;
            
            duration += years * payment_pv;
            total_pv += payment_pv;
        }
    }
    
    return (total_pv > 0) ? (duration / total_pv) : 0.0;
}

Amount ProjectedCashFlow::calculateCashOnCashReturn() const {
    if (purchased_amount_ <= 0) return 0.0;
    
    return cumulative_payments_received_ / purchased_amount_;
}

double ProjectedCashFlow::calculateVolatilityMeasure() const {
    if (payment_schedule_.size() < 2) return volatility_measure_;
    
    std::vector<double> payment_ratios;
    
    for (const auto& payment : payment_schedule_) {
        if (payment.is_received && payment.scheduled_amount > 0) {
            double ratio = payment.actual_amount / payment.scheduled_amount;
            payment_ratios.push_back(ratio);
        }
    }
    
    if (payment_ratios.size() < 2) return volatility_measure_;
    
    // Calculate standard deviation of payment ratios
    double mean = std::accumulate(payment_ratios.begin(), payment_ratios.end(), 0.0) / payment_ratios.size();
    double variance = 0.0;
    
    for (double ratio : payment_ratios) {
        variance += std::pow(ratio - mean, 2);
    }
    variance /= (payment_ratios.size() - 1);
    
    return std::sqrt(variance);
}

double ProjectedCashFlow::calculateCorrelation(const ProjectedCashFlow& other_asset) const {
    // Simplified correlation calculation based on asset types and risk factors
    double correlation = 0.0;
    
    // Base correlation on asset type similarity
    if (cash_flow_type_ == other_asset.cash_flow_type_) {
        correlation = 0.6; // High correlation for same asset type
    } else {
        correlation = 0.2; // Lower correlation for different types
    }
    
    // Adjust for common obligors
    if (primary_obligor_.has_value() && other_asset.primary_obligor_.has_value()) {
        if (primary_obligor_->getId() == other_asset.primary_obligor_->getId()) {
            correlation = 0.8; // High correlation for same obligor
        }
    }
    
    // Adjust for common risk factors
    int common_risk_factors = 0;
    for (const auto& risk_factor : risk_factors_) {
        if (std::find(other_asset.risk_factors_.begin(), other_asset.risk_factors_.end(), risk_factor) 
            != other_asset.risk_factors_.end()) {
            common_risk_factors++;
        }
    }
    
    if (common_risk_factors > 0) {
        correlation += common_risk_factors * 0.05; // Increase correlation for shared risks
    }
    
    return std::min(0.95, correlation);
}

Amount ProjectedCashFlow::calculateDiversificationBenefit(const std::vector<ProjectedCashFlow>& portfolio) const {
    // Simplified diversification benefit calculation
    Amount standalone_var = calculateValueAtRisk(0.95);
    
    // Calculate average correlation with portfolio
    double avg_correlation = 0.0;
    if (!portfolio.empty()) {
        for (const auto& asset : portfolio) {
            avg_correlation += calculateCorrelation(asset);
        }
        avg_correlation /= portfolio.size();
    }
    
    // Diversification reduces risk
    Amount diversified_var = standalone_var * std::sqrt(avg_correlation);
    
    return standalone_var - diversified_var;
}

void ProjectedCashFlow::setCashFlowDates(Date start_date, Date end_date) {
    cash_flow_start_date_ = start_date;
    cash_flow_end_date_ = end_date;
    
    // Update projection horizon
    int days = static_cast<int>((end_date - start_date));
    projection_model_.projection_horizon_years = days / 365;
}

void ProjectedCashFlow::generateBasicPaymentSchedule() {
    if (projection_model_.projection_horizon_years <= 0) return;
    
    int payments_per_year = paymentFrequencyToPaymentsPerYear(payment_frequency_);
    int total_payments = projection_model_.projection_horizon_years * payments_per_year;
    Amount payment_amount = total_face_value_ / total_payments;
    
    payment_schedule_.clear();
    Date payment_date = cash_flow_start_date_;
    int days_between_payments = 365 / payments_per_year;
    
    for (int i = 0; i < total_payments; ++i) {
        payment_date = payment_date + days_between_payments;
        ProjectedPayment payment(payment_date, payment_amount);
        payment_schedule_.push_back(payment);
    }
    
    total_payment_count_ = total_payments;
}

// Utility functions implementation

std::string projectedCashFlowTypeToString(ProjectedCashFlowType type) {
    switch (type) {
        case ProjectedCashFlowType::STRUCTURED_SETTLEMENT: return "Structured Settlement";
        case ProjectedCashFlowType::LOTTERY_PAYMENTS: return "Lottery Payments";
        case ProjectedCashFlowType::PENSION_BENEFITS: return "Pension Benefits";
        case ProjectedCashFlowType::ROYALTY_STREAM: return "Royalty Stream";
        case ProjectedCashFlowType::MINERAL_RIGHTS: return "Mineral Rights";
        case ProjectedCashFlowType::FILM_ROYALTIES: return "Film Royalties";
        case ProjectedCashFlowType::MUSIC_ROYALTIES: return "Music Royalties";
        case ProjectedCashFlowType::PATENT_ROYALTIES: return "Patent Royalties";
        case ProjectedCashFlowType::FRANCHISE_FEES: return "Franchise Fees";
        case ProjectedCashFlowType::INSURANCE_SETTLEMENTS: return "Insurance Settlements";
        case ProjectedCashFlowType::DEFERRED_COMPENSATION: return "Deferred Compensation";
        case ProjectedCashFlowType::SPORTS_CONTRACTS: return "Sports Contracts";
        case ProjectedCashFlowType::REAL_ESTATE_RENTS: return "Real Estate Rents";
        case ProjectedCashFlowType::GOVERNMENT_CONTRACTS: return "Government Contracts";
        case ProjectedCashFlowType::UTILITY_PAYMENTS: return "Utility Payments";
        case ProjectedCashFlowType::INFRASTRUCTURE_TOLLS: return "Infrastructure Tolls";
        case ProjectedCashFlowType::AIRPORT_CONCESSIONS: return "Airport Concessions";
        case ProjectedCashFlowType::PARKING_REVENUES: return "Parking Revenues";
        case ProjectedCashFlowType::VENDING_REVENUES: return "Vending Revenues";
        case ProjectedCashFlowType::ATM_REVENUES: return "ATM Revenues";
        case ProjectedCashFlowType::BILLBOARD_REVENUES: return "Billboard Revenues";
        case ProjectedCashFlowType::CELL_TOWER_LEASE: return "Cell Tower Lease";
        case ProjectedCashFlowType::SOLAR_POWER_PPA: return "Solar Power PPA";
        case ProjectedCashFlowType::WIND_POWER_PPA: return "Wind Power PPA";
        case ProjectedCashFlowType::OTHER_PROJECTED_FLOWS: return "Other Projected Flows";
        default: return "Unknown";
    }
}

std::string paymentFrequencyToString(CashFlowFrequency frequency) {
    switch (frequency) {
        case CashFlowFrequency::DAILY: return "Daily";
        case CashFlowFrequency::WEEKLY: return "Weekly";
        case CashFlowFrequency::BIWEEKLY: return "Bi-weekly";
        case CashFlowFrequency::MONTHLY: return "Monthly";
        case CashFlowFrequency::QUARTERLY: return "Quarterly";
        case CashFlowFrequency::SEMI_ANNUAL: return "Semi-annual";
        case CashFlowFrequency::ANNUAL: return "Annual";
        case CashFlowFrequency::IRREGULAR: return "Irregular";
        case CashFlowFrequency::ONE_TIME: return "One-time";
        case CashFlowFrequency::SEASONAL: return "Seasonal";
        case CashFlowFrequency::MILESTONE_BASED: return "Milestone-based";
        default: return "Unknown";
    }
}

std::string cashFlowRiskFactorToString(CashFlowRiskFactor risk_factor) {
    switch (risk_factor) {
        case CashFlowRiskFactor::COUNTERPARTY_RISK: return "Counterparty Risk";
        case CashFlowRiskFactor::REGULATORY_RISK: return "Regulatory Risk";
        case CashFlowRiskFactor::MARKET_RISK: return "Market Risk";
        case CashFlowRiskFactor::OPERATIONAL_RISK: return "Operational Risk";
        case CashFlowRiskFactor::TECHNOLOGY_RISK: return "Technology Risk";
        case CashFlowRiskFactor::LEGAL_RISK: return "Legal Risk";
        case CashFlowRiskFactor::CONCENTRATION_RISK: return "Concentration Risk";
        case CashFlowRiskFactor::LIQUIDITY_RISK: return "Liquidity Risk";
        case CashFlowRiskFactor::INFLATION_RISK: return "Inflation Risk";
        case CashFlowRiskFactor::CURRENCY_RISK: return "Currency Risk";
        case CashFlowRiskFactor::FORCE_MAJEURE_RISK: return "Force Majeure Risk";
        case CashFlowRiskFactor::PERFORMANCE_RISK: return "Performance Risk";
        default: return "Unknown";
    }
}

int paymentFrequencyToPaymentsPerYear(CashFlowFrequency frequency) {
    switch (frequency) {
        case CashFlowFrequency::DAILY: return 365;
        case CashFlowFrequency::WEEKLY: return 52;
        case CashFlowFrequency::BIWEEKLY: return 26;
        case CashFlowFrequency::MONTHLY: return 12;
        case CashFlowFrequency::QUARTERLY: return 4;
        case CashFlowFrequency::SEMI_ANNUAL: return 2;
        case CashFlowFrequency::ANNUAL: return 1;
        case CashFlowFrequency::SEASONAL: return 4;
        case CashFlowFrequency::ONE_TIME: return 1;
        default: return 12; // Default to monthly
    }
}

CashFlowFrequency paymentsPerYearToFrequency(int payments_per_year) {
    switch (payments_per_year) {
        case 365: return CashFlowFrequency::DAILY;
        case 52: return CashFlowFrequency::WEEKLY;
        case 26: return CashFlowFrequency::BIWEEKLY;
        case 12: return CashFlowFrequency::MONTHLY;
        case 4: return CashFlowFrequency::QUARTERLY;
        case 2: return CashFlowFrequency::SEMI_ANNUAL;
        case 1: return CashFlowFrequency::ANNUAL;
        default: return CashFlowFrequency::IRREGULAR;
    }
}

int paymentFrequencyToDays(CashFlowFrequency frequency) {
    return 365 / paymentFrequencyToPaymentsPerYear(frequency);
}

Amount calculatePresentValue(const std::vector<ProjectedPayment>& payments, Rate discount_rate) {
    Amount pv = 0.0;
    Date current_date = Date(); // Today's date in QuantLib
    
    for (const auto& payment : payments) {
        int days_to_payment = static_cast<int>((payment.payment_date - current_date));
        double years_to_payment = days_to_payment / 365.0;
        
        if (years_to_payment > 0) {
            double discount_factor = std::pow(1.0 + discount_rate, -years_to_payment);
            pv += payment.scheduled_amount * payment.certainty_factor * discount_factor;
        }
    }
    
    return pv;
}

Amount calculateFutureValue(const std::vector<ProjectedPayment>& payments, Rate growth_rate, Date future_date) {
    Amount fv = 0.0;
    
    for (const auto& payment : payments) {
        if (payment.payment_date <= future_date) {
            int days_from_payment = static_cast<int>((future_date - payment.payment_date));
            double years_from_payment = days_from_payment / 365.0;
            
            if (years_from_payment >= 0) {
                double growth_factor = std::pow(1.0 + growth_rate, years_from_payment);
                fv += payment.scheduled_amount * payment.certainty_factor * growth_factor;
            }
        }
    }
    
    return fv;
}

double calculateInternalRateOfReturn(const std::vector<ProjectedPayment>& payments, Amount initial_investment) {
    if (initial_investment <= 0 || payments.empty()) return 0.0;
    
    // Newton-Raphson method for IRR calculation
    double irr = 0.1; // Initial guess
    const double tolerance = 1e-8;
    const int max_iterations = 100;
    
    Date current_date = Date(); // Today's date in QuantLib
    
    for (int i = 0; i < max_iterations; ++i) {
        double npv = -initial_investment;
        double npv_derivative = 0.0;
        
        for (const auto& payment : payments) {
            double years = static_cast<double>((payment.payment_date - current_date)) / 365.0;
            if (years > 0) {
                double discount_factor = std::pow(1.0 + irr, -years);
                Amount payment_amount = payment.scheduled_amount * payment.certainty_factor;
                
                npv += payment_amount * discount_factor;
                npv_derivative -= payment_amount * years * discount_factor / (1.0 + irr);
            }
        }
        
        if (std::abs(npv) < tolerance) break;
        if (std::abs(npv_derivative) < tolerance) break;
        
        irr -= npv / npv_derivative;
        
        // Prevent negative IRR
        if (irr < -0.99) irr = -0.99;
    }
    
    return irr;
}

double calculatePaybackPeriod(const std::vector<ProjectedPayment>& payments, Amount initial_investment) {
    Amount cumulative_cash_flow = 0.0;
    Date current_date = Date(); // Today's date in QuantLib
    
    for (const auto& payment : payments) {
        Amount payment_amount = payment.scheduled_amount * payment.certainty_factor;
        cumulative_cash_flow += payment_amount;
        
        if (cumulative_cash_flow >= initial_investment) {
            double years = static_cast<double>((payment.payment_date - current_date)) / 365.0;
            return years;
        }
    }
    
    return -1.0; // Payback period not achieved
}

double calculateCashFlowVolatility(const std::vector<ProjectedPayment>& payments) {
    if (payments.size() < 2) return 0.1; // Default volatility
    
    std::vector<Amount> payment_amounts;
    for (const auto& payment : payments) {
        payment_amounts.push_back(payment.scheduled_amount);
    }
    
    Amount mean = std::accumulate(payment_amounts.begin(), payment_amounts.end(), 0.0) / payment_amounts.size();
    
    double variance = 0.0;
    for (Amount amount : payment_amounts) {
        variance += std::pow(amount - mean, 2);
    }
    variance /= (payment_amounts.size() - 1);
    
    double volatility = std::sqrt(variance) / mean;
    return std::min(0.5, volatility); // Cap at 50%
}

Amount calculateExpectedShortfall(const std::vector<ProjectedPayment>& payments, 
                                Amount total_investment, double confidence_level) {
    Amount expected_value = calculatePresentValue(payments, 0.06); // Use 6% discount rate
    Amount var = expected_value * 0.1; // Simplified 10% volatility assumption
    
    // Calculate expected shortfall (conditional VaR)
    double z_score = (confidence_level >= 0.99) ? 2.326 : 1.645; // 99% or 95%
    Amount expected_shortfall = var * 1.2; // Expected shortfall is typically higher than VaR
    
    return expected_shortfall;
}

double assessCounterpartyRisk(const Obligor& obligor, Amount exposure) {
    // Simplified counterparty risk assessment
    double base_risk = 0.05;
    
    // Adjust based on credit rating
    auto credit_rating = obligor.getStringField("credit_rating");
    if (credit_rating.has_value()) {
        if (credit_rating->find("AAA") != std::string::npos) {
            base_risk = 0.01;
        } else if (credit_rating->find("AA") != std::string::npos) {
            base_risk = 0.02;
        } else if (credit_rating->find("A") != std::string::npos) {
            base_risk = 0.03;
        } else if (credit_rating->find("BBB") != std::string::npos) {
            base_risk = 0.05;
        } else if (credit_rating->find("BB") != std::string::npos) {
            base_risk = 0.10;
        } else {
            base_risk = 0.15;
        }
    }
    
    // Adjust for exposure size
    if (exposure > 1000000) { // Large exposure
        base_risk *= 1.2;
    }
    
    return std::min(0.5, base_risk);
}

double calculatePortfolioCorrelation(const std::vector<ProjectedCashFlow>& portfolio) {
    if (portfolio.size() < 2) return 0.0;
    
    double total_correlation = 0.0;
    int correlation_count = 0;
    
    for (size_t i = 0; i < portfolio.size(); ++i) {
        for (size_t j = i + 1; j < portfolio.size(); ++j) {
            total_correlation += portfolio[i].calculateCorrelation(portfolio[j]);
            correlation_count++;
        }
    }
    
    return (correlation_count > 0) ? (total_correlation / correlation_count) : 0.0;
}

Amount calculatePortfolioDiversificationBenefit(const std::vector<ProjectedCashFlow>& portfolio) {
    Amount total_benefit = 0.0;
    
    for (const auto& asset : portfolio) {
        total_benefit += asset.calculateDiversificationBenefit(portfolio);
    }
    
    return total_benefit;
}

Amount calculatePortfolioValueAtRisk(const std::vector<ProjectedCashFlow>& portfolio, double confidence_level) {
    Amount total_var = 0.0;
    
    for (const auto& asset : portfolio) {
        total_var += asset.calculateValueAtRisk(confidence_level);
    }
    
    // Adjust for correlation (simplified)
    double avg_correlation = calculatePortfolioCorrelation(portfolio);
    total_var *= std::sqrt(avg_correlation);
    
    return total_var;
}

std::map<std::string, Amount> performScenarioAnalysis(const ProjectedCashFlow& asset,
                                                     const std::map<std::string, std::map<std::string, double>>& scenarios) {
    std::map<std::string, Amount> results;
    
    for (const auto& scenario : scenarios) {
        Amount scenario_value = asset.stressTestScenario(scenario.first, scenario.second);
        results[scenario.first] = scenario_value;
    }
    
    return results;
}

Amount calculateStressTestValue(const ProjectedCashFlow& asset, const std::map<std::string, double>& stress_factors) {
    return asset.stressTestScenario("stress_test", stress_factors);
}

} // namespace Structura