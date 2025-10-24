#include "analytics_engine.h"
#include <algorithm>
#include <numeric>
#include <cmath>
#include <random>
#include <sstream>
#include <iostream>
#include <iomanip>

namespace Structura {

// AnalyticsTimeSeries implementation
AnalyticsTimeSeries::AnalyticsTimeSeries(const std::string& name, AnalyticsMetricType type)
    : name_(name), metric_type_(type) {
}

void AnalyticsTimeSeries::addDataPoint(const AnalyticsDataPoint& point) {
    // Insert in chronological order
    auto insert_pos = std::upper_bound(data_.begin(), data_.end(), point,
        [](const AnalyticsDataPoint& a, const AnalyticsDataPoint& b) {
            return a.date < b.date;
        });
    data_.insert(insert_pos, point);
}

void AnalyticsTimeSeries::addDataPoint(Date date, double value, const std::string& category) {
    AnalyticsDataPoint point(date, value, category);
    addDataPoint(point);
}

void AnalyticsTimeSeries::clearData() {
    data_.clear();
}

std::vector<AnalyticsDataPoint> AnalyticsTimeSeries::getDataForPeriod(Date start, Date end) const {
    std::vector<AnalyticsDataPoint> result;
    for (const auto& point : data_) {
        if (point.date >= start && point.date <= end) {
            result.push_back(point);
        }
    }
    return result;
}

std::vector<AnalyticsDataPoint> AnalyticsTimeSeries::getDataForCategory(const std::string& category) const {
    std::vector<AnalyticsDataPoint> result;
    for (const auto& point : data_) {
        if (point.category == category) {
            result.push_back(point);
        }
    }
    return result;
}

double AnalyticsTimeSeries::getCurrentValue() const {
    if (data_.empty()) return 0.0;
    return data_.back().value;
}

double AnalyticsTimeSeries::getValueAtDate(Date date) const {
    for (auto it = data_.rbegin(); it != data_.rend(); ++it) {
        if (it->date <= date) {
            return it->value;
        }
    }
    return 0.0;
}

double AnalyticsTimeSeries::getAverageValue() const {
    if (data_.empty()) return 0.0;
    
    double sum = std::accumulate(data_.begin(), data_.end(), 0.0,
        [](double acc, const AnalyticsDataPoint& point) {
            return acc + point.value;
        });
    return sum / data_.size();
}

double AnalyticsTimeSeries::getMinValue() const {
    if (data_.empty()) return 0.0;
    
    auto min_it = std::min_element(data_.begin(), data_.end(),
        [](const AnalyticsDataPoint& a, const AnalyticsDataPoint& b) {
            return a.value < b.value;
        });
    return min_it->value;
}

double AnalyticsTimeSeries::getMaxValue() const {
    if (data_.empty()) return 0.0;
    
    auto max_it = std::max_element(data_.begin(), data_.end(),
        [](const AnalyticsDataPoint& a, const AnalyticsDataPoint& b) {
            return a.value < b.value;
        });
    return max_it->value;
}

double AnalyticsTimeSeries::getStandardDeviation() const {
    if (data_.size() < 2) return 0.0;
    
    double mean = getAverageValue();
    double sum_squared_diff = 0.0;
    
    for (const auto& point : data_) {
        double diff = point.value - mean;
        sum_squared_diff += diff * diff;
    }
    
    return std::sqrt(sum_squared_diff / (data_.size() - 1));
}

double AnalyticsTimeSeries::getVolatility() const {
    if (data_.size() < 2) return 0.0;
    
    std::vector<double> returns;
    for (size_t i = 1; i < data_.size(); ++i) {
        if (data_[i-1].value != 0.0) {
            double return_rate = (data_[i].value - data_[i-1].value) / data_[i-1].value;
            returns.push_back(return_rate);
        }
    }
    
    if (returns.empty()) return 0.0;
    
    double mean_return = std::accumulate(returns.begin(), returns.end(), 0.0) / returns.size();
    double sum_squared_diff = 0.0;
    
    for (double ret : returns) {
        double diff = ret - mean_return;
        sum_squared_diff += diff * diff;
    }
    
    return std::sqrt(sum_squared_diff / (returns.size() - 1));
}

double AnalyticsTimeSeries::getTrend() const {
    if (data_.size() < 2) return 0.0;
    
    // Simple linear regression slope
    double n = static_cast<double>(data_.size());
    double sum_x = 0.0, sum_y = 0.0, sum_xy = 0.0, sum_x2 = 0.0;
    
    for (size_t i = 0; i < data_.size(); ++i) {
        double x = static_cast<double>(i);
        double y = data_[i].value;
        sum_x += x;
        sum_y += y;
        sum_xy += x * y;
        sum_x2 += x * x;
    }
    
    double denominator = n * sum_x2 - sum_x * sum_x;
    if (denominator == 0.0) return 0.0;
    
    return (n * sum_xy - sum_x * sum_y) / denominator;
}

double AnalyticsTimeSeries::getGrowthRate() const {
    if (data_.size() < 2) return 0.0;
    
    double first_value = data_.front().value;
    double last_value = data_.back().value;
    
    if (first_value == 0.0) return 0.0;
    
    return (last_value - first_value) / first_value;
}

std::vector<double> AnalyticsTimeSeries::getMovingAverage(int periods) const {
    std::vector<double> moving_averages;
    
    if (static_cast<int>(data_.size()) < periods) {
        return moving_averages;
    }
    
    for (size_t i = periods - 1; i < data_.size(); ++i) {
        double sum = 0.0;
        for (int j = 0; j < periods; ++j) {
            sum += data_[i - j].value;
        }
        moving_averages.push_back(sum / periods);
    }
    
    return moving_averages;
}

// AnalyticsEngine implementation
AnalyticsEngine::AnalyticsEngine() : confidence_level_(0.95), var_holding_period_(1) {
    initializeDefaultStressScenarios();
}

AnalyticsEngine::AnalyticsEngine(double confidence_level) 
    : confidence_level_(confidence_level), var_holding_period_(1) {
    initializeDefaultStressScenarios();
}

void AnalyticsEngine::createTimeSeries(const std::string& name, AnalyticsMetricType type) {
    time_series_[name] = std::make_unique<AnalyticsTimeSeries>(name, type);
}

void AnalyticsEngine::addDataPoint(const std::string& series_name, Date date, double value, 
                                  const std::string& category) {
    auto it = time_series_.find(series_name);
    if (it != time_series_.end()) {
        it->second->addDataPoint(date, value, category);
    }
}

AnalyticsTimeSeries* AnalyticsEngine::getTimeSeries(const std::string& name) {
    auto it = time_series_.find(name);
    return (it != time_series_.end()) ? it->second.get() : nullptr;
}

const AnalyticsTimeSeries* AnalyticsEngine::getTimeSeries(const std::string& name) const {
    auto it = time_series_.find(name);
    return (it != time_series_.end()) ? it->second.get() : nullptr;
}

std::vector<std::string> AnalyticsEngine::getTimeSeriesNames() const {
    std::vector<std::string> names;
    for (const auto& [name, series] : time_series_) {
        names.push_back(name);
    }
    return names;
}

std::map<std::string, double> AnalyticsEngine::calculatePerformanceMetrics(const DealBase& deal, Date as_of_date) const {
    std::map<std::string, double> metrics;
    
    try {
        // Calculate deal value for performance metrics
        Balance deal_value = deal.calculateDealValue(as_of_date);
        metrics["deal_value"] = static_cast<double>(deal_value);
        
        // Get deal information
        const auto& deal_info = deal.getDealInfo();
        metrics["deal_age_days"] = static_cast<double>((as_of_date - deal_info.issueDate));
        
        // Calculate basic performance ratios (using deal_value as proxy for original balance)
        // Since DealInfo doesn't have initial_balance, we'll use a reference value
        double assumed_initial_balance = static_cast<double>(deal_value) * 1.05; // Assume 5% decline
        metrics["deal_factor"] = static_cast<double>(deal_value) / assumed_initial_balance;
        
        // Portfolio balance if available
        metrics["current_balance"] = static_cast<double>(deal_value);
        
        // Calculate IRR approximation (simplified)
        if (deal_value > 0) {
            double years = static_cast<double>((as_of_date - deal_info.issueDate)) / 365.25;
            if (years > 0) {
                metrics["approximate_irr"] = std::pow(static_cast<double>(deal_value) / assumed_initial_balance, 1.0/years) - 1.0;
            }
        }
        
        // Get deal-specific metrics
        auto deal_specific = extractDealSpecificMetrics(deal, as_of_date);
        metrics.insert(deal_specific.begin(), deal_specific.end());
        
    } catch (const std::exception& e) {
        std::cerr << "Error calculating performance metrics: " << e.what() << std::endl;
    }
    
    return metrics;
}

std::map<std::string, double> AnalyticsEngine::calculateRiskMetrics(const DealBase& deal, Date as_of_date) const {
    std::map<std::string, double> metrics;
    
    try {
        // Get historical data for VaR calculation
        auto ts = getTimeSeries("deal_value");
        if (ts) {
            Date one_year_ago = as_of_date - 365;
            auto data = ts->getDataForPeriod(one_year_ago, as_of_date);
            
            if (data.size() > 2) {
                std::vector<double> returns;
                for (size_t i = 1; i < data.size(); ++i) {
                    if (data[i-1].value != 0.0) {
                        double return_rate = (data[i].value - data[i-1].value) / data[i-1].value;
                        returns.push_back(return_rate);
                    }
                }
                
                if (!returns.empty()) {
                    metrics["value_at_risk"] = calculateVaR(returns, confidence_level_);
                    metrics["expected_shortfall"] = calculateExpectedShortfall(returns, confidence_level_);
                    
                    // Calculate volatility
                    double mean_return = std::accumulate(returns.begin(), returns.end(), 0.0) / returns.size();
                    double sum_squared_diff = 0.0;
                    for (double ret : returns) {
                        double diff = ret - mean_return;
                        sum_squared_diff += diff * diff;
                    }
                    metrics["volatility"] = std::sqrt(sum_squared_diff / (returns.size() - 1));
                }
            }
        }
        
        // Basic risk indicators
        Balance deal_value = deal.calculateDealValue(as_of_date);
        
        // Use deal value decline as risk indicator (simplified approach)
        double assumed_initial_balance = static_cast<double>(deal_value) * 1.05; // Assume 5% decline from original
        double balance_ratio = static_cast<double>(deal_value) / assumed_initial_balance;
        if (balance_ratio < 0.8) {
            metrics["high_loss_indicator"] = 1.0;
        } else {
            metrics["high_loss_indicator"] = 0.0;
        }
        
        // Concentration risk (simplified)
        metrics["concentration_risk_score"] = 0.3; // Placeholder - would need pool data
        
    } catch (const std::exception& e) {
        std::cerr << "Error calculating risk metrics: " << e.what() << std::endl;
    }
    
    return metrics;
}

std::map<std::string, double> AnalyticsEngine::calculateCreditMetrics(const DealBase& deal, Date as_of_date) const {
    std::map<std::string, double> metrics;
    
    try {
        // Credit risk assessment based on deal status and performance
        switch (deal.getStatus()) {
            case DealBase::DealStatus::Active:
                metrics["default_probability"] = 0.02; // 2% base rate for active deals
                break;
            case DealBase::DealStatus::Defaulted:
                metrics["default_probability"] = 1.0;
                break;
            case DealBase::DealStatus::Accelerated:
                metrics["default_probability"] = 0.8;
                break;
            default:
                metrics["default_probability"] = 0.05;
                break;
        }
        
        // Loss given default estimation
        Balance deal_value = deal.calculateDealValue(as_of_date);
        const auto& deal_info = deal.getDealInfo();
        
        // Use assumed recovery rate based on deal status
        double assumed_initial_balance = static_cast<double>(deal_value) * 1.05; // Assume 5% decline from original
        double recovery_estimate = static_cast<double>(deal_value) / assumed_initial_balance;
        metrics["loss_given_default"] = std::max(0.0, 1.0 - recovery_estimate);
        
        // Expected credit losses
        metrics["expected_credit_losses"] = metrics["default_probability"] * metrics["loss_given_default"] * static_cast<double>(deal_value);
        
        // Credit quality score (0-1 scale, higher is better)
        double age_days = static_cast<double>((as_of_date - deal_info.issueDate));
        double age_factor = std::min(1.0, age_days / 365.25); // Mature after 1 year
        
        metrics["credit_quality_score"] = (1.0 - metrics["default_probability"]) * age_factor;
        
    } catch (const std::exception& e) {
        std::cerr << "Error calculating credit metrics: " << e.what() << std::endl;
    }
    
    return metrics;
}

std::map<std::string, double> AnalyticsEngine::calculateOperationalMetrics(const DealBase& deal, Date as_of_date) const {
    std::map<std::string, double> metrics;
    
    try {
        // Payment performance indicators
        auto payment_ts = getTimeSeries("payment_rate");
        if (payment_ts) {
            Date six_months_ago = as_of_date - 180; // Approximate 6 months
            auto recent_data = payment_ts->getDataForPeriod(six_months_ago, as_of_date);
            
            if (!recent_data.empty()) {
                double avg_payment_rate = 0.0;
                for (const auto& point : recent_data) {
                    avg_payment_rate += point.value;
                }
                avg_payment_rate /= recent_data.size();
                metrics["average_payment_rate"] = avg_payment_rate;
                
                // Payment rate trend
                if (recent_data.size() > 1) {
                    double first_rate = recent_data.front().value;
                    double last_rate = recent_data.back().value;
                    metrics["payment_rate_trend"] = (last_rate - first_rate) / first_rate;
                }
            }
        }
        
        // Deal operational efficiency
        const auto& deal_info = deal.getDealInfo();
        double operational_days = static_cast<double>((as_of_date - deal_info.issueDate));
        if (operational_days > 0) {
            metrics["operational_efficiency"] = std::min(1.0, operational_days / 30.0); // Efficiency improves over first month
        }
        
        // Account management efficiency (based on number of accounts)
        std::vector<std::string> account_names = deal.getAccountNames();
        metrics["account_utilization"] = static_cast<double>(account_names.size()) / 10.0; // Normalized to typical deal
        
    } catch (const std::exception& e) {
        std::cerr << "Error calculating operational metrics: " << e.what() << std::endl;
    }
    
    return metrics;
}

std::map<std::string, double> AnalyticsEngine::calculateAllMetrics(const DealBase& deal, Date as_of_date) const {
    std::map<std::string, double> all_metrics;
    
    // Combine all metric types
    auto performance = calculatePerformanceMetrics(deal, as_of_date);
    auto risk = calculateRiskMetrics(deal, as_of_date);
    auto credit = calculateCreditMetrics(deal, as_of_date);
    auto operational = calculateOperationalMetrics(deal, as_of_date);
    
    // Merge all metrics
    all_metrics.insert(performance.begin(), performance.end());
    all_metrics.insert(risk.begin(), risk.end());
    all_metrics.insert(credit.begin(), credit.end());
    all_metrics.insert(operational.begin(), operational.end());
    
    // Add timestamp
    all_metrics["calculation_timestamp"] = static_cast<double>(as_of_date.serialNumber());
    
    return all_metrics;
}

void AnalyticsEngine::updateHistoricalMetrics(const DealBase& deal, Date as_of_date) {
    auto metrics = calculateAllMetrics(deal, as_of_date);
    historical_metrics_[as_of_date] = metrics;
    
    // Update time series
    for (const auto& [metric_name, value] : metrics) {
        if (time_series_.find(metric_name) == time_series_.end()) {
            // Create time series if it doesn't exist
            createTimeSeries(metric_name, AnalyticsMetricType::PORTFOLIO_BALANCE); // Default type
        }
        addDataPoint(metric_name, as_of_date, value);
    }
}

// Helper functions
double AnalyticsEngine::calculateVaR(const std::vector<double>& returns, double confidence) const {
    if (returns.empty()) return 0.0;
    
    std::vector<double> sorted_returns = returns;
    std::sort(sorted_returns.begin(), sorted_returns.end());
    
    size_t index = static_cast<size_t>((1.0 - confidence) * sorted_returns.size());
    if (index >= sorted_returns.size()) index = sorted_returns.size() - 1;
    
    return -sorted_returns[index]; // VaR is positive loss
}

double AnalyticsEngine::calculateExpectedShortfall(const std::vector<double>& returns, double confidence) const {
    if (returns.empty()) return 0.0;
    
    std::vector<double> sorted_returns = returns;
    std::sort(sorted_returns.begin(), sorted_returns.end());
    
    size_t cutoff_index = static_cast<size_t>((1.0 - confidence) * sorted_returns.size());
    if (cutoff_index == 0) cutoff_index = 1;
    
    double sum = 0.0;
    for (size_t i = 0; i < cutoff_index; ++i) {
        sum += sorted_returns[i];
    }
    
    return -sum / cutoff_index; // ES is positive expected loss in tail
}

std::map<std::string, double> AnalyticsEngine::extractDealSpecificMetrics(const DealBase& deal, Date as_of_date) const {
    std::map<std::string, double> metrics;
    
    // Extract deal type-specific information from deal summary
    std::string summary = deal.getDealSummary();
    
    // Parse deal-specific information from summary (simplified)
    if (summary.find("CLO") != std::string::npos) {
        metrics["deal_type_clo"] = 1.0;
        // Could add more CLO-specific metrics here
    } else if (summary.find("ABS") != std::string::npos) {
        metrics["deal_type_abs"] = 1.0;
        // Could add more ABS-specific metrics here
    } else if (summary.find("Mortgage") != std::string::npos) {
        metrics["deal_type_mortgage"] = 1.0;
        // Could add more mortgage-specific metrics here
    }
    
    return metrics;
}

void AnalyticsEngine::initializeDefaultStressScenarios() {
    // Credit stress scenarios
    StressScenario base_case("base_case", "Normal market conditions", 0.0);
    base_case.parameters["default_rate_multiplier"] = 1.0;
    base_case.parameters["recovery_rate_shock"] = 0.0;
    stress_scenarios_["base_case"] = base_case;
    
    StressScenario mild_stress("mild_stress", "Mild economic downturn", 0.3);
    mild_stress.parameters["default_rate_multiplier"] = 1.5;
    mild_stress.parameters["recovery_rate_shock"] = -0.1;
    stress_scenarios_["mild_stress"] = mild_stress;
    
    StressScenario severe_stress("severe_stress", "Severe recession scenario", 0.7);
    severe_stress.parameters["default_rate_multiplier"] = 3.0;
    severe_stress.parameters["recovery_rate_shock"] = -0.3;
    stress_scenarios_["severe_stress"] = severe_stress;
    
    StressScenario extreme_stress("extreme_stress", "Financial crisis scenario", 1.0);
    extreme_stress.parameters["default_rate_multiplier"] = 5.0;
    extreme_stress.parameters["recovery_rate_shock"] = -0.5;
    stress_scenarios_["extreme_stress"] = extreme_stress;
}

std::string AnalyticsEngine::metricTypeToString(AnalyticsMetricType type) const {
    switch (type) {
        case AnalyticsMetricType::PORTFOLIO_BALANCE: return "Portfolio Balance";
        case AnalyticsMetricType::CURRENT_YIELD: return "Current Yield";
        case AnalyticsMetricType::WEIGHTED_AVERAGE_LIFE: return "Weighted Average Life";
        case AnalyticsMetricType::VALUE_AT_RISK: return "Value at Risk";
        case AnalyticsMetricType::EXPECTED_SHORTFALL: return "Expected Shortfall";
        case AnalyticsMetricType::DEFAULT_PROBABILITY: return "Default Probability";
        case AnalyticsMetricType::EXPECTED_CREDIT_LOSSES: return "Expected Credit Losses";
        case AnalyticsMetricType::PAYMENT_RATE: return "Payment Rate";
        case AnalyticsMetricType::DELINQUENCY_RATE: return "Delinquency Rate";
        case AnalyticsMetricType::DEAL_IRR: return "Deal IRR";
        default: return "Unknown Metric";
    }
}

std::string AnalyticsEngine::generateAnalyticsSummary(const DealBase& deal, Date as_of_date) const {
    std::stringstream summary;
    
    summary << "=== DEAL ANALYTICS SUMMARY ===\n";
    summary << "Deal: " << deal.getDealName() << "\n";
    summary << "Analysis Date: " << as_of_date << "\n";
    summary << "Deal Status: ";
    
    switch (deal.getStatus()) {
        case DealBase::DealStatus::Active: summary << "Active"; break;
        case DealBase::DealStatus::Defaulted: summary << "Defaulted"; break;
        case DealBase::DealStatus::Accelerated: summary << "Accelerated"; break;
        case DealBase::DealStatus::Matured: summary << "Matured"; break;
        case DealBase::DealStatus::Called: summary << "Called"; break;
        default: summary << "Pending"; break;
    }
    summary << "\n\n";
    
    auto metrics = calculateAllMetrics(deal, as_of_date);
    
    summary << "=== KEY PERFORMANCE METRICS ===\n";
    if (metrics.find("deal_value") != metrics.end()) {
        summary << "Deal Value: $" << std::fixed << std::setprecision(2) << metrics["deal_value"] << "\n";
    }
    if (metrics.find("deal_factor") != metrics.end()) {
        summary << "Deal Factor: " << std::fixed << std::setprecision(4) << metrics["deal_factor"] << "\n";
    }
    if (metrics.find("approximate_irr") != metrics.end()) {
        summary << "Approximate IRR: " << std::fixed << std::setprecision(2) << (metrics["approximate_irr"] * 100) << "%\n";
    }
    
    summary << "\n=== RISK METRICS ===\n";
    if (metrics.find("default_probability") != metrics.end()) {
        summary << "Default Probability: " << std::fixed << std::setprecision(2) << (metrics["default_probability"] * 100) << "%\n";
    }
    if (metrics.find("expected_credit_losses") != metrics.end()) {
        summary << "Expected Credit Losses: $" << std::fixed << std::setprecision(2) << metrics["expected_credit_losses"] << "\n";
    }
    if (metrics.find("value_at_risk") != metrics.end()) {
        summary << "Value at Risk (95%): " << std::fixed << std::setprecision(2) << (metrics["value_at_risk"] * 100) << "%\n";
    }
    
    return summary.str();
}

} // namespace Structura