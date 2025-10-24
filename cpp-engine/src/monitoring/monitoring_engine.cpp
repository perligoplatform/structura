#include "monitoring_engine.h"
#include <iostream>
#include <sstream>
#include <iomanip>
#include <algorithm>
#include <random>
#include <ctime>

namespace Structura {

MonitoringEngine::MonitoringEngine() 
    : monitoring_active_(false), 
      default_monitoring_interval_(std::chrono::minutes(5)),
      anomaly_detection_enabled_(false),
      last_health_check_(std::chrono::steady_clock::now()) {
    
    // Initialize basic health indicators
    health_indicators_["system_startup"] = true;
    health_indicators_["alert_processing"] = true;
    health_indicators_["notification_system"] = true;
    health_indicators_["escalation_system"] = true;
    health_indicators_["data_connectivity"] = true;
}

MonitoringEngine::MonitoringEngine(std::shared_ptr<AnalyticsEngine> analytics,
                                   std::shared_ptr<ValidationEngine> validation)
    : MonitoringEngine() {
    analytics_engine_ = analytics;
    validation_engine_ = validation;
}

MonitoringEngine::~MonitoringEngine() {
    stopMonitoring();
}

void MonitoringEngine::startMonitoring() {
    if (monitoring_active_) {
        return;
    }
    
    monitoring_active_ = true;
    
    // Start monitoring thread
    monitoring_thread_ = std::thread(&MonitoringEngine::monitoringLoop, this);
    
    // Start alert processing thread
    alert_processor_thread_ = std::thread(&MonitoringEngine::processAlertQueue, this);
    
    std::cout << "Monitoring system started" << std::endl;
}

void MonitoringEngine::stopMonitoring() {
    if (!monitoring_active_) {
        return;
    }
    
    monitoring_active_ = false;
    
    // Notify threads to stop
    alert_condition_.notify_all();
    queue_condition_.notify_all();
    
    // Wait for threads to finish
    if (monitoring_thread_.joinable()) {
        monitoring_thread_.join();
    }
    if (alert_processor_thread_.joinable()) {
        alert_processor_thread_.join();
    }
    
    std::cout << "Monitoring system stopped" << std::endl;
}

bool MonitoringEngine::isMonitoringActive() const {
    return monitoring_active_;
}

void MonitoringEngine::setAnalyticsEngine(std::shared_ptr<AnalyticsEngine> analytics) {
    analytics_engine_ = analytics;
}

void MonitoringEngine::setValidationEngine(std::shared_ptr<ValidationEngine> validation) {
    validation_engine_ = validation;
}

void MonitoringEngine::setMonitoringInterval(std::chrono::seconds interval) {
    default_monitoring_interval_ = interval;
}

void MonitoringEngine::addAlertRule(const AlertRule& rule) {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    alert_rules_[rule.rule_id] = rule;
}

void MonitoringEngine::updateAlertRule(const std::string& rule_id, const AlertRule& rule) {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    if (alert_rules_.find(rule_id) != alert_rules_.end()) {
        alert_rules_[rule_id] = rule;
    }
}

void MonitoringEngine::removeAlertRule(const std::string& rule_id) {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    alert_rules_.erase(rule_id);
}

void MonitoringEngine::enableAlertRule(const std::string& rule_id) {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    if (alert_rules_.find(rule_id) != alert_rules_.end()) {
        alert_rules_[rule_id].is_active = true;
    }
}

void MonitoringEngine::disableAlertRule(const std::string& rule_id) {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    if (alert_rules_.find(rule_id) != alert_rules_.end()) {
        alert_rules_[rule_id].is_active = false;
    }
}

AlertRule MonitoringEngine::getAlertRule(const std::string& rule_id) const {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    auto it = alert_rules_.find(rule_id);
    if (it != alert_rules_.end()) {
        return it->second;
    }
    return AlertRule();
}

std::vector<AlertRule> MonitoringEngine::getAlertRules() const {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    std::vector<AlertRule> rules;
    for (const auto& pair : alert_rules_) {
        rules.push_back(pair.second);
    }
    return rules;
}

std::vector<AlertRule> MonitoringEngine::getAlertRulesByCategory(AlertCategory category) const {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    std::vector<AlertRule> rules;
    for (const auto& pair : alert_rules_) {
        if (pair.second.category == category) {
            rules.push_back(pair.second);
        }
    }
    return rules;
}

void MonitoringEngine::addDealToMonitoring(std::shared_ptr<DealBase> deal) {
    if (deal) {
        // Use a simple counter or generate ID since DealBase doesn't have getId()
        std::string deal_id = "DEAL_" + std::to_string(monitored_deals_.size() + 1);
        monitored_deals_[deal_id] = deal;
    }
}

void MonitoringEngine::removeDealFromMonitoring(const std::string& deal_id) {
    monitored_deals_.erase(deal_id);
}

void MonitoringEngine::evaluateDeal(const DealBase& deal) {
    evaluateAlertRules(deal);
}

std::vector<std::string> MonitoringEngine::getMonitoredDeals() const {
    std::vector<std::string> deal_ids;
    for (const auto& pair : monitored_deals_) {
        deal_ids.push_back(pair.first);
    }
    return deal_ids;
}

std::vector<Alert> MonitoringEngine::getActiveAlerts() const {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    std::vector<Alert> alerts;
    for (const auto& pair : active_alerts_) {
        if (pair.second.status == AlertStatus::ACTIVE) {
            alerts.push_back(pair.second);
        }
    }
    return alerts;
}

std::vector<Alert> MonitoringEngine::getAlertsByDeal(const std::string& deal_id) const {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    std::vector<Alert> alerts;
    for (const auto& pair : active_alerts_) {
        if (pair.second.deal_id == deal_id) {
            alerts.push_back(pair.second);
        }
    }
    return alerts;
}

std::vector<Alert> MonitoringEngine::getAlertsByCategory(AlertCategory category) const {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    std::vector<Alert> alerts;
    for (const auto& pair : active_alerts_) {
        if (pair.second.category == category) {
            alerts.push_back(pair.second);
        }
    }
    return alerts;
}

std::vector<Alert> MonitoringEngine::getAlertsBySeverity(AlertSeverity severity) const {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    std::vector<Alert> alerts;
    for (const auto& pair : active_alerts_) {
        if (pair.second.severity == severity) {
            alerts.push_back(pair.second);
        }
    }
    return alerts;
}

Alert MonitoringEngine::getAlert(const std::string& alert_id) const {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    auto it = active_alerts_.find(alert_id);
    if (it != active_alerts_.end()) {
        return it->second;
    }
    return Alert();
}

void MonitoringEngine::acknowledgeAlert(const std::string& alert_id, const std::string& acknowledged_by) {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    auto it = active_alerts_.find(alert_id);
    if (it != active_alerts_.end()) {
        it->second.status = AlertStatus::ACKNOWLEDGED;
        it->second.acknowledged_by = acknowledged_by;
        it->second.acknowledged_date = Date::todaysDate();
        it->second.last_updated = Date::todaysDate();
    }
}

void MonitoringEngine::assignAlert(const std::string& alert_id, const std::string& assigned_to) {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    auto it = active_alerts_.find(alert_id);
    if (it != active_alerts_.end()) {
        it->second.assigned_to = assigned_to;
        it->second.last_updated = Date::todaysDate();
        if (it->second.status == AlertStatus::ACTIVE) {
            it->second.status = AlertStatus::INVESTIGATING;
        }
    }
}

void MonitoringEngine::addResponseAction(const std::string& alert_id, const std::string& action) {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    auto it = active_alerts_.find(alert_id);
    if (it != active_alerts_.end()) {
        it->second.response_actions.push_back(action);
        it->second.last_updated = Date::todaysDate();
    }
}

void MonitoringEngine::resolveAlert(const std::string& alert_id, const std::string& resolution_notes) {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    auto it = active_alerts_.find(alert_id);
    if (it != active_alerts_.end()) {
        it->second.status = AlertStatus::RESOLVED;
        it->second.resolution_notes = resolution_notes;
        it->second.resolved_date = Date::todaysDate();
        it->second.last_updated = Date::todaysDate();
        
        // Move to history
        alert_history_.push_back(it->second);
        active_alerts_.erase(it);
    }
}

void MonitoringEngine::escalateAlert(const std::string& alert_id) {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    auto it = active_alerts_.find(alert_id);
    if (it != active_alerts_.end()) {
        it->second.escalation_level++;
        it->second.last_escalation_date = Date::todaysDate();
        it->second.escalation_history.push_back(Date::todaysDate());
        it->second.status = AlertStatus::ESCALATED;
        it->second.last_updated = Date::todaysDate();
    }
}

void MonitoringEngine::suppressAlert(const std::string& alert_id, std::chrono::seconds duration, const std::string& reason) {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    auto it = active_alerts_.find(alert_id);
    if (it != active_alerts_.end()) {
        it->second.status = AlertStatus::SUPPRESSED;
        it->second.last_updated = Date::todaysDate();
        
        // Create suppression rule
        SuppressionRule suppression;
        suppression.rule_id = "SUPP_" + alert_id;
        suppression.alert_rule_id = it->second.rule_id;
        suppression.suppression_duration = duration;
        suppression.start_date = Date::todaysDate();
        suppression.reason = reason;
        suppression.is_active = true;
        
        suppression_rules_[suppression.rule_id] = suppression;
    }
}

void MonitoringEngine::configureNotificationChannel(NotificationChannel channel, const NotificationConfig& config) {
    notification_configs_[channel] = config;
}

void MonitoringEngine::addNotificationRecipient(const std::string& alert_rule_id, const std::string& recipient) {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    auto it = alert_rules_.find(alert_rule_id);
    if (it != alert_rules_.end()) {
        it->second.recipients.push_back(recipient);
    }
}

void MonitoringEngine::removeNotificationRecipient(const std::string& alert_rule_id, const std::string& recipient) {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    auto it = alert_rules_.find(alert_rule_id);
    if (it != alert_rules_.end()) {
        auto& recipients = it->second.recipients;
        recipients.erase(std::remove(recipients.begin(), recipients.end(), recipient), recipients.end());
    }
}

void MonitoringEngine::testNotificationChannel(NotificationChannel channel, const std::string& test_message) {
    // This would integrate with actual notification systems
    std::cout << "Testing " << notificationChannelToString(channel) << " channel: " << test_message << std::endl;
}

void MonitoringEngine::addEscalationRule(const EscalationRule& rule) {
    escalation_rules_.push_back(rule);
}

void MonitoringEngine::updateEscalationRule(const std::string& rule_id, const EscalationRule& rule) {
    for (auto& existing_rule : escalation_rules_) {
        if (existing_rule.rule_id == rule_id) {
            existing_rule = rule;
            break;
        }
    }
}

void MonitoringEngine::removeEscalationRule(const std::string& rule_id) {
    escalation_rules_.erase(
        std::remove_if(escalation_rules_.begin(), escalation_rules_.end(),
                      [&rule_id](const EscalationRule& rule) { return rule.rule_id == rule_id; }),
        escalation_rules_.end());
}

std::vector<EscalationRule> MonitoringEngine::getEscalationRules() const {
    return escalation_rules_;
}

void MonitoringEngine::addSuppressionRule(const SuppressionRule& rule) {
    suppression_rules_[rule.rule_id] = rule;
}

void MonitoringEngine::removeSuppressionRule(const std::string& rule_id) {
    suppression_rules_.erase(rule_id);
}

std::vector<SuppressionRule> MonitoringEngine::getActiveSuppressions() const {
    std::vector<SuppressionRule> active_suppressions;
    for (const auto& pair : suppression_rules_) {
        if (pair.second.is_active) {
            active_suppressions.push_back(pair.second);
        }
    }
    return active_suppressions;
}

MonitoringDashboard MonitoringEngine::generateDashboard() const {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    
    MonitoringDashboard dashboard;
    dashboard.last_updated = Date::todaysDate();
    
    // Initialize counters
    for (int i = static_cast<int>(AlertSeverity::INFO); i <= static_cast<int>(AlertSeverity::EMERGENCY); ++i) {
        dashboard.alert_counts_by_severity[static_cast<AlertSeverity>(i)] = 0;
    }
    
    for (int i = static_cast<int>(AlertCategory::COVENANT_BREACH); i <= static_cast<int>(AlertCategory::CUSTOM_ALERT); ++i) {
        dashboard.alert_counts_by_category[static_cast<AlertCategory>(i)] = 0;
    }
    
    for (int i = static_cast<int>(AlertStatus::ACTIVE); i <= static_cast<int>(AlertStatus::CLOSED); ++i) {
        dashboard.alert_counts_by_status[static_cast<AlertStatus>(i)] = 0;
    }
    
    // Count alerts
    for (const auto& pair : active_alerts_) {
        const Alert& alert = pair.second;
        dashboard.alert_counts_by_severity[alert.severity]++;
        dashboard.alert_counts_by_category[alert.category]++;
        dashboard.alert_counts_by_status[alert.status]++;
        
        // Add to recent alerts (last 10)
        if (dashboard.recent_alerts.size() < 10) {
            dashboard.recent_alerts.push_back(alert);
        }
        
        // Add critical alerts
        if (alert.severity == AlertSeverity::CRITICAL || alert.severity == AlertSeverity::EMERGENCY) {
            dashboard.critical_alerts.push_back(alert);
        }
    }
    
    // Sort recent alerts by date
    std::sort(dashboard.recent_alerts.begin(), dashboard.recent_alerts.end(),
             [](const Alert& a, const Alert& b) { return a.trigger_date > b.trigger_date; });
    
    // Add key metrics
    dashboard.key_metrics["total_active_alerts"] = static_cast<double>(active_alerts_.size());
    dashboard.key_metrics["critical_alerts"] = static_cast<double>(dashboard.critical_alerts.size());
    dashboard.key_metrics["monitored_deals"] = static_cast<double>(monitored_deals_.size());
    dashboard.key_metrics["alert_rules"] = static_cast<double>(alert_rules_.size());
    
    // System health indicators
    dashboard.system_health_indicators = getSystemHealthStatus();
    
    return dashboard;
}

std::map<std::string, int> MonitoringEngine::getAlertStatistics() const {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    
    std::map<std::string, int> stats;
    stats["total_active"] = static_cast<int>(active_alerts_.size());
    stats["total_rules"] = static_cast<int>(alert_rules_.size());
    stats["total_history"] = static_cast<int>(alert_history_.size());
    
    // Count by severity
    for (const auto& pair : active_alerts_) {
        std::string severity_key = "severity_" + alertSeverityToString(pair.second.severity);
        stats[severity_key]++;
    }
    
    // Count by category
    for (const auto& pair : active_alerts_) {
        std::string category_key = "category_" + alertCategoryToString(pair.second.category);
        stats[category_key]++;
    }
    
    return stats;
}

std::vector<Alert> MonitoringEngine::getRecentAlerts(int limit) const {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    
    std::vector<Alert> recent_alerts;
    for (const auto& pair : active_alerts_) {
        recent_alerts.push_back(pair.second);
    }
    
    // Sort by trigger date (most recent first)
    std::sort(recent_alerts.begin(), recent_alerts.end(),
             [](const Alert& a, const Alert& b) { return a.trigger_date > b.trigger_date; });
    
    // Limit results
    if (recent_alerts.size() > static_cast<size_t>(limit)) {
        recent_alerts.resize(limit);
    }
    
    return recent_alerts;
}

std::vector<Alert> MonitoringEngine::getAlertHistory(Date start_date, Date end_date) const {
    std::vector<Alert> filtered_alerts;
    for (const Alert& alert : alert_history_) {
        if (alert.trigger_date >= start_date && alert.trigger_date <= end_date) {
            filtered_alerts.push_back(alert);
        }
    }
    return filtered_alerts;
}

std::vector<std::string> MonitoringEngine::getSystemHealthStatus() const {
    std::lock_guard<std::mutex> lock(health_mutex_);
    
    std::vector<std::string> status_messages;
    
    for (const auto& indicator : health_indicators_) {
        std::string status = indicator.second ? "OK" : "ERROR";
        status_messages.push_back(indicator.first + ": " + status);
    }
    
    // Check system uptime
    auto now = std::chrono::steady_clock::now();
    auto uptime = std::chrono::duration_cast<std::chrono::minutes>(now - last_health_check_);
    status_messages.push_back("system_uptime: " + std::to_string(uptime.count()) + " minutes");
    
    // Check monitoring status
    status_messages.push_back("monitoring_active: " + std::string(monitoring_active_ ? "true" : "false"));
    
    return status_messages;
}

std::map<std::string, double> MonitoringEngine::getMonitoringMetrics() const {
    std::map<std::string, double> metrics;
    
    metrics["alerts_processed_per_minute"] = 0.0; // Would track actual processing rate
    metrics["average_alert_resolution_time"] = 0.0; // In minutes
    metrics["system_cpu_usage"] = 0.0; // System monitoring integration
    metrics["memory_usage_mb"] = 0.0; // Memory monitoring
    metrics["disk_usage_percent"] = 0.0; // Disk space monitoring
    
    return metrics;
}

void MonitoringEngine::performHealthCheck() {
    std::lock_guard<std::mutex> lock(health_mutex_);
    
    last_health_check_ = std::chrono::steady_clock::now();
    
    // Check alert processing
    health_indicators_["alert_processing"] = monitoring_active_;
    
    // Check dependencies
    health_indicators_["analytics_engine"] = (analytics_engine_ != nullptr);
    health_indicators_["validation_engine"] = (validation_engine_ != nullptr);
    
    // Check system resources (simplified)
    health_indicators_["memory_available"] = true; // Would check actual memory
    health_indicators_["disk_space"] = true; // Would check actual disk space
    
    // Update overall system health
    bool overall_health = true;
    for (const auto& indicator : health_indicators_) {
        if (!indicator.second) {
            overall_health = false;
            break;
        }
    }
    health_indicators_["overall_system_health"] = overall_health;
}

void MonitoringEngine::enableAnomalyDetection(bool enable) {
    anomaly_detection_enabled_ = enable;
}

void MonitoringEngine::configureTrendAnalysis(const std::string& metric_name, int lookback_periods) {
    trend_analysis_config_[metric_name] = lookback_periods;
}

void MonitoringEngine::addCustomMetricCalculator(const std::string& metric_name, 
                                                std::function<double(const DealBase&)> calculator) {
    custom_calculators_[metric_name] = calculator;
}

void MonitoringEngine::acknowledgeMultipleAlerts(const std::vector<std::string>& alert_ids, const std::string& acknowledged_by) {
    for (const std::string& alert_id : alert_ids) {
        acknowledgeAlert(alert_id, acknowledged_by);
    }
}

void MonitoringEngine::resolveMultipleAlerts(const std::vector<std::string>& alert_ids, const std::string& resolution_notes) {
    for (const std::string& alert_id : alert_ids) {
        resolveAlert(alert_id, resolution_notes);
    }
}

void MonitoringEngine::bulkUpdateAlertStatus(const std::vector<std::string>& alert_ids, AlertStatus new_status) {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    for (const std::string& alert_id : alert_ids) {
        auto it = active_alerts_.find(alert_id);
        if (it != active_alerts_.end()) {
            it->second.status = new_status;
            it->second.last_updated = Date::todaysDate();
        }
    }
}

std::string MonitoringEngine::exportAlertsToJSON(Date start_date, Date end_date) const {
    std::ostringstream json;
    json << "{\n";
    json << "  \"export_date\": \"" << Date::todaysDate() << "\",\n";
    json << "  \"date_range\": {\n";
    json << "    \"start\": \"" << start_date << "\",\n";
    json << "    \"end\": \"" << end_date << "\"\n";
    json << "  },\n";
    json << "  \"alerts\": [\n";
    
    std::vector<Alert> filtered_alerts = getAlertHistory(start_date, end_date);
    for (size_t i = 0; i < filtered_alerts.size(); ++i) {
        const Alert& alert = filtered_alerts[i];
        json << "    {\n";
        json << "      \"alert_id\": \"" << alert.alert_id << "\",\n";
        json << "      \"rule_id\": \"" << alert.rule_id << "\",\n";
        json << "      \"deal_id\": \"" << alert.deal_id << "\",\n";
        json << "      \"title\": \"" << alert.title << "\",\n";
        json << "      \"category\": \"" << alertCategoryToString(alert.category) << "\",\n";
        json << "      \"severity\": \"" << alertSeverityToString(alert.severity) << "\",\n";
        json << "      \"status\": \"" << alertStatusToString(alert.status) << "\",\n";
        json << "      \"trigger_date\": \"" << alert.trigger_date << "\",\n";
        json << "      \"metric_value\": " << alert.metric_value << ",\n";
        json << "      \"threshold_value\": " << alert.threshold_value << "\n";
        json << "    }";
        if (i < filtered_alerts.size() - 1) json << ",";
        json << "\n";
    }
    
    json << "  ]\n";
    json << "}";
    
    return json.str();
}

std::string MonitoringEngine::exportAlertRulesToJSON() const {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    
    std::ostringstream json;
    json << "{\n";
    json << "  \"export_date\": \"" << Date::todaysDate() << "\",\n";
    json << "  \"alert_rules\": [\n";
    
    size_t i = 0;
    for (const auto& pair : alert_rules_) {
        const AlertRule& rule = pair.second;
        json << "    {\n";
        json << "      \"rule_id\": \"" << rule.rule_id << "\",\n";
        json << "      \"name\": \"" << rule.name << "\",\n";
        json << "      \"description\": \"" << rule.description << "\",\n";
        json << "      \"category\": \"" << alertCategoryToString(rule.category) << "\",\n";
        json << "      \"severity\": \"" << alertSeverityToString(rule.severity) << "\",\n";
        json << "      \"metric_name\": \"" << rule.metric_name << "\",\n";
        json << "      \"threshold_value\": " << rule.threshold_value << ",\n";
        json << "      \"comparison_operator\": \"" << rule.comparison_operator << "\",\n";
        json << "      \"is_active\": " << (rule.is_active ? "true" : "false") << "\n";
        json << "    }";
        if (i < alert_rules_.size() - 1) json << ",";
        json << "\n";
        ++i;
    }
    
    json << "  ]\n";
    json << "}";
    
    return json.str();
}

void MonitoringEngine::importAlertRulesFromJSON(const std::string& json_data) {
    // This would parse JSON and create AlertRule objects
    // For now, just log the import attempt
    std::cout << "Importing alert rules from JSON data (length: " << json_data.length() << ")" << std::endl;
}

// Private implementation methods

void MonitoringEngine::monitoringLoop() {
    while (monitoring_active_) {
        try {
            // Evaluate all monitored deals
            for (const auto& deal_pair : monitored_deals_) {
                if (!monitoring_active_) break;
                evaluateAlertRules(*deal_pair.second);
            }
            
            // Process escalations
            processEscalations();
            
            // Clean up expired suppressions
            cleanupExpiredSuppressions();
            
            // Perform periodic health check
            performHealthCheck();
            
            // Wait for next monitoring cycle
            std::unique_lock<std::mutex> lock(alert_mutex_);
            alert_condition_.wait_for(lock, default_monitoring_interval_,
                                    [this] { return !monitoring_active_; });
            
        } catch (const std::exception& e) {
            std::cerr << "Error in monitoring loop: " << e.what() << std::endl;
        }
    }
}

void MonitoringEngine::processAlertQueue() {
    while (monitoring_active_) {
        std::unique_lock<std::mutex> lock(queue_mutex_);
        queue_condition_.wait(lock, [this] { return !alert_queue_.empty() || !monitoring_active_; });
        
        while (!alert_queue_.empty() && monitoring_active_) {
            Alert alert = alert_queue_.front();
            alert_queue_.pop();
            lock.unlock();
            
            try {
                // Process the alert (send notifications, etc.)
                auto rule_it = alert_rules_.find(alert.rule_id);
                if (rule_it != alert_rules_.end()) {
                    for (NotificationChannel channel : rule_it->second.notification_channels) {
                        sendNotification(alert, channel);
                    }
                }
                
                // Store the alert
                {
                    std::lock_guard<std::mutex> alert_lock(alert_mutex_);
                    active_alerts_[alert.alert_id] = alert;
                }
                
            } catch (const std::exception& e) {
                std::cerr << "Error processing alert: " << e.what() << std::endl;
            }
            
            lock.lock();
        }
    }
}

void MonitoringEngine::evaluateAlertRules(const DealBase& deal) {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    
    for (const auto& rule_pair : alert_rules_) {
        const AlertRule& rule = rule_pair.second;
        
        if (!rule.is_active) continue;
        if (isAlertSuppressed(rule.rule_id)) continue;
        
        try {
            if (evaluateAlertRule(rule, deal)) {
                double metric_value = getMetricValue(deal, rule.metric_name);
                triggerAlert(rule, deal, metric_value);
            }
        } catch (const std::exception& e) {
            std::cerr << "Error evaluating rule " << rule.rule_id << ": " << e.what() << std::endl;
        }
    }
}

bool MonitoringEngine::evaluateAlertRule(const AlertRule& rule, const DealBase& deal) {
    double metric_value = getMetricValue(deal, rule.metric_name);
    return compareValues(metric_value, rule.threshold_value, rule.comparison_operator);
}

void MonitoringEngine::triggerAlert(const AlertRule& rule, const DealBase& deal, double metric_value) {
    Alert alert;
    alert.alert_id = generateAlertId();
    alert.rule_id = rule.rule_id;
    alert.deal_id = deal.getDealInfo().dealName;
    alert.title = rule.name;
    alert.message = formatAlertMessage(alert, rule);
    alert.category = rule.category;
    alert.severity = rule.severity;
    alert.status = AlertStatus::ACTIVE;
    alert.trigger_type = rule.trigger_type;
    alert.trigger_date = Date::todaysDate();
    alert.last_updated = Date::todaysDate();
    alert.triggered_metric = rule.metric_name;
    alert.metric_value = metric_value;
    alert.threshold_value = rule.threshold_value;
    
    // Add to processing queue
    {
        std::lock_guard<std::mutex> lock(queue_mutex_);
        alert_queue_.push(alert);
    }
    queue_condition_.notify_one();
}

void MonitoringEngine::processEscalations() {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    
    for (auto& alert_pair : active_alerts_) {
        Alert& alert = alert_pair.second;
        
        // Find applicable escalation rules
        for (const EscalationRule& escalation_rule : escalation_rules_) {
            if (alert.severity >= escalation_rule.min_severity &&
                escalation_rule.auto_escalate &&
                alert.escalation_level < escalation_rule.max_escalation_level) {
                
                // Check if escalation time has passed
                auto time_since_trigger = std::chrono::steady_clock::now(); // Simplified
                // In real implementation, would calculate based on alert.trigger_date
                
                // Escalate if needed (simplified logic)
                if (alert.escalation_level == 0) {
                    alert.escalation_level++;
                    alert.last_escalation_date = Date::todaysDate();
                    alert.escalation_history.push_back(Date::todaysDate());
                    alert.status = AlertStatus::ESCALATED;
                }
            }
        }
    }
}

void MonitoringEngine::sendNotification(const Alert& alert, NotificationChannel channel) {
    auto config_it = notification_configs_.find(channel);
    if (config_it != notification_configs_.end() && config_it->second.is_enabled) {
        std::string message = formatAlertMessage(alert, getAlertRule(alert.rule_id));
        std::cout << "Sending " << notificationChannelToString(channel) 
                  << " notification: " << message << std::endl;
    }
}

void MonitoringEngine::updateAlertStatus(const std::string& alert_id, AlertStatus new_status) {
    std::lock_guard<std::mutex> lock(alert_mutex_);
    auto it = active_alerts_.find(alert_id);
    if (it != active_alerts_.end()) {
        it->second.status = new_status;
        it->second.last_updated = Date::todaysDate();
    }
}

double MonitoringEngine::getMetricValue(const DealBase& deal, const std::string& metric_name) {
    // Check custom calculators first
    auto custom_it = custom_calculators_.find(metric_name);
    if (custom_it != custom_calculators_.end()) {
        return custom_it->second(deal);
    }
    
    // Use analytics engine if available
    if (analytics_engine_) {
        auto metrics = analytics_engine_->calculatePerformanceMetrics(deal, Date::todaysDate());
        
        if (metric_name == "net_present_value") {
            auto it = metrics.find("net_present_value");
            if (it != metrics.end()) return it->second;
        }
        if (metric_name == "internal_return_rate") {
            auto it = metrics.find("internal_return_rate");
            if (it != metrics.end()) return it->second;
        }
        if (metric_name == "weighted_average_life") {
            auto it = metrics.find("weighted_average_life");
            if (it != metrics.end()) return it->second;
        }
        if (metric_name == "duration") {
            auto it = metrics.find("duration");
            if (it != metrics.end()) return it->second;
        }
        if (metric_name == "convexity") {
            auto it = metrics.find("convexity");
            if (it != metrics.end()) return it->second;
        }
        
        auto risk_metrics = analytics_engine_->calculateRiskMetrics(deal, Date::todaysDate());
        if (metric_name == "value_at_risk") {
            auto it = risk_metrics.find("value_at_risk");
            if (it != risk_metrics.end()) return it->second;
        }
        if (metric_name == "expected_shortfall") {
            auto it = risk_metrics.find("expected_shortfall");
            if (it != risk_metrics.end()) return it->second;
        }
    }
    
    // Default metrics from deal
    if (metric_name == "total_balance") return deal.getTotalAccountBalance();
    if (metric_name == "deal_count") return 1.0; // Simple metric
    
    return 0.0; // Default fallback
}

bool MonitoringEngine::compareValues(double actual, double threshold, const std::string& operator_type) {
    if (operator_type == ">=") return actual >= threshold;
    if (operator_type == "<=") return actual <= threshold;
    if (operator_type == ">") return actual > threshold;
    if (operator_type == "<") return actual < threshold;
    if (operator_type == "==") return std::abs(actual - threshold) < 1e-9;
    if (operator_type == "!=") return std::abs(actual - threshold) >= 1e-9;
    
    return false;
}

std::string MonitoringEngine::generateAlertId() {
    static std::random_device rd;
    static std::mt19937 gen(rd());
    static std::uniform_int_distribution<> dis(1000, 9999);
    
    auto now = std::chrono::system_clock::now();
    auto time_t = std::chrono::system_clock::to_time_t(now);
    
    std::ostringstream oss;
    oss << "ALERT_" << time_t << "_" << dis(gen);
    return oss.str();
}

std::string MonitoringEngine::formatAlertMessage(const Alert& alert, const AlertRule& rule) {
    std::ostringstream message;
    message << "ALERT: " << alert.title << "\n";
    message << "Deal: " << alert.deal_id << "\n";
    message << "Severity: " << alertSeverityToString(alert.severity) << "\n";
    message << "Category: " << alertCategoryToString(alert.category) << "\n";
    message << "Metric: " << alert.triggered_metric << "\n";
    message << "Value: " << alert.metric_value << "\n";
    message << "Threshold: " << alert.threshold_value << "\n";
    message << "Operator: " << rule.comparison_operator << "\n";
    message << "Triggered: " << alert.trigger_date;
    return message.str();
}

bool MonitoringEngine::isAlertSuppressed(const std::string& rule_id) {
    for (const auto& suppression_pair : suppression_rules_) {
        const SuppressionRule& suppression = suppression_pair.second;
        if (suppression.alert_rule_id == rule_id && suppression.is_active) {
            // Check if suppression is still valid
            Date current_date = Date::todaysDate();
            if (current_date >= suppression.start_date && 
                (suppression.end_date == Date() || current_date <= suppression.end_date)) {
                return true;
            }
        }
    }
    return false;
}

void MonitoringEngine::applySuppression(const std::string& alert_id) {
    // This would apply suppression logic to a specific alert
    updateAlertStatus(alert_id, AlertStatus::SUPPRESSED);
}

void MonitoringEngine::cleanupExpiredSuppressions() {
    Date current_date = Date::todaysDate();
    auto it = suppression_rules_.begin();
    while (it != suppression_rules_.end()) {
        if (it->second.end_date != Date() && current_date > it->second.end_date) {
            it = suppression_rules_.erase(it);
        } else {
            ++it;
        }
    }
}

// String conversion utilities
std::string MonitoringEngine::alertSeverityToString(AlertSeverity severity) {
    switch (severity) {
        case AlertSeverity::INFO: return "INFO";
        case AlertSeverity::WARNING: return "WARNING";
        case AlertSeverity::CRITICAL: return "CRITICAL";
        case AlertSeverity::EMERGENCY: return "EMERGENCY";
        default: return "UNKNOWN";
    }
}

std::string MonitoringEngine::alertCategoryToString(AlertCategory category) {
    switch (category) {
        case AlertCategory::COVENANT_BREACH: return "COVENANT_BREACH";
        case AlertCategory::PERFORMANCE_DECLINE: return "PERFORMANCE_DECLINE";
        case AlertCategory::OPERATIONAL_ISSUE: return "OPERATIONAL_ISSUE";
        case AlertCategory::MARKET_RISK: return "MARKET_RISK";
        case AlertCategory::CREDIT_RISK: return "CREDIT_RISK";
        case AlertCategory::LIQUIDITY_RISK: return "LIQUIDITY_RISK";
        case AlertCategory::COMPLIANCE_VIOLATION: return "COMPLIANCE_VIOLATION";
        case AlertCategory::SYSTEM_ERROR: return "SYSTEM_ERROR";
        case AlertCategory::DATA_QUALITY: return "DATA_QUALITY";
        case AlertCategory::CUSTOM_ALERT: return "CUSTOM_ALERT";
        default: return "UNKNOWN";
    }
}

std::string MonitoringEngine::alertStatusToString(AlertStatus status) {
    switch (status) {
        case AlertStatus::ACTIVE: return "ACTIVE";
        case AlertStatus::ACKNOWLEDGED: return "ACKNOWLEDGED";
        case AlertStatus::INVESTIGATING: return "INVESTIGATING";
        case AlertStatus::RESOLVED: return "RESOLVED";
        case AlertStatus::SUPPRESSED: return "SUPPRESSED";
        case AlertStatus::ESCALATED: return "ESCALATED";
        case AlertStatus::CLOSED: return "CLOSED";
        default: return "UNKNOWN";
    }
}

std::string MonitoringEngine::alertTriggerTypeToString(AlertTriggerType type) {
    switch (type) {
        case AlertTriggerType::THRESHOLD_BREACH: return "THRESHOLD_BREACH";
        case AlertTriggerType::TREND_DETERIORATION: return "TREND_DETERIORATION";
        case AlertTriggerType::ANOMALY_DETECTION: return "ANOMALY_DETECTION";
        case AlertTriggerType::BUSINESS_RULE: return "BUSINESS_RULE";
        case AlertTriggerType::SCHEDULED_CHECK: return "SCHEDULED_CHECK";
        case AlertTriggerType::MANUAL_TRIGGER: return "MANUAL_TRIGGER";
        case AlertTriggerType::CASCADE_TRIGGER: return "CASCADE_TRIGGER";
        default: return "UNKNOWN";
    }
}

std::string MonitoringEngine::notificationChannelToString(NotificationChannel channel) {
    switch (channel) {
        case NotificationChannel::EMAIL: return "EMAIL";
        case NotificationChannel::SMS: return "SMS";
        case NotificationChannel::SLACK: return "SLACK";
        case NotificationChannel::WEBHOOK: return "WEBHOOK";
        case NotificationChannel::DASHBOARD: return "DASHBOARD";
        case NotificationChannel::MOBILE_PUSH: return "MOBILE_PUSH";
        case NotificationChannel::PHONE_CALL: return "PHONE_CALL";
        case NotificationChannel::CUSTOM: return "CUSTOM";
        default: return "UNKNOWN";
    }
}

AlertSeverity MonitoringEngine::stringToAlertSeverity(const std::string& str) {
    if (str == "INFO") return AlertSeverity::INFO;
    if (str == "WARNING") return AlertSeverity::WARNING;
    if (str == "CRITICAL") return AlertSeverity::CRITICAL;
    if (str == "EMERGENCY") return AlertSeverity::EMERGENCY;
    return AlertSeverity::INFO;
}

AlertCategory MonitoringEngine::stringToAlertCategory(const std::string& str) {
    if (str == "COVENANT_BREACH") return AlertCategory::COVENANT_BREACH;
    if (str == "PERFORMANCE_DECLINE") return AlertCategory::PERFORMANCE_DECLINE;
    if (str == "OPERATIONAL_ISSUE") return AlertCategory::OPERATIONAL_ISSUE;
    if (str == "MARKET_RISK") return AlertCategory::MARKET_RISK;
    if (str == "CREDIT_RISK") return AlertCategory::CREDIT_RISK;
    if (str == "LIQUIDITY_RISK") return AlertCategory::LIQUIDITY_RISK;
    if (str == "COMPLIANCE_VIOLATION") return AlertCategory::COMPLIANCE_VIOLATION;
    if (str == "SYSTEM_ERROR") return AlertCategory::SYSTEM_ERROR;
    if (str == "DATA_QUALITY") return AlertCategory::DATA_QUALITY;
    if (str == "CUSTOM_ALERT") return AlertCategory::CUSTOM_ALERT;
    return AlertCategory::CUSTOM_ALERT;
}

AlertStatus MonitoringEngine::stringToAlertStatus(const std::string& str) {
    if (str == "ACTIVE") return AlertStatus::ACTIVE;
    if (str == "ACKNOWLEDGED") return AlertStatus::ACKNOWLEDGED;
    if (str == "INVESTIGATING") return AlertStatus::INVESTIGATING;
    if (str == "RESOLVED") return AlertStatus::RESOLVED;
    if (str == "SUPPRESSED") return AlertStatus::SUPPRESSED;
    if (str == "ESCALATED") return AlertStatus::ESCALATED;
    if (str == "CLOSED") return AlertStatus::CLOSED;
    return AlertStatus::ACTIVE;
}

AlertTriggerType MonitoringEngine::stringToAlertTriggerType(const std::string& str) {
    if (str == "THRESHOLD_BREACH") return AlertTriggerType::THRESHOLD_BREACH;
    if (str == "TREND_DETERIORATION") return AlertTriggerType::TREND_DETERIORATION;
    if (str == "ANOMALY_DETECTION") return AlertTriggerType::ANOMALY_DETECTION;
    if (str == "BUSINESS_RULE") return AlertTriggerType::BUSINESS_RULE;
    if (str == "SCHEDULED_CHECK") return AlertTriggerType::SCHEDULED_CHECK;
    if (str == "MANUAL_TRIGGER") return AlertTriggerType::MANUAL_TRIGGER;
    if (str == "CASCADE_TRIGGER") return AlertTriggerType::CASCADE_TRIGGER;
    return AlertTriggerType::THRESHOLD_BREACH;
}

NotificationChannel MonitoringEngine::stringToNotificationChannel(const std::string& str) {
    if (str == "EMAIL") return NotificationChannel::EMAIL;
    if (str == "SMS") return NotificationChannel::SMS;
    if (str == "SLACK") return NotificationChannel::SLACK;
    if (str == "WEBHOOK") return NotificationChannel::WEBHOOK;
    if (str == "DASHBOARD") return NotificationChannel::DASHBOARD;
    if (str == "MOBILE_PUSH") return NotificationChannel::MOBILE_PUSH;
    if (str == "PHONE_CALL") return NotificationChannel::PHONE_CALL;
    if (str == "CUSTOM") return NotificationChannel::CUSTOM;
    return NotificationChannel::EMAIL;
}

} // namespace Structura