#pragma once

#include <string>
#include <vector>
#include <map>
#include <memory>
#include <functional>
#include <chrono>
#include <queue>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <atomic>

#include "../core/types.h"
#include "../core/deal_base.h"
#include "../analytics/analytics_engine.h"
#include "../validation/validation_engine.h"

namespace Structura {

// Forward declarations
class DealBase;
class AnalyticsEngine;
class ValidationEngine;

/**
 * @brief Alert severity levels
 */
enum class AlertSeverity {
    INFO,           // Informational alerts
    WARNING,        // Warning conditions  
    CRITICAL,       // Critical issues requiring attention
    EMERGENCY       // Emergency conditions requiring immediate action
};

/**
 * @brief Alert categories for classification
 */
enum class AlertCategory {
    COVENANT_BREACH,        // Covenant violations
    PERFORMANCE_DECLINE,    // Performance degradation
    OPERATIONAL_ISSUE,      // Operational problems
    MARKET_RISK,           // Market risk events
    CREDIT_RISK,           // Credit risk alerts
    LIQUIDITY_RISK,        // Liquidity concerns
    COMPLIANCE_VIOLATION,   // Regulatory compliance issues
    SYSTEM_ERROR,          // System/technical errors
    DATA_QUALITY,          // Data quality issues
    CUSTOM_ALERT           // User-defined alerts
};

/**
 * @brief Alert trigger types
 */
enum class AlertTriggerType {
    THRESHOLD_BREACH,      // Metric crosses threshold
    TREND_DETERIORATION,   // Negative trend detected
    ANOMALY_DETECTION,     // Statistical anomaly
    BUSINESS_RULE,         // Business rule violation
    SCHEDULED_CHECK,       // Scheduled monitoring
    MANUAL_TRIGGER,        // Manually triggered
    CASCADE_TRIGGER        // Triggered by another alert
};

/**
 * @brief Alert status tracking
 */
enum class AlertStatus {
    ACTIVE,         // Alert is active
    ACKNOWLEDGED,   // Alert has been acknowledged
    INVESTIGATING,  // Under investigation
    RESOLVED,       // Issue resolved
    SUPPRESSED,     // Temporarily suppressed
    ESCALATED,      // Escalated to higher level
    CLOSED          // Alert closed
};

/**
 * @brief Notification channels
 */
enum class NotificationChannel {
    EMAIL,          // Email notification
    SMS,            // SMS text message
    SLACK,          // Slack integration
    WEBHOOK,        // HTTP webhook
    DASHBOARD,      // Dashboard display
    MOBILE_PUSH,    // Mobile push notification
    PHONE_CALL,     // Automated phone call
    CUSTOM          // Custom notification channel
};

/**
 * @brief Alert configuration and metadata
 */
struct AlertRule {
    std::string rule_id;
    std::string name;
    std::string description;
    AlertCategory category;
    AlertSeverity severity;
    AlertTriggerType trigger_type;
    
    // Threshold configuration
    std::string metric_name;
    double threshold_value;
    std::string comparison_operator; // ">=", "<=", ">", "<", "==", "!="
    
    // Time-based configuration
    std::chrono::seconds evaluation_frequency;
    std::chrono::seconds minimum_duration; // Minimum time before triggering
    std::chrono::seconds suppression_window; // Time to suppress duplicate alerts
    
    // Escalation configuration
    std::vector<std::chrono::seconds> escalation_delays;
    std::vector<std::string> escalation_contacts;
    
    // Notification configuration
    std::vector<NotificationChannel> notification_channels;
    std::vector<std::string> recipients;
    std::string message_template;
    
    // Operational settings
    bool is_active;
    Date effective_date;
    Date expiration_date;
    std::map<std::string, std::string> custom_parameters;
    
    AlertRule() = default;
    AlertRule(const std::string& id, const std::string& rule_name, 
              AlertCategory cat, AlertSeverity sev)
        : rule_id(id), name(rule_name), category(cat), severity(sev),
          trigger_type(AlertTriggerType::THRESHOLD_BREACH), threshold_value(0.0),
          comparison_operator(">="), evaluation_frequency(std::chrono::minutes(5)),
          minimum_duration(std::chrono::seconds(0)), 
          suppression_window(std::chrono::minutes(15)),
          is_active(true) {}
};

/**
 * @brief Individual alert instance
 */
struct Alert {
    std::string alert_id;
    std::string rule_id;
    std::string deal_id;
    std::string title;
    std::string message;
    std::string detailed_description;
    
    AlertCategory category;
    AlertSeverity severity;
    AlertStatus status;
    AlertTriggerType trigger_type;
    
    Date trigger_date;
    Date last_updated;
    Date acknowledged_date;
    Date resolved_date;
    
    // Alert context
    std::string triggered_metric;
    double metric_value;
    double threshold_value;
    std::map<std::string, double> context_metrics;
    std::vector<std::string> affected_components;
    
    // Response tracking
    std::string acknowledged_by;
    std::string assigned_to;
    std::vector<std::string> response_actions;
    std::string resolution_notes;
    
    // Escalation tracking
    int escalation_level;
    Date last_escalation_date;
    std::vector<Date> escalation_history;
    
    Alert() = default;
    Alert(const std::string& id, const std::string& rule, const std::string& deal,
          const std::string& alert_title, AlertCategory cat, AlertSeverity sev)
        : alert_id(id), rule_id(rule), deal_id(deal), title(alert_title),
          category(cat), severity(sev), status(AlertStatus::ACTIVE),
          trigger_date(Date::todaysDate()), last_updated(Date::todaysDate()),
          metric_value(0.0), threshold_value(0.0), escalation_level(0) {}
};

/**
 * @brief Alert notification configuration
 */
struct NotificationConfig {
    NotificationChannel channel;
    std::map<std::string, std::string> channel_config; // Email server, API keys, etc.
    std::vector<std::string> recipients;
    std::string message_template;
    bool is_enabled;
    
    NotificationConfig() : channel(NotificationChannel::EMAIL), is_enabled(true) {}
};

/**
 * @brief Escalation rule configuration
 */
struct EscalationRule {
    std::string rule_id;
    AlertSeverity min_severity;
    std::chrono::seconds initial_delay;
    std::vector<std::chrono::seconds> escalation_intervals;
    std::vector<std::string> escalation_contacts;
    std::vector<NotificationChannel> escalation_channels;
    int max_escalation_level;
    bool auto_escalate;
    
    EscalationRule() : min_severity(AlertSeverity::CRITICAL), 
                      initial_delay(std::chrono::minutes(15)),
                      max_escalation_level(3), auto_escalate(true) {}
};

/**
 * @brief Monitoring dashboard data
 */
struct MonitoringDashboard {
    Date last_updated;
    std::map<AlertSeverity, int> alert_counts_by_severity;
    std::map<AlertCategory, int> alert_counts_by_category;
    std::map<AlertStatus, int> alert_counts_by_status;
    std::vector<Alert> recent_alerts;
    std::vector<Alert> critical_alerts;
    std::map<std::string, double> key_metrics;
    std::vector<std::string> system_health_indicators;
    
    MonitoringDashboard() : last_updated(Date::todaysDate()) {}
};

/**
 * @brief Alert suppression configuration
 */
struct SuppressionRule {
    std::string rule_id;
    std::string alert_rule_id;
    std::chrono::seconds suppression_duration;
    Date start_date;
    Date end_date;
    std::string reason;
    std::string created_by;
    bool is_active;
    
    SuppressionRule() : suppression_duration(std::chrono::hours(1)), is_active(true) {}
};

/**
 * @brief Main monitoring and alerts system
 */
class MonitoringEngine {
private:
    // Dependencies
    std::shared_ptr<AnalyticsEngine> analytics_engine_;
    std::shared_ptr<ValidationEngine> validation_engine_;
    
    // Alert management
    std::map<std::string, AlertRule> alert_rules_;
    std::map<std::string, Alert> active_alerts_;
    std::vector<Alert> alert_history_;
    std::map<std::string, SuppressionRule> suppression_rules_;
    
    // Notification system
    std::map<NotificationChannel, NotificationConfig> notification_configs_;
    std::vector<EscalationRule> escalation_rules_;
    
    // Monitoring thread management
    std::atomic<bool> monitoring_active_;
    std::thread monitoring_thread_;
    mutable std::mutex alert_mutex_;
    std::condition_variable alert_condition_;
    
    // Alert processing queue
    std::queue<Alert> alert_queue_;
    std::thread alert_processor_thread_;
    std::mutex queue_mutex_;
    std::condition_variable queue_condition_;
    
    // Monitoring frequency
    std::chrono::seconds default_monitoring_interval_;
    
    // Internal processing methods
    void monitoringLoop();
    void processAlertQueue();
    void evaluateAlertRules(const DealBase& deal);
    bool evaluateAlertRule(const AlertRule& rule, const DealBase& deal);
    void triggerAlert(const AlertRule& rule, const DealBase& deal, double metric_value);
    void processEscalations();
    void sendNotification(const Alert& alert, NotificationChannel channel);
    void updateAlertStatus(const std::string& alert_id, AlertStatus new_status);
    
    // Metric evaluation helpers
    double getMetricValue(const DealBase& deal, const std::string& metric_name);
    bool compareValues(double actual, double threshold, const std::string& operator_type);
    std::string generateAlertId();
    std::string formatAlertMessage(const Alert& alert, const AlertRule& rule);
    
    // Suppression management
    bool isAlertSuppressed(const std::string& rule_id);
    void applySuppression(const std::string& alert_id);
    void cleanupExpiredSuppressions();

public:
    MonitoringEngine();
    explicit MonitoringEngine(std::shared_ptr<AnalyticsEngine> analytics,
                             std::shared_ptr<ValidationEngine> validation);
    ~MonitoringEngine();
    
    // System lifecycle
    void startMonitoring();
    void stopMonitoring();
    bool isMonitoringActive() const;
    
    // Engine configuration
    void setAnalyticsEngine(std::shared_ptr<AnalyticsEngine> analytics);
    void setValidationEngine(std::shared_ptr<ValidationEngine> validation);
    void setMonitoringInterval(std::chrono::seconds interval);
    
    // Alert rule management
    void addAlertRule(const AlertRule& rule);
    void updateAlertRule(const std::string& rule_id, const AlertRule& rule);
    void removeAlertRule(const std::string& rule_id);
    void enableAlertRule(const std::string& rule_id);
    void disableAlertRule(const std::string& rule_id);
    AlertRule getAlertRule(const std::string& rule_id) const;
    std::vector<AlertRule> getAlertRules() const;
    std::vector<AlertRule> getAlertRulesByCategory(AlertCategory category) const;
    
    // Deal monitoring
    void addDealToMonitoring(std::shared_ptr<DealBase> deal);
    void removeDealFromMonitoring(const std::string& deal_id);
    void evaluateDeal(const DealBase& deal);
    std::vector<std::string> getMonitoredDeals() const;
    
    // Alert management
    std::vector<Alert> getActiveAlerts() const;
    std::vector<Alert> getAlertsByDeal(const std::string& deal_id) const;
    std::vector<Alert> getAlertsByCategory(AlertCategory category) const;
    std::vector<Alert> getAlertsBySeverity(AlertSeverity severity) const;
    Alert getAlert(const std::string& alert_id) const;
    
    // Alert operations
    void acknowledgeAlert(const std::string& alert_id, const std::string& acknowledged_by);
    void assignAlert(const std::string& alert_id, const std::string& assigned_to);
    void addResponseAction(const std::string& alert_id, const std::string& action);
    void resolveAlert(const std::string& alert_id, const std::string& resolution_notes);
    void escalateAlert(const std::string& alert_id);
    void suppressAlert(const std::string& alert_id, std::chrono::seconds duration, const std::string& reason);
    
    // Notification system
    void configureNotificationChannel(NotificationChannel channel, const NotificationConfig& config);
    void addNotificationRecipient(const std::string& alert_rule_id, const std::string& recipient);
    void removeNotificationRecipient(const std::string& alert_rule_id, const std::string& recipient);
    void testNotificationChannel(NotificationChannel channel, const std::string& test_message);
    
    // Escalation management
    void addEscalationRule(const EscalationRule& rule);
    void updateEscalationRule(const std::string& rule_id, const EscalationRule& rule);
    void removeEscalationRule(const std::string& rule_id);
    std::vector<EscalationRule> getEscalationRules() const;
    
    // Suppression management
    void addSuppressionRule(const SuppressionRule& rule);
    void removeSuppressionRule(const std::string& rule_id);
    std::vector<SuppressionRule> getActiveSuppressions() const;
    
    // Dashboard and reporting
    MonitoringDashboard generateDashboard() const;
    std::map<std::string, int> getAlertStatistics() const;
    std::vector<Alert> getRecentAlerts(int limit = 50) const;
    std::vector<Alert> getAlertHistory(Date start_date, Date end_date) const;
    
    // Health monitoring
    std::vector<std::string> getSystemHealthStatus() const;
    std::map<std::string, double> getMonitoringMetrics() const;
    void performHealthCheck();
    
    // Advanced features
    void enableAnomalyDetection(bool enable = true);
    void configureTrendAnalysis(const std::string& metric_name, int lookback_periods);
    void addCustomMetricCalculator(const std::string& metric_name, 
                                  std::function<double(const DealBase&)> calculator);
    
    // Bulk operations
    void acknowledgeMultipleAlerts(const std::vector<std::string>& alert_ids, const std::string& acknowledged_by);
    void resolveMultipleAlerts(const std::vector<std::string>& alert_ids, const std::string& resolution_notes);
    void bulkUpdateAlertStatus(const std::vector<std::string>& alert_ids, AlertStatus new_status);
    
    // Export and import
    std::string exportAlertsToJSON(Date start_date, Date end_date) const;
    std::string exportAlertRulesToJSON() const;
    void importAlertRulesFromJSON(const std::string& json_data);
    
    // String conversion utilities
    static std::string alertSeverityToString(AlertSeverity severity);
    static std::string alertCategoryToString(AlertCategory category);
    static std::string alertStatusToString(AlertStatus status);
    static std::string alertTriggerTypeToString(AlertTriggerType type);
    static std::string notificationChannelToString(NotificationChannel channel);
    
    static AlertSeverity stringToAlertSeverity(const std::string& str);
    static AlertCategory stringToAlertCategory(const std::string& str);
    static AlertStatus stringToAlertStatus(const std::string& str);
    static AlertTriggerType stringToAlertTriggerType(const std::string& str);
    static NotificationChannel stringToNotificationChannel(const std::string& str);

private:
    // Monitored deals
    std::map<std::string, std::shared_ptr<DealBase>> monitored_deals_;
    
    // Custom metric calculators
    std::map<std::string, std::function<double(const DealBase&)>> custom_calculators_;
    
    // Anomaly detection configuration
    bool anomaly_detection_enabled_;
    std::map<std::string, std::vector<double>> metric_baselines_;
    
    // Trend analysis configuration
    std::map<std::string, int> trend_analysis_config_;
    std::map<std::string, std::vector<std::pair<Date, double>>> trend_data_;
    
    // System health monitoring
    mutable std::mutex health_mutex_;
    std::chrono::steady_clock::time_point last_health_check_;
    std::map<std::string, bool> health_indicators_;
};

} // namespace Structura