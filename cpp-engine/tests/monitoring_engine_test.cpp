#include <gtest/gtest.h>
#include <memory>
#include <chrono>
#include <thread>
#include "../src/monitoring/monitoring_engine.h"
#include "../src/core/deal_base.h"
#include "../src/analytics/analytics_engine.h"
#include "../src/validation/validation_engine.h"

using namespace Structura;

// Mock DealBase for testing
class MockDealBase : public DealBase {
private:
    std::string deal_id_;
    double total_balance_;
    
public:
    MockDealBase(const std::string& name, double value) 
        : DealBase(DealInfo{name, "TEST", Date::todaysDate(), Date::todaysDate()}),
          deal_id_(name), total_balance_(value) {
        // Add a mock account with the initial balance
        auto mock_account = std::make_unique<Account>("MOCK_ACCOUNT", Balance(value));
        addAccount("MOCK_ACCOUNT", std::move(mock_account));
    }
    
    void setTotalBalance(double balance) { 
        total_balance_ = balance;
        // Update the mock account balance
        Account* account = getAccount("MOCK_ACCOUNT");
        if (account) {
            double current_balance = account->getBalance();
            double difference = balance - current_balance;
            
            if (difference > 0) {
                // Deposit the difference
                account->deposit(Date::todaysDate(), difference, "Mock balance update");
            } else if (difference < 0) {
                // Withdraw the difference
                account->withdraw(Date::todaysDate(), -difference, "Mock balance update");
            }
        }
    }
    
    // Mock implementations for pure virtual methods
    void runWaterfall(const Date& /*payment_date*/) override {}
    Balance calculateDealValue(const Date& /*valuationDate*/) const override { return total_balance_; }
    std::vector<std::string> validate() const override { return {}; }
    std::string getDealSummary() const override { return "Mock Deal Summary"; }
};

class MonitoringEngineTest : public ::testing::Test {
protected:
    void SetUp() override {
        analytics_engine_ = std::make_shared<AnalyticsEngine>();
        validation_engine_ = std::make_shared<ValidationEngine>();
        monitoring_engine_ = std::make_unique<MonitoringEngine>(analytics_engine_, validation_engine_);
        
        // Start the monitoring system
        monitoring_engine_->startMonitoring();
        
        mock_deal_ = std::make_shared<MockDealBase>("TEST_DEAL_001", 1000000.0);
    }
    
    void TearDown() override {
        if (monitoring_engine_->isMonitoringActive()) {
            monitoring_engine_->stopMonitoring();
        }
    }
    
    std::shared_ptr<AnalyticsEngine> analytics_engine_;
    std::shared_ptr<ValidationEngine> validation_engine_;
    std::unique_ptr<MonitoringEngine> monitoring_engine_;
    std::shared_ptr<MockDealBase> mock_deal_;
};

// Test: Basic Monitoring Engine Construction
TEST_F(MonitoringEngineTest, BasicConstruction) {
    EXPECT_FALSE(monitoring_engine_->isMonitoringActive());
    EXPECT_EQ(monitoring_engine_->getActiveAlerts().size(), 0);
    EXPECT_EQ(monitoring_engine_->getAlertRules().size(), 0);
}

// Test: Engine Dependencies Configuration
TEST_F(MonitoringEngineTest, EngineDependencies) {
    auto new_analytics = std::make_shared<AnalyticsEngine>();
    auto new_validation = std::make_shared<ValidationEngine>();
    
    monitoring_engine_->setAnalyticsEngine(new_analytics);
    monitoring_engine_->setValidationEngine(new_validation);
    
    // Dependencies are set (can't directly test private members, but no exceptions should occur)
    EXPECT_NO_THROW(monitoring_engine_->performHealthCheck());
}

// Test: Alert Severity Enum Conversions
TEST_F(MonitoringEngineTest, AlertSeverityConversions) {
    EXPECT_EQ(MonitoringEngine::alertSeverityToString(AlertSeverity::INFO), "INFO");
    EXPECT_EQ(MonitoringEngine::alertSeverityToString(AlertSeverity::WARNING), "WARNING");
    EXPECT_EQ(MonitoringEngine::alertSeverityToString(AlertSeverity::CRITICAL), "CRITICAL");
    EXPECT_EQ(MonitoringEngine::alertSeverityToString(AlertSeverity::EMERGENCY), "EMERGENCY");
    
    EXPECT_EQ(MonitoringEngine::stringToAlertSeverity("INFO"), AlertSeverity::INFO);
    EXPECT_EQ(MonitoringEngine::stringToAlertSeverity("WARNING"), AlertSeverity::WARNING);
    EXPECT_EQ(MonitoringEngine::stringToAlertSeverity("CRITICAL"), AlertSeverity::CRITICAL);
    EXPECT_EQ(MonitoringEngine::stringToAlertSeverity("EMERGENCY"), AlertSeverity::EMERGENCY);
    EXPECT_EQ(MonitoringEngine::stringToAlertSeverity("INVALID"), AlertSeverity::INFO); // Default fallback
}

// Test: Alert Category Enum Conversions
TEST_F(MonitoringEngineTest, AlertCategoryConversions) {
    EXPECT_EQ(MonitoringEngine::alertCategoryToString(AlertCategory::COVENANT_BREACH), "COVENANT_BREACH");
    EXPECT_EQ(MonitoringEngine::alertCategoryToString(AlertCategory::PERFORMANCE_DECLINE), "PERFORMANCE_DECLINE");
    EXPECT_EQ(MonitoringEngine::alertCategoryToString(AlertCategory::OPERATIONAL_ISSUE), "OPERATIONAL_ISSUE");
    EXPECT_EQ(MonitoringEngine::alertCategoryToString(AlertCategory::MARKET_RISK), "MARKET_RISK");
    EXPECT_EQ(MonitoringEngine::alertCategoryToString(AlertCategory::CREDIT_RISK), "CREDIT_RISK");
    EXPECT_EQ(MonitoringEngine::alertCategoryToString(AlertCategory::LIQUIDITY_RISK), "LIQUIDITY_RISK");
    EXPECT_EQ(MonitoringEngine::alertCategoryToString(AlertCategory::COMPLIANCE_VIOLATION), "COMPLIANCE_VIOLATION");
    EXPECT_EQ(MonitoringEngine::alertCategoryToString(AlertCategory::SYSTEM_ERROR), "SYSTEM_ERROR");
    EXPECT_EQ(MonitoringEngine::alertCategoryToString(AlertCategory::DATA_QUALITY), "DATA_QUALITY");
    EXPECT_EQ(MonitoringEngine::alertCategoryToString(AlertCategory::CUSTOM_ALERT), "CUSTOM_ALERT");
    
    EXPECT_EQ(MonitoringEngine::stringToAlertCategory("COVENANT_BREACH"), AlertCategory::COVENANT_BREACH);
    EXPECT_EQ(MonitoringEngine::stringToAlertCategory("PERFORMANCE_DECLINE"), AlertCategory::PERFORMANCE_DECLINE);
    EXPECT_EQ(MonitoringEngine::stringToAlertCategory("INVALID"), AlertCategory::CUSTOM_ALERT); // Default fallback
}

// Test: Alert Status Enum Conversions
TEST_F(MonitoringEngineTest, AlertStatusConversions) {
    EXPECT_EQ(MonitoringEngine::alertStatusToString(AlertStatus::ACTIVE), "ACTIVE");
    EXPECT_EQ(MonitoringEngine::alertStatusToString(AlertStatus::ACKNOWLEDGED), "ACKNOWLEDGED");
    EXPECT_EQ(MonitoringEngine::alertStatusToString(AlertStatus::INVESTIGATING), "INVESTIGATING");
    EXPECT_EQ(MonitoringEngine::alertStatusToString(AlertStatus::RESOLVED), "RESOLVED");
    EXPECT_EQ(MonitoringEngine::alertStatusToString(AlertStatus::SUPPRESSED), "SUPPRESSED");
    EXPECT_EQ(MonitoringEngine::alertStatusToString(AlertStatus::ESCALATED), "ESCALATED");
    EXPECT_EQ(MonitoringEngine::alertStatusToString(AlertStatus::CLOSED), "CLOSED");
    
    EXPECT_EQ(MonitoringEngine::stringToAlertStatus("ACTIVE"), AlertStatus::ACTIVE);
    EXPECT_EQ(MonitoringEngine::stringToAlertStatus("ACKNOWLEDGED"), AlertStatus::ACKNOWLEDGED);
    EXPECT_EQ(MonitoringEngine::stringToAlertStatus("INVALID"), AlertStatus::ACTIVE); // Default fallback
}

// Test: Alert Trigger Type Enum Conversions
TEST_F(MonitoringEngineTest, AlertTriggerTypeConversions) {
    EXPECT_EQ(MonitoringEngine::alertTriggerTypeToString(AlertTriggerType::THRESHOLD_BREACH), "THRESHOLD_BREACH");
    EXPECT_EQ(MonitoringEngine::alertTriggerTypeToString(AlertTriggerType::TREND_DETERIORATION), "TREND_DETERIORATION");
    EXPECT_EQ(MonitoringEngine::alertTriggerTypeToString(AlertTriggerType::ANOMALY_DETECTION), "ANOMALY_DETECTION");
    EXPECT_EQ(MonitoringEngine::alertTriggerTypeToString(AlertTriggerType::BUSINESS_RULE), "BUSINESS_RULE");
    EXPECT_EQ(MonitoringEngine::alertTriggerTypeToString(AlertTriggerType::SCHEDULED_CHECK), "SCHEDULED_CHECK");
    EXPECT_EQ(MonitoringEngine::alertTriggerTypeToString(AlertTriggerType::MANUAL_TRIGGER), "MANUAL_TRIGGER");
    EXPECT_EQ(MonitoringEngine::alertTriggerTypeToString(AlertTriggerType::CASCADE_TRIGGER), "CASCADE_TRIGGER");
    
    EXPECT_EQ(MonitoringEngine::stringToAlertTriggerType("THRESHOLD_BREACH"), AlertTriggerType::THRESHOLD_BREACH);
    EXPECT_EQ(MonitoringEngine::stringToAlertTriggerType("TREND_DETERIORATION"), AlertTriggerType::TREND_DETERIORATION);
    EXPECT_EQ(MonitoringEngine::stringToAlertTriggerType("INVALID"), AlertTriggerType::THRESHOLD_BREACH); // Default fallback
}

// Test: Notification Channel Enum Conversions
TEST_F(MonitoringEngineTest, NotificationChannelConversions) {
    EXPECT_EQ(MonitoringEngine::notificationChannelToString(NotificationChannel::EMAIL), "EMAIL");
    EXPECT_EQ(MonitoringEngine::notificationChannelToString(NotificationChannel::SMS), "SMS");
    EXPECT_EQ(MonitoringEngine::notificationChannelToString(NotificationChannel::SLACK), "SLACK");
    EXPECT_EQ(MonitoringEngine::notificationChannelToString(NotificationChannel::WEBHOOK), "WEBHOOK");
    EXPECT_EQ(MonitoringEngine::notificationChannelToString(NotificationChannel::DASHBOARD), "DASHBOARD");
    EXPECT_EQ(MonitoringEngine::notificationChannelToString(NotificationChannel::MOBILE_PUSH), "MOBILE_PUSH");
    EXPECT_EQ(MonitoringEngine::notificationChannelToString(NotificationChannel::PHONE_CALL), "PHONE_CALL");
    EXPECT_EQ(MonitoringEngine::notificationChannelToString(NotificationChannel::CUSTOM), "CUSTOM");
    
    EXPECT_EQ(MonitoringEngine::stringToNotificationChannel("EMAIL"), NotificationChannel::EMAIL);
    EXPECT_EQ(MonitoringEngine::stringToNotificationChannel("SMS"), NotificationChannel::SMS);
    EXPECT_EQ(MonitoringEngine::stringToNotificationChannel("INVALID"), NotificationChannel::EMAIL); // Default fallback
}

// Test: Alert Rule Management
TEST_F(MonitoringEngineTest, AlertRuleManagement) {
    AlertRule rule("RULE_001", "Test Balance Rule", AlertCategory::PERFORMANCE_DECLINE, AlertSeverity::WARNING);
    rule.metric_name = "total_balance";
    rule.threshold_value = 500000.0;
    rule.comparison_operator = "<";
    
    // Add rule
    monitoring_engine_->addAlertRule(rule);
    EXPECT_EQ(monitoring_engine_->getAlertRules().size(), 1);
    
    // Get rule
    AlertRule retrieved_rule = monitoring_engine_->getAlertRule("RULE_001");
    EXPECT_EQ(retrieved_rule.rule_id, "RULE_001");
    EXPECT_EQ(retrieved_rule.name, "Test Balance Rule");
    EXPECT_EQ(retrieved_rule.category, AlertCategory::PERFORMANCE_DECLINE);
    EXPECT_EQ(retrieved_rule.severity, AlertSeverity::WARNING);
    
    // Update rule
    rule.threshold_value = 750000.0;
    monitoring_engine_->updateAlertRule("RULE_001", rule);
    AlertRule updated_rule = monitoring_engine_->getAlertRule("RULE_001");
    EXPECT_EQ(updated_rule.threshold_value, 750000.0);
    
    // Disable/Enable rule
    monitoring_engine_->disableAlertRule("RULE_001");
    AlertRule disabled_rule = monitoring_engine_->getAlertRule("RULE_001");
    EXPECT_FALSE(disabled_rule.is_active);
    
    monitoring_engine_->enableAlertRule("RULE_001");
    AlertRule enabled_rule = monitoring_engine_->getAlertRule("RULE_001");
    EXPECT_TRUE(enabled_rule.is_active);
    
    // Remove rule
    monitoring_engine_->removeAlertRule("RULE_001");
    EXPECT_EQ(monitoring_engine_->getAlertRules().size(), 0);
}

// Test: Alert Rules by Category
TEST_F(MonitoringEngineTest, AlertRulesByCategory) {
    AlertRule rule1("RULE_001", "Covenant Rule", AlertCategory::COVENANT_BREACH, AlertSeverity::CRITICAL);
    AlertRule rule2("RULE_002", "Performance Rule", AlertCategory::PERFORMANCE_DECLINE, AlertSeverity::WARNING);
    AlertRule rule3("RULE_003", "Risk Rule", AlertCategory::MARKET_RISK, AlertSeverity::WARNING);
    AlertRule rule4("RULE_004", "Another Performance Rule", AlertCategory::PERFORMANCE_DECLINE, AlertSeverity::INFO);
    
    monitoring_engine_->addAlertRule(rule1);
    monitoring_engine_->addAlertRule(rule2);
    monitoring_engine_->addAlertRule(rule3);
    monitoring_engine_->addAlertRule(rule4);
    
    std::vector<AlertRule> covenant_rules = monitoring_engine_->getAlertRulesByCategory(AlertCategory::COVENANT_BREACH);
    EXPECT_EQ(covenant_rules.size(), 1);
    EXPECT_EQ(covenant_rules[0].rule_id, "RULE_001");
    
    std::vector<AlertRule> performance_rules = monitoring_engine_->getAlertRulesByCategory(AlertCategory::PERFORMANCE_DECLINE);
    EXPECT_EQ(performance_rules.size(), 2);
    
    std::vector<AlertRule> market_rules = monitoring_engine_->getAlertRulesByCategory(AlertCategory::MARKET_RISK);
    EXPECT_EQ(market_rules.size(), 1);
    EXPECT_EQ(market_rules[0].rule_id, "RULE_003");
}

// Test: Deal Monitoring Management
TEST_F(MonitoringEngineTest, DealMonitoringManagement) {
    auto deal1 = std::make_shared<MockDealBase>("DEAL_001", 1000000.0);
    auto deal2 = std::make_shared<MockDealBase>("DEAL_002", 2000000.0);
    
    // Add deals to monitoring
    monitoring_engine_->addDealToMonitoring(deal1);
    monitoring_engine_->addDealToMonitoring(deal2);
    
    std::vector<std::string> monitored_deals = monitoring_engine_->getMonitoredDeals();
    EXPECT_EQ(monitored_deals.size(), 2);
    EXPECT_TRUE(std::find(monitored_deals.begin(), monitored_deals.end(), "DEAL_001") != monitored_deals.end());
    EXPECT_TRUE(std::find(monitored_deals.begin(), monitored_deals.end(), "DEAL_002") != monitored_deals.end());
    
    // Remove deal from monitoring
    monitoring_engine_->removeDealFromMonitoring("DEAL_001");
    monitored_deals = monitoring_engine_->getMonitoredDeals();
    EXPECT_EQ(monitored_deals.size(), 1);
    EXPECT_TRUE(std::find(monitored_deals.begin(), monitored_deals.end(), "DEAL_002") != monitored_deals.end());
}

// Test: Manual Deal Evaluation (Alert Triggering)
TEST_F(MonitoringEngineTest, ManualDealEvaluation) {
    // Create alert rule that should trigger
    AlertRule rule("RULE_001", "Low Balance Alert", AlertCategory::PERFORMANCE_DECLINE, AlertSeverity::WARNING);
    rule.metric_name = "total_balance";
    rule.threshold_value = 1500000.0; // Higher than mock deal balance
    rule.comparison_operator = "<";
    rule.notification_channels = {NotificationChannel::EMAIL};
    
    monitoring_engine_->addAlertRule(rule);
    
    // Set up mock deal with balance that should trigger alert
    mock_deal_->setTotalBalance(1000000.0); // Less than threshold
    
    // Evaluate deal manually
    monitoring_engine_->evaluateDeal(*mock_deal_);
    
    // Allow some time for alert processing
    std::this_thread::sleep_for(std::chrono::milliseconds(100));
    
    // Check if alert was triggered
    std::vector<Alert> active_alerts = monitoring_engine_->getActiveAlerts();
    EXPECT_GT(active_alerts.size(), 0);
    
    if (!active_alerts.empty()) {
        Alert alert = active_alerts[0];
        EXPECT_EQ(alert.rule_id, "RULE_001");
        EXPECT_EQ(alert.deal_id, "TEST_DEAL_001");
        EXPECT_EQ(alert.category, AlertCategory::PERFORMANCE_DECLINE);
        EXPECT_EQ(alert.severity, AlertSeverity::WARNING);
        EXPECT_EQ(alert.status, AlertStatus::ACTIVE);
        EXPECT_EQ(alert.metric_value, 1000000.0);
        EXPECT_EQ(alert.threshold_value, 1500000.0);
    }
}

// Test: Alert Lifecycle Management
TEST_F(MonitoringEngineTest, AlertLifecycleManagement) {
    // Create and trigger an alert first
    AlertRule rule("RULE_001", "Test Rule", AlertCategory::OPERATIONAL_ISSUE, AlertSeverity::CRITICAL);
    rule.metric_name = "total_balance";
    rule.threshold_value = 1500000.0;
    rule.comparison_operator = "<";
    
    monitoring_engine_->addAlertRule(rule);
    mock_deal_->setTotalBalance(1000000.0);
    monitoring_engine_->evaluateDeal(*mock_deal_);
    
    std::this_thread::sleep_for(std::chrono::milliseconds(100));
    
    std::vector<Alert> alerts = monitoring_engine_->getActiveAlerts();
    ASSERT_GT(alerts.size(), 0);
    
    std::string alert_id = alerts[0].alert_id;
    
    // Test acknowledgment
    monitoring_engine_->acknowledgeAlert(alert_id, "test_user");
    Alert acknowledged_alert = monitoring_engine_->getAlert(alert_id);
    EXPECT_EQ(acknowledged_alert.status, AlertStatus::ACKNOWLEDGED);
    EXPECT_EQ(acknowledged_alert.acknowledged_by, "test_user");
    
    // Test assignment
    monitoring_engine_->assignAlert(alert_id, "assigned_user");
    Alert assigned_alert = monitoring_engine_->getAlert(alert_id);
    EXPECT_EQ(assigned_alert.assigned_to, "assigned_user");
    EXPECT_EQ(assigned_alert.status, AlertStatus::INVESTIGATING);
    
    // Test response action
    monitoring_engine_->addResponseAction(alert_id, "Contacted deal manager");
    Alert action_alert = monitoring_engine_->getAlert(alert_id);
    EXPECT_EQ(action_alert.response_actions.size(), 1);
    EXPECT_EQ(action_alert.response_actions[0], "Contacted deal manager");
    
    // Test escalation
    monitoring_engine_->escalateAlert(alert_id);
    Alert escalated_alert = monitoring_engine_->getAlert(alert_id);
    EXPECT_EQ(escalated_alert.status, AlertStatus::ESCALATED);
    EXPECT_EQ(escalated_alert.escalation_level, 1);
    EXPECT_EQ(escalated_alert.escalation_history.size(), 1);
    
    // Test resolution (this should move alert to history)
    int initial_active_count = monitoring_engine_->getActiveAlerts().size();
    monitoring_engine_->resolveAlert(alert_id, "Issue resolved by manual intervention");
    int final_active_count = monitoring_engine_->getActiveAlerts().size();
    
    EXPECT_EQ(final_active_count, initial_active_count - 1);
    
    // Verify alert is no longer active
    Alert resolved_alert = monitoring_engine_->getAlert(alert_id);
    EXPECT_EQ(resolved_alert.alert_id, ""); // Should return empty alert since it's moved to history
}

// Test: Alert Suppression
TEST_F(MonitoringEngineTest, AlertSuppression) {
    // Create alert rule and trigger alert
    AlertRule rule("RULE_001", "Test Rule", AlertCategory::OPERATIONAL_ISSUE, AlertSeverity::WARNING);
    rule.metric_name = "total_balance";
    rule.threshold_value = 1500000.0;
    rule.comparison_operator = "<";
    
    monitoring_engine_->addAlertRule(rule);
    mock_deal_->setTotalBalance(1000000.0);
    monitoring_engine_->evaluateDeal(*mock_deal_);
    
    std::this_thread::sleep_for(std::chrono::milliseconds(100));
    
    std::vector<Alert> alerts = monitoring_engine_->getActiveAlerts();
    ASSERT_GT(alerts.size(), 0);
    
    std::string alert_id = alerts[0].alert_id;
    
    // Test suppression
    monitoring_engine_->suppressAlert(alert_id, std::chrono::hours(1), "Planned maintenance");
    Alert suppressed_alert = monitoring_engine_->getAlert(alert_id);
    EXPECT_EQ(suppressed_alert.status, AlertStatus::SUPPRESSED);
    
    // Check suppression rules
    std::vector<SuppressionRule> suppressions = monitoring_engine_->getActiveSuppressions();
    EXPECT_GT(suppressions.size(), 0);
    
    if (!suppressions.empty()) {
        EXPECT_EQ(suppressions[0].alert_rule_id, "RULE_001");
        EXPECT_EQ(suppressions[0].reason, "Planned maintenance");
        EXPECT_TRUE(suppressions[0].is_active);
    }
}

// Test: Alert Filtering and Retrieval
TEST_F(MonitoringEngineTest, AlertFilteringAndRetrieval) {
    // Create multiple alert rules and trigger alerts
    AlertRule rule1("RULE_001", "Covenant Rule", AlertCategory::COVENANT_BREACH, AlertSeverity::CRITICAL);
    rule1.metric_name = "total_balance";
    rule1.threshold_value = 1500000.0;
    rule1.comparison_operator = "<";
    
    AlertRule rule2("RULE_002", "Performance Rule", AlertCategory::PERFORMANCE_DECLINE, AlertSeverity::WARNING);
    rule2.metric_name = "total_balance";
    rule2.threshold_value = 1200000.0;
    rule2.comparison_operator = "<";
    
    monitoring_engine_->addAlertRule(rule1);
    monitoring_engine_->addAlertRule(rule2);
    
    // Add multiple deals
    auto deal1 = std::make_shared<MockDealBase>("DEAL_001", 1000000.0);
    auto deal2 = std::make_shared<MockDealBase>("DEAL_002", 800000.0);
    monitoring_engine_->addDealToMonitoring(deal1);
    monitoring_engine_->addDealToMonitoring(deal2);
    
    // Trigger alerts
    monitoring_engine_->evaluateDeal(*deal1);
    monitoring_engine_->evaluateDeal(*deal2);
    
    std::this_thread::sleep_for(std::chrono::milliseconds(200));
    
    // Test filtering by deal
    std::vector<Alert> deal1_alerts = monitoring_engine_->getAlertsByDeal("DEAL_001");
    std::vector<Alert> deal2_alerts = monitoring_engine_->getAlertsByDeal("DEAL_002");
    
    EXPECT_GT(deal1_alerts.size(), 0);
    EXPECT_GT(deal2_alerts.size(), 0);
    
    // Test filtering by category
    std::vector<Alert> covenant_alerts = monitoring_engine_->getAlertsByCategory(AlertCategory::COVENANT_BREACH);
    std::vector<Alert> performance_alerts = monitoring_engine_->getAlertsByCategory(AlertCategory::PERFORMANCE_DECLINE);
    
    EXPECT_GT(covenant_alerts.size(), 0);
    EXPECT_GT(performance_alerts.size(), 0);
    
    // Test filtering by severity
    std::vector<Alert> critical_alerts = monitoring_engine_->getAlertsBySeverity(AlertSeverity::CRITICAL);
    std::vector<Alert> warning_alerts = monitoring_engine_->getAlertsBySeverity(AlertSeverity::WARNING);
    
    EXPECT_GT(critical_alerts.size(), 0);
    EXPECT_GT(warning_alerts.size(), 0);
}

// Test: Notification Channel Configuration
TEST_F(MonitoringEngineTest, NotificationChannelConfiguration) {
    NotificationConfig email_config;
    email_config.channel = NotificationChannel::EMAIL;
    email_config.recipients = {"admin@example.com", "alerts@example.com"};
    email_config.message_template = "Alert: {title} - {message}";
    email_config.is_enabled = true;
    email_config.channel_config["smtp_server"] = "smtp.example.com";
    email_config.channel_config["smtp_port"] = "587";
    
    monitoring_engine_->configureNotificationChannel(NotificationChannel::EMAIL, email_config);
    
    // Test notification channel
    EXPECT_NO_THROW(monitoring_engine_->testNotificationChannel(NotificationChannel::EMAIL, "Test message"));
    
    // Test recipient management
    AlertRule rule("RULE_001", "Test Rule", AlertCategory::SYSTEM_ERROR, AlertSeverity::INFO);
    monitoring_engine_->addAlertRule(rule);
    
    monitoring_engine_->addNotificationRecipient("RULE_001", "new_user@example.com");
    AlertRule updated_rule = monitoring_engine_->getAlertRule("RULE_001");
    EXPECT_EQ(updated_rule.recipients.size(), 1);
    EXPECT_EQ(updated_rule.recipients[0], "new_user@example.com");
    
    monitoring_engine_->removeNotificationRecipient("RULE_001", "new_user@example.com");
    updated_rule = monitoring_engine_->getAlertRule("RULE_001");
    EXPECT_EQ(updated_rule.recipients.size(), 0);
}

// Test: Escalation Rules Management
TEST_F(MonitoringEngineTest, EscalationRulesManagement) {
    EscalationRule escalation_rule;
    escalation_rule.rule_id = "ESCALATION_001";
    escalation_rule.min_severity = AlertSeverity::CRITICAL;
    escalation_rule.initial_delay = std::chrono::minutes(15);
    escalation_rule.escalation_intervals = {std::chrono::minutes(30), std::chrono::hours(1)};
    escalation_rule.escalation_contacts = {"manager@example.com", "director@example.com"};
    escalation_rule.escalation_channels = {NotificationChannel::EMAIL, NotificationChannel::SMS};
    escalation_rule.max_escalation_level = 2;
    escalation_rule.auto_escalate = true;
    
    monitoring_engine_->addEscalationRule(escalation_rule);
    
    std::vector<EscalationRule> rules = monitoring_engine_->getEscalationRules();
    EXPECT_EQ(rules.size(), 1);
    EXPECT_EQ(rules[0].rule_id, "ESCALATION_001");
    EXPECT_EQ(rules[0].min_severity, AlertSeverity::CRITICAL);
    EXPECT_EQ(rules[0].max_escalation_level, 2);
    EXPECT_TRUE(rules[0].auto_escalate);
    
    // Test escalation rule update
    escalation_rule.max_escalation_level = 3;
    monitoring_engine_->updateEscalationRule("ESCALATION_001", escalation_rule);
    
    rules = monitoring_engine_->getEscalationRules();
    EXPECT_EQ(rules[0].max_escalation_level, 3);
    
    // Test escalation rule removal
    monitoring_engine_->removeEscalationRule("ESCALATION_001");
    rules = monitoring_engine_->getEscalationRules();
    EXPECT_EQ(rules.size(), 0);
}

// Test: Suppression Rules Management
TEST_F(MonitoringEngineTest, SuppressionRulesManagement) {
    SuppressionRule suppression_rule;
    suppression_rule.rule_id = "SUPPRESSION_001";
    suppression_rule.alert_rule_id = "RULE_001";
    suppression_rule.suppression_duration = std::chrono::hours(2);
    suppression_rule.start_date = Date::todaysDate();
    suppression_rule.reason = "Scheduled maintenance window";
    suppression_rule.created_by = "admin_user";
    suppression_rule.is_active = true;
    
    monitoring_engine_->addSuppressionRule(suppression_rule);
    
    std::vector<SuppressionRule> suppressions = monitoring_engine_->getActiveSuppressions();
    EXPECT_EQ(suppressions.size(), 1);
    EXPECT_EQ(suppressions[0].rule_id, "SUPPRESSION_001");
    EXPECT_EQ(suppressions[0].alert_rule_id, "RULE_001");
    EXPECT_EQ(suppressions[0].reason, "Scheduled maintenance window");
    EXPECT_EQ(suppressions[0].created_by, "admin_user");
    EXPECT_TRUE(suppressions[0].is_active);
    
    // Test suppression rule removal
    monitoring_engine_->removeSuppressionRule("SUPPRESSION_001");
    suppressions = monitoring_engine_->getActiveSuppressions();
    EXPECT_EQ(suppressions.size(), 0);
}

// Test: Monitoring Dashboard Generation
TEST_F(MonitoringEngineTest, MonitoringDashboardGeneration) {
    // Create some alert rules and trigger alerts to populate dashboard
    AlertRule rule1("RULE_001", "Critical Rule", AlertCategory::COVENANT_BREACH, AlertSeverity::CRITICAL);
    rule1.metric_name = "total_balance";
    rule1.threshold_value = 1500000.0;
    rule1.comparison_operator = "<";
    
    AlertRule rule2("RULE_002", "Warning Rule", AlertCategory::PERFORMANCE_DECLINE, AlertSeverity::WARNING);
    rule2.metric_name = "total_balance";
    rule2.threshold_value = 1200000.0;
    rule2.comparison_operator = "<";
    
    monitoring_engine_->addAlertRule(rule1);
    monitoring_engine_->addAlertRule(rule2);
    
    mock_deal_->setTotalBalance(1000000.0);
    monitoring_engine_->evaluateDeal(*mock_deal_);
    
    std::this_thread::sleep_for(std::chrono::milliseconds(200));
    
    MonitoringDashboard dashboard = monitoring_engine_->generateDashboard();
    
    EXPECT_GT(dashboard.alert_counts_by_severity[AlertSeverity::CRITICAL], 0);
    EXPECT_GT(dashboard.alert_counts_by_severity[AlertSeverity::WARNING], 0);
    EXPECT_GT(dashboard.alert_counts_by_category[AlertCategory::COVENANT_BREACH], 0);
    EXPECT_GT(dashboard.alert_counts_by_category[AlertCategory::PERFORMANCE_DECLINE], 0);
    EXPECT_GT(dashboard.alert_counts_by_status[AlertStatus::ACTIVE], 0);
    
    EXPECT_GT(dashboard.key_metrics["total_active_alerts"], 0);
    EXPECT_EQ(dashboard.key_metrics["alert_rules"], 2);
    EXPECT_EQ(dashboard.key_metrics["monitored_deals"], 1); // mock_deal_ added during SetUp
    
    EXPECT_GT(dashboard.recent_alerts.size(), 0);
    EXPECT_GT(dashboard.critical_alerts.size(), 0);
    EXPECT_GT(dashboard.system_health_indicators.size(), 0);
}

// Test: Alert Statistics
TEST_F(MonitoringEngineTest, AlertStatistics) {
    // Create rules and trigger alerts
    AlertRule critical_rule("RULE_001", "Critical Rule", AlertCategory::COVENANT_BREACH, AlertSeverity::CRITICAL);
    critical_rule.metric_name = "total_balance";
    critical_rule.threshold_value = 1500000.0;
    critical_rule.comparison_operator = "<";
    
    AlertRule warning_rule("RULE_002", "Warning Rule", AlertCategory::PERFORMANCE_DECLINE, AlertSeverity::WARNING);
    warning_rule.metric_name = "total_balance";
    warning_rule.threshold_value = 1200000.0;
    warning_rule.comparison_operator = "<";
    
    monitoring_engine_->addAlertRule(critical_rule);
    monitoring_engine_->addAlertRule(warning_rule);
    
    mock_deal_->setTotalBalance(1000000.0);
    monitoring_engine_->evaluateDeal(*mock_deal_);
    
    std::this_thread::sleep_for(std::chrono::milliseconds(200));
    
    std::map<std::string, int> stats = monitoring_engine_->getAlertStatistics();
    
    EXPECT_GT(stats["total_active"], 0);
    EXPECT_EQ(stats["total_rules"], 2);
    EXPECT_GE(stats["total_history"], 0);
    EXPECT_GT(stats["severity_CRITICAL"], 0);
    EXPECT_GT(stats["severity_WARNING"], 0);
    EXPECT_GT(stats["category_COVENANT_BREACH"], 0);
    EXPECT_GT(stats["category_PERFORMANCE_DECLINE"], 0);
}

// Test: Recent Alerts Retrieval
TEST_F(MonitoringEngineTest, RecentAlertsRetrieval) {
    // Create multiple alert rules
    for (int i = 1; i <= 5; ++i) {
        AlertRule rule("RULE_" + std::string(3 - std::to_string(i).length(), '0') + std::to_string(i), 
                      "Rule " + std::to_string(i), 
                      AlertCategory::OPERATIONAL_ISSUE, 
                      AlertSeverity::INFO);
        rule.metric_name = "total_balance";
        rule.threshold_value = 2000000.0; // High threshold to trigger alerts
        rule.comparison_operator = "<";
        monitoring_engine_->addAlertRule(rule);
    }
    
    mock_deal_->setTotalBalance(1000000.0);
    monitoring_engine_->evaluateDeal(*mock_deal_);
    
    std::this_thread::sleep_for(std::chrono::milliseconds(200));
    
    // Test recent alerts with limit
    std::vector<Alert> recent_alerts_3 = monitoring_engine_->getRecentAlerts(3);
    EXPECT_LE(recent_alerts_3.size(), 3);
    
    std::vector<Alert> recent_alerts_10 = monitoring_engine_->getRecentAlerts(10);
    EXPECT_LE(recent_alerts_10.size(), 10);
    
    // Verify alerts are sorted by trigger date (most recent first)
    if (recent_alerts_10.size() > 1) {
        for (size_t i = 1; i < recent_alerts_10.size(); ++i) {
            EXPECT_GE(recent_alerts_10[i-1].trigger_date.serialNumber(), recent_alerts_10[i].trigger_date.serialNumber());
        }
    }
}

// Test: Health Monitoring
TEST_F(MonitoringEngineTest, HealthMonitoring) {
    // Test initial health status
    std::vector<std::string> health_status = monitoring_engine_->getSystemHealthStatus();
    EXPECT_GT(health_status.size(), 0);
    
    // Verify basic health indicators
    bool found_system_startup = false;
    bool found_monitoring_active = false;
    
    for (const std::string& status : health_status) {
        if (status.find("system_startup") != std::string::npos) {
            found_system_startup = true;
            EXPECT_TRUE(status.find("OK") != std::string::npos);
        }
        if (status.find("monitoring_active") != std::string::npos) {
            found_monitoring_active = true;
            EXPECT_TRUE(status.find("false") != std::string::npos); // Not started yet
        }
    }
    
    EXPECT_TRUE(found_system_startup);
    EXPECT_TRUE(found_monitoring_active);
    
    // Test health check
    EXPECT_NO_THROW(monitoring_engine_->performHealthCheck());
    
    // Test monitoring metrics
    std::map<std::string, double> metrics = monitoring_engine_->getMonitoringMetrics();
    EXPECT_GT(metrics.size(), 0);
    
    // Verify expected metrics exist
    EXPECT_TRUE(metrics.find("alerts_processed_per_minute") != metrics.end());
    EXPECT_TRUE(metrics.find("average_alert_resolution_time") != metrics.end());
    EXPECT_TRUE(metrics.find("system_cpu_usage") != metrics.end());
    EXPECT_TRUE(metrics.find("memory_usage_mb") != metrics.end());
    EXPECT_TRUE(metrics.find("disk_usage_percent") != metrics.end());
}

// Test: Monitoring System Start/Stop
TEST_F(MonitoringEngineTest, MonitoringSystemStartStop) {
    EXPECT_FALSE(monitoring_engine_->isMonitoringActive());
    
    // Start monitoring
    monitoring_engine_->startMonitoring();
    EXPECT_TRUE(monitoring_engine_->isMonitoringActive());
    
    // Allow monitoring threads to initialize
    std::this_thread::sleep_for(std::chrono::milliseconds(100));
    
    // Stop monitoring
    monitoring_engine_->stopMonitoring();
    EXPECT_FALSE(monitoring_engine_->isMonitoringActive());
    
    // Test multiple start/stop cycles
    monitoring_engine_->startMonitoring();
    EXPECT_TRUE(monitoring_engine_->isMonitoringActive());
    
    monitoring_engine_->stopMonitoring();
    EXPECT_FALSE(monitoring_engine_->isMonitoringActive());
}

// Test: Advanced Features Configuration
TEST_F(MonitoringEngineTest, AdvancedFeaturesConfiguration) {
    // Test anomaly detection
    monitoring_engine_->enableAnomalyDetection(true);
    monitoring_engine_->enableAnomalyDetection(false);
    
    // Test trend analysis configuration
    monitoring_engine_->configureTrendAnalysis("total_balance", 30);
    monitoring_engine_->configureTrendAnalysis("net_present_value", 60);
    
    // Test custom metric calculator
    monitoring_engine_->addCustomMetricCalculator("custom_metric", 
        [](const DealBase& deal) -> double {
            return deal.getTotalAccountBalance() * 0.1; // 10% of total balance
        });
    
    // No direct way to test these without triggering evaluation, but they should not throw
    EXPECT_NO_THROW(monitoring_engine_->evaluateDeal(*mock_deal_));
}

// Test: Bulk Alert Operations
TEST_F(MonitoringEngineTest, BulkAlertOperations) {
    // Create multiple alert rules and trigger alerts
    std::vector<std::string> alert_ids;
    
    for (int i = 1; i <= 3; ++i) {
        AlertRule rule("RULE_" + std::string(3 - std::to_string(i).length(), '0') + std::to_string(i), 
                      "Rule " + std::to_string(i), 
                      AlertCategory::OPERATIONAL_ISSUE, 
                      AlertSeverity::WARNING);
        rule.metric_name = "total_balance";
        rule.threshold_value = 2000000.0;
        rule.comparison_operator = "<";
        monitoring_engine_->addAlertRule(rule);
    }
    
    mock_deal_->setTotalBalance(1000000.0);
    monitoring_engine_->evaluateDeal(*mock_deal_);
    
    std::this_thread::sleep_for(std::chrono::milliseconds(200));
    
    // Get alert IDs
    std::vector<Alert> alerts = monitoring_engine_->getActiveAlerts();
    for (const Alert& alert : alerts) {
        alert_ids.push_back(alert.alert_id);
    }
    
    ASSERT_GT(alert_ids.size(), 0);
    
    // Test bulk acknowledgment
    monitoring_engine_->acknowledgeMultipleAlerts(alert_ids, "bulk_user");
    
    for (const std::string& alert_id : alert_ids) {
        Alert alert = monitoring_engine_->getAlert(alert_id);
        EXPECT_EQ(alert.status, AlertStatus::ACKNOWLEDGED);
        EXPECT_EQ(alert.acknowledged_by, "bulk_user");
    }
    
    // Test bulk status update
    monitoring_engine_->bulkUpdateAlertStatus(alert_ids, AlertStatus::INVESTIGATING);
    
    for (const std::string& alert_id : alert_ids) {
        Alert alert = monitoring_engine_->getAlert(alert_id);
        EXPECT_EQ(alert.status, AlertStatus::INVESTIGATING);
    }
    
    // Test bulk resolution
    int initial_active_count = monitoring_engine_->getActiveAlerts().size();
    monitoring_engine_->resolveMultipleAlerts(alert_ids, "Bulk resolution test");
    int final_active_count = monitoring_engine_->getActiveAlerts().size();
    
    EXPECT_EQ(final_active_count, initial_active_count - alert_ids.size());
}

// Test: JSON Export/Import Functionality
TEST_F(MonitoringEngineTest, JSONExportImportFunctionality) {
    // Create alert rules for export
    AlertRule rule1("RULE_001", "Export Rule 1", AlertCategory::COVENANT_BREACH, AlertSeverity::CRITICAL);
    rule1.metric_name = "total_balance";
    rule1.threshold_value = 1000000.0;
    rule1.comparison_operator = "<=";
    
    AlertRule rule2("RULE_002", "Export Rule 2", AlertCategory::PERFORMANCE_DECLINE, AlertSeverity::WARNING);
    rule2.metric_name = "net_present_value";
    rule2.threshold_value = 500000.0;
    rule2.comparison_operator = "<";
    
    monitoring_engine_->addAlertRule(rule1);
    monitoring_engine_->addAlertRule(rule2);
    
    // Test alert rules export
    std::string rules_json = monitoring_engine_->exportAlertRulesToJSON();
    EXPECT_GT(rules_json.length(), 0);
    EXPECT_TRUE(rules_json.find("RULE_001") != std::string::npos);
    EXPECT_TRUE(rules_json.find("RULE_002") != std::string::npos);
    EXPECT_TRUE(rules_json.find("Export Rule 1") != std::string::npos);
    EXPECT_TRUE(rules_json.find("Export Rule 2") != std::string::npos);
    EXPECT_TRUE(rules_json.find("COVENANT_BREACH") != std::string::npos);
    EXPECT_TRUE(rules_json.find("PERFORMANCE_DECLINE") != std::string::npos);
    
    // Test alerts export (with date range)
    Date start_date = Date::todaysDate();
    Date end_date = Date::todaysDate();
    
    std::string alerts_json = monitoring_engine_->exportAlertsToJSON(start_date, end_date);
    EXPECT_GT(alerts_json.length(), 0);
    EXPECT_TRUE(alerts_json.find("export_date") != std::string::npos);
    EXPECT_TRUE(alerts_json.find("date_range") != std::string::npos);
    EXPECT_TRUE(alerts_json.find("alerts") != std::string::npos);
    
    // Test import (basic functionality - just ensure it doesn't crash)
    EXPECT_NO_THROW(monitoring_engine_->importAlertRulesFromJSON(rules_json));
}

// Test: Monitoring Interval Configuration
TEST_F(MonitoringEngineTest, MonitoringIntervalConfiguration) {
    // Test setting different monitoring intervals
    monitoring_engine_->setMonitoringInterval(std::chrono::seconds(30));
    monitoring_engine_->setMonitoringInterval(std::chrono::minutes(1));
    monitoring_engine_->setMonitoringInterval(std::chrono::minutes(5));
    
    // No direct way to verify the interval without accessing private members,
    // but the method should not throw exceptions
    EXPECT_NO_THROW(monitoring_engine_->setMonitoringInterval(std::chrono::minutes(10)));
}

// Test: Alert Rule Constructor and Default Values
TEST_F(MonitoringEngineTest, AlertRuleConstructorAndDefaults) {
    AlertRule rule("TEST_RULE", "Test Rule Name", AlertCategory::CUSTOM_ALERT, AlertSeverity::INFO);
    
    EXPECT_EQ(rule.rule_id, "TEST_RULE");
    EXPECT_EQ(rule.name, "Test Rule Name");
    EXPECT_EQ(rule.category, AlertCategory::CUSTOM_ALERT);
    EXPECT_EQ(rule.severity, AlertSeverity::INFO);
    EXPECT_EQ(rule.trigger_type, AlertTriggerType::THRESHOLD_BREACH);
    EXPECT_EQ(rule.threshold_value, 0.0);
    EXPECT_EQ(rule.comparison_operator, ">=");
    EXPECT_EQ(rule.evaluation_frequency, std::chrono::minutes(5));
    EXPECT_EQ(rule.minimum_duration, std::chrono::seconds(0));
    EXPECT_EQ(rule.suppression_window, std::chrono::minutes(15));
    EXPECT_TRUE(rule.is_active);
}

// Test: Alert Constructor and Default Values
TEST_F(MonitoringEngineTest, AlertConstructorAndDefaults) {
    Alert alert("TEST_ALERT", "TEST_RULE", "TEST_DEAL", "Test Alert Title", 
                AlertCategory::SYSTEM_ERROR, AlertSeverity::CRITICAL);
    
    EXPECT_EQ(alert.alert_id, "TEST_ALERT");
    EXPECT_EQ(alert.rule_id, "TEST_RULE");
    EXPECT_EQ(alert.deal_id, "TEST_DEAL");
    EXPECT_EQ(alert.title, "Test Alert Title");
    EXPECT_EQ(alert.category, AlertCategory::SYSTEM_ERROR);
    EXPECT_EQ(alert.severity, AlertSeverity::CRITICAL);
    EXPECT_EQ(alert.status, AlertStatus::ACTIVE);
    EXPECT_EQ(alert.metric_value, 0.0);
    EXPECT_EQ(alert.threshold_value, 0.0);
    EXPECT_EQ(alert.escalation_level, 0);
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}