#pragma once

#include <crow.h>
#include <nlohmann/json.hpp>
#include <string>
#include <vector>
#include <unordered_map>
#include <memory>
#include <thread>
#include <mutex>

// Forward declarations to avoid circular dependencies
namespace Structura {
    class AnalyticsEngine;
    class ReportingEngine;
    class ValidationEngine;
    class MonitoringEngine;
    class Deal;
}

#include "../core/types.h"

using json = nlohmann::json;

namespace structura {
namespace api {

/**
 * @brief Vector parameter structure for 132-period forecasts
 */
struct VectorParameter {
    std::string type;                    // "vector", "constant", "seasonal", etc.
    double baseValue;                    // Base value for calculations
    std::vector<double> values;          // 132 monthly values or multipliers
    std::string description;             // Human-readable description
    std::string startDate;               // Start date for the vector
    
    // Validation
    bool isValid() const {
        return values.size() == 132 && !type.empty();
    }
    
    // Convert from JSON
    static VectorParameter fromJson(const json& j);
    
    // Convert to JSON
    json toJson() const;
};

/**
 * @brief Deal performance assumptions with vector support
 */
struct PerformanceAssumptions {
    VectorParameter defaultRate;
    VectorParameter prepaymentRate;
    VectorParameter recoveryRate;
    VectorParameter recoveryLag;
    std::unordered_map<std::string, VectorParameter> interestRates;
    std::unordered_map<std::string, VectorParameter> macroDrivers;
    
    static PerformanceAssumptions fromJson(const json& j);
    json toJson() const;
};

/**
 * @brief Scenario definition for multi-scenario analysis
 */
struct Scenario {
    std::string name;
    std::string description;
    PerformanceAssumptions assumptions;
    std::unordered_map<std::string, json> customParameters;
    
    static Scenario fromJson(const json& j);
    json toJson() const;
};

/**
 * @brief HTTP API Server for Structura JSON DSL
 */
class ApiServer {
public:
    ApiServer(int port = 8080);
    ~ApiServer();
    
    // Server lifecycle
    void start();
    void stop();
    void waitForShutdown();
    
    // Configuration
    void setPort(int port) { port_ = port; }
    void setThreads(size_t threads) { threads_ = threads; }
    void enableCORS(bool enable = true) { cors_enabled_ = enable; }
    void setLogLevel(crow::LogLevel level);
    
private:
    // Core components
    crow::SimpleApp app_;
    int port_;
    size_t threads_;
    bool cors_enabled_;
    bool running_;
    std::thread server_thread_;
    std::mutex deals_mutex_;
    
    // Engine instances
    std::unique_ptr<Structura::AnalyticsEngine> analytics_engine_;
    std::unique_ptr<Structura::ReportingEngine> reporting_engine_;
    std::unique_ptr<Structura::ValidationEngine> validation_engine_;
    std::unique_ptr<Structura::MonitoringEngine> monitoring_engine_;
    
    // Deal storage (in-memory for now, will move to database)
    std::unordered_map<std::string, json> deals_;
    std::unordered_map<std::string, json> scenarios_;
    
    // Route handlers - Deal CRUD
    void setupDealRoutes();
    crow::response createDeal(const crow::request& req);
    crow::response getDeal(const std::string& dealId);
    crow::response updateDeal(const std::string& dealId, const crow::request& req);
    crow::response deleteDeal(const std::string& dealId);
    crow::response listDeals();
    
    // Route handlers - Bulk Operations
    void setupBulkRoutes();
    crow::response bulkCreateDeals(const crow::request& req);
    crow::response bulkAddAssets(const std::string& dealId, const crow::request& req);
    crow::response bulkUpdateDeals(const crow::request& req);
    
    // Route handlers - Scenario Analysis
    void setupScenarioRoutes();
    crow::response runScenarios(const std::string& dealId, const crow::request& req);
    crow::response runMultiDealScenarios(const crow::request& req);
    crow::response getScenarioResults(const std::string& scenarioId);
    
    // Route handlers - Analytics
    void setupAnalyticsRoutes();
    crow::response runAnalytics(const std::string& dealId, const crow::request& req);
    crow::response getPerformanceMetrics(const std::string& dealId);
    crow::response runStressTesting(const std::string& dealId, const crow::request& req);
    
    // Route handlers - Reporting
    void setupReportingRoutes();
    crow::response generateReport(const std::string& dealId, const crow::request& req);
    crow::response getAvailableReports();
    crow::response downloadReport(const std::string& reportId, const std::string& format);
    
    // Route handlers - Validation
    void setupValidationRoutes();
    crow::response validateDeal(const std::string& dealId, const crow::request& req);
    crow::response validateAssets(const std::string& dealId, const crow::request& req);
    crow::response runComplianceCheck(const std::string& dealId, const crow::request& req);
    
    // Route handlers - Monitoring
    void setupMonitoringRoutes();
    crow::response startMonitoring(const std::string& dealId, const crow::request& req);
    crow::response stopMonitoring(const std::string& dealId);
    crow::response getAlerts(const std::string& dealId);
    crow::response acknowledgeAlert(const std::string& dealId, const std::string& alertId);
    
    // Route handlers - Vector Utilities
    void setupVectorRoutes();
    crow::response createSeasonalVector(const crow::request& req);
    crow::response createRampVector(const crow::request& req);
    crow::response applyShockVector(const crow::request& req);
    crow::response interpolateVector(const crow::request& req);
    crow::response validateVector(const crow::request& req);
    
    // WebSocket support for real-time updates
    void setupWebSocketRoutes();
    void handleWebSocketConnection(crow::websocket::connection& conn);
    void broadcastAlert(const std::string& dealId, const json& alert);
    
    // Utility functions
    void setupMiddleware();
    void setupErrorHandlers();
    crow::response createSuccessResponse(const json& data, int status = 200);
    crow::response createErrorResponse(const std::string& message, int status = 400);
    crow::response createErrorResponse(const std::string& message, const std::string& detail, int status = 400);
    
    // JSON validation
    bool validateDealJson(const json& deal, std::string& error);
    bool validateScenarioJson(const json& scenario, std::string& error);
    bool validateVectorParameter(const json& param, std::string& error);
    
    // Utility functions
    std::string generateDealId();
    std::string generateTimestamp();
    
    // Deal processing
    Structura::Deal jsonToDeal(const json& dealJson);
    json dealToJson(const Structura::Deal& deal);
    
    // Vector processing
    std::vector<double> processVector(const VectorParameter& param, size_t expectedSize = 132);
    VectorParameter createSeasonalPattern(double baseValue, const std::vector<double>& seasonalFactors, int years);
    VectorParameter createRampPattern(double initial, double peak, int peakPeriod, int totalPeriods);
    VectorParameter applyShocks(const VectorParameter& baseVector, const std::vector<json>& shocks);
    
    // Scenario execution
    std::string executeScenario(const std::string& dealId, const Scenario& scenario);
    void processScenarioAsync(const std::string& scenarioId, const std::string& dealId, const Scenario& scenario);
    
    // Health and diagnostics
    void setupHealthRoutes();
    crow::response healthCheck();
    crow::response getSystemMetrics();
    crow::response getApiDocumentation();
};

} // namespace api
} // namespace structura