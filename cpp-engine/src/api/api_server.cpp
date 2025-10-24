#include "api_server.h"
#include <iostream>
#include <chrono>
#include <iomanip>
#include <sstream>
#include <algorithm>
#include <random>

// Include actual engine implementations
#include "../analytics/analytics_engine.h"
#include "../reporting/reporting_engine.h" 
#include "../validation/validation_engine.h"
#include "../monitoring/monitoring_engine.h"
// Schema validator will be implemented later

namespace structura {
namespace api {

// VectorParameter implementation
VectorParameter VectorParameter::fromJson(const json& j) {
    VectorParameter param;
    param.type = j.value("type", "vector");
    param.baseValue = j.value("baseValue", 0.0);
    param.description = j.value("description", "");
    param.startDate = j.value("startDate", "");
    
    if (j.contains("values") && j["values"].is_array()) {
        param.values = j["values"].get<std::vector<double>>();
    } else if (j.contains("monthlyMultipliers") && j["monthlyMultipliers"].is_array()) {
        param.values = j["monthlyMultipliers"].get<std::vector<double>>();
    }
    
    return param;
}

json VectorParameter::toJson() const {
    json j;
    j["type"] = type;
    j["baseValue"] = baseValue;
    j["values"] = values;
    j["description"] = description;
    j["startDate"] = startDate;
    return j;
}

// PerformanceAssumptions implementation
PerformanceAssumptions PerformanceAssumptions::fromJson(const json& j) {
    PerformanceAssumptions assumptions;
    
    if (j.contains("defaultRate")) {
        assumptions.defaultRate = VectorParameter::fromJson(j["defaultRate"]);
    }
    if (j.contains("prepaymentRate")) {
        assumptions.prepaymentRate = VectorParameter::fromJson(j["prepaymentRate"]);
    }
    if (j.contains("recoveryRate")) {
        assumptions.recoveryRate = VectorParameter::fromJson(j["recoveryRate"]);
    }
    if (j.contains("recoveryLag")) {
        assumptions.recoveryLag = VectorParameter::fromJson(j["recoveryLag"]);
    }
    
    if (j.contains("interestRates")) {
        for (auto& [key, value] : j["interestRates"].items()) {
            assumptions.interestRates[key] = VectorParameter::fromJson(value);
        }
    }
    
    if (j.contains("macroDrivers")) {
        for (auto& [key, value] : j["macroDrivers"].items()) {
            assumptions.macroDrivers[key] = VectorParameter::fromJson(value);
        }
    }
    
    return assumptions;
}

json PerformanceAssumptions::toJson() const {
    json j;
    j["defaultRate"] = defaultRate.toJson();
    j["prepaymentRate"] = prepaymentRate.toJson();
    j["recoveryRate"] = recoveryRate.toJson();
    j["recoveryLag"] = recoveryLag.toJson();
    
    for (const auto& [key, value] : interestRates) {
        j["interestRates"][key] = value.toJson();
    }
    
    for (const auto& [key, value] : macroDrivers) {
        j["macroDrivers"][key] = value.toJson();
    }
    
    return j;
}

// Scenario implementation
Scenario Scenario::fromJson(const json& j) {
    Scenario scenario;
    scenario.name = j.value("name", "");
    scenario.description = j.value("description", "");
    
    if (j.contains("assumptions")) {
        scenario.assumptions = PerformanceAssumptions::fromJson(j["assumptions"]);
    }
    
    if (j.contains("customParameters")) {
        scenario.customParameters = j["customParameters"];
    }
    
    return scenario;
}

json Scenario::toJson() const {
    json j;
    j["name"] = name;
    j["description"] = description;
    j["assumptions"] = assumptions.toJson();
    j["customParameters"] = customParameters;
    return j;
}

// ApiServer implementation
ApiServer::ApiServer(int port) 
    : port_(port), threads_(4), cors_enabled_(true), running_(false) {
    
    // Initialize engines
    analytics_engine_ = std::make_unique<Structura::AnalyticsEngine>();
    reporting_engine_ = std::make_unique<Structura::ReportingEngine>();
    validation_engine_ = std::make_unique<Structura::ValidationEngine>();
    monitoring_engine_ = std::make_unique<Structura::MonitoringEngine>();
    
    // Setup all routes
    setupMiddleware();
    setupDealRoutes();
    setupBulkRoutes();
    setupScenarioRoutes();
    setupAnalyticsRoutes();
    setupReportingRoutes();
    setupValidationRoutes();
    setupMonitoringRoutes();
    setupVectorRoutes();
    setupWebSocketRoutes();
    setupHealthRoutes();
    setupErrorHandlers();
    
    std::cout << "Structura API Server initialized on port " << port_ << std::endl;
}

ApiServer::~ApiServer() {
    if (running_) {
        stop();
    }
}

void ApiServer::start() {
    if (running_) {
        std::cout << "Server is already running" << std::endl;
        return;
    }
    
    running_ = true;
    
    // Start server in separate thread
    server_thread_ = std::thread([this]() {
        std::cout << "Starting Structura API Server on port " << port_ << std::endl;
        std::cout << "CORS enabled: " << (cors_enabled_ ? "yes" : "no") << std::endl;
        std::cout << "Threads: " << threads_ << std::endl;
        
        app_.port(port_).multithreaded().run();
    });
    
    // Give server time to start
    std::this_thread::sleep_for(std::chrono::milliseconds(100));
    std::cout << "API Server started successfully!" << std::endl;
    std::cout << "API Documentation available at: http://localhost:" << port_ << "/api/docs" << std::endl;
}

void ApiServer::stop() {
    if (!running_) {
        return;
    }
    
    std::cout << "Stopping API Server..." << std::endl;
    running_ = false;
    
    app_.stop();
    
    if (server_thread_.joinable()) {
        server_thread_.join();
    }
    
    std::cout << "API Server stopped." << std::endl;
}

void ApiServer::waitForShutdown() {
    if (server_thread_.joinable()) {
        server_thread_.join();
    }
}

void ApiServer::setLogLevel(crow::LogLevel level) {
    app_.loglevel(level);
}

void ApiServer::setupMiddleware() {
    // CORS middleware
    if (cors_enabled_) {
                // Note: Simple CORS handling will be done manually in each response
        // Crow v1.2 doesn't have a built-in CORSHandler middleware
    }
    
    // Request logging middleware
    CROW_ROUTE(app_, "/")
    .methods("GET"_method, "POST"_method, "PUT"_method, "DELETE"_method, "OPTIONS"_method)
    ([this](const crow::request& req) {
        auto now = std::chrono::system_clock::now();
        auto time_t = std::chrono::system_clock::to_time_t(now);
        
        std::ostringstream oss;
        oss << std::put_time(std::localtime(&time_t), "%Y-%m-%d %H:%M:%S");
        
        std::cout << "[" << oss.str() << "] " 
                  << static_cast<int>(req.method) << " " << req.url << std::endl;
        
        return createSuccessResponse({
            {"message", "Structura API Server"},
            {"version", "1.0.0"},
            {"timestamp", oss.str()},
            {"endpoints", {
                {"deals", "/api/v1/deals"},
                {"scenarios", "/api/v1/scenarios"},
                {"analytics", "/api/v1/analytics"},
                {"reports", "/api/v1/reports"},
                {"validation", "/api/v1/validation"},
                {"monitoring", "/api/v1/monitoring"},
                {"vectors", "/api/v1/vectors"},
                {"health", "/api/health"},
                {"docs", "/api/docs"}
            }}
        });
    });
}

void ApiServer::setupDealRoutes() {
    // Create deal
    CROW_ROUTE(app_, "/api/v1/deals").methods("POST"_method)
    ([this](const crow::request& req) {
        return createDeal(req);
    });
    
    // Get deal
    CROW_ROUTE(app_, "/api/v1/deals/<string>").methods("GET"_method)
    ([this](const std::string& dealId) {
        return getDeal(dealId);
    });
    
    // Update deal
    CROW_ROUTE(app_, "/api/v1/deals/<string>").methods("PUT"_method)
    ([this](const crow::request& req, const std::string& dealId) {
        return updateDeal(dealId, req);
    });
    
    // Delete deal
    CROW_ROUTE(app_, "/api/v1/deals/<string>").methods("DELETE"_method)
    ([this](const std::string& dealId) {
        return deleteDeal(dealId);
    });
    
    // List deals
    CROW_ROUTE(app_, "/api/v1/deals").methods("GET"_method)
    ([this]() {
        return listDeals();
    });
}

void ApiServer::setupBulkRoutes() {
    // Bulk create deals
    CROW_ROUTE(app_, "/api/v1/deals/bulk").methods("POST"_method)
    ([this](const crow::request& req) {
        return bulkCreateDeals(req);
    });
    
    // Bulk add assets
    CROW_ROUTE(app_, "/api/v1/deals/<string>/assets/bulk").methods("POST"_method)
    ([this](const crow::request& req, const std::string& dealId) {
        return bulkAddAssets(dealId, req);
    });
}

void ApiServer::setupScenarioRoutes() {
    // Run scenarios
    CROW_ROUTE(app_, "/api/v1/deals/<string>/scenarios").methods("POST"_method)
    ([this](const crow::request& req, const std::string& dealId) {
        return runScenarios(dealId, req);
    });
    
    // Multi-deal scenarios
    CROW_ROUTE(app_, "/api/v1/scenarios/multi-deal").methods("POST"_method)
    ([this](const crow::request& req) {
        return runMultiDealScenarios(req);
    });
}

void ApiServer::setupVectorRoutes() {
    // Create seasonal vector
    CROW_ROUTE(app_, "/api/v1/vectors/seasonal").methods("POST"_method)
    ([this](const crow::request& req) {
        return createSeasonalVector(req);
    });
    
    // Create ramp vector
    CROW_ROUTE(app_, "/api/v1/vectors/ramp").methods("POST"_method)
    ([this](const crow::request& req) {
        return createRampVector(req);
    });
    
    // Apply shock to vector
    CROW_ROUTE(app_, "/api/v1/vectors/shock").methods("POST"_method)
    ([this](const crow::request& req) {
        return applyShockVector(req);
    });
    
    // Interpolate vector
    CROW_ROUTE(app_, "/api/v1/vectors/interpolate").methods("POST"_method)
    ([this](const crow::request& req) {
        return interpolateVector(req);
    });
    
    // Validate vector
    CROW_ROUTE(app_, "/api/v1/vectors/validate").methods("POST"_method)
    ([this](const crow::request& req) {
        return validateVector(req);
    });
}

void ApiServer::setupHealthRoutes() {
    // Health check
    CROW_ROUTE(app_, "/api/health").methods("GET"_method)
    ([this]() {
        return healthCheck();
    });
    
    // System metrics
    CROW_ROUTE(app_, "/api/metrics").methods("GET"_method)
    ([this]() {
        return getSystemMetrics();
    });
    
    // API documentation
    CROW_ROUTE(app_, "/api/docs").methods("GET"_method)
    ([this]() {
        return getApiDocumentation();
    });
}

void ApiServer::setupErrorHandlers() {
    // Note: Crow v1.2 doesn't have handle_404, we'll handle this in route setup
    // For now, we'll add a catch-all route later if needed
}

// Response helpers
crow::response ApiServer::createSuccessResponse(const json& data, int status) {
    crow::response res(status);
    res.set_header("Content-Type", "application/json");
    res.body = data.dump(2);
    return res;
}

crow::response ApiServer::createErrorResponse(const std::string& message, int status) {
    json error_json = {
        {"error", "API Error"},
        {"message", message},
        {"timestamp", std::chrono::duration_cast<std::chrono::seconds>(
            std::chrono::system_clock::now().time_since_epoch()).count()}
    };
    
    crow::response res(status);
    res.set_header("Content-Type", "application/json");
    res.body = error_json.dump(2);
    return res;
}

crow::response ApiServer::createErrorResponse(const std::string& message, const std::string& detail, int status) {
    json error_json = {
        {"error", "API Error"},
        {"message", message},
        {"detail", detail},
        {"timestamp", std::chrono::duration_cast<std::chrono::seconds>(
            std::chrono::system_clock::now().time_since_epoch()).count()}
    };
    
    crow::response res(status);
    res.set_header("Content-Type", "application/json");
    res.body = error_json.dump(2);
    return res;
}

// Health check implementation
crow::response ApiServer::healthCheck() {
    json health = {
        {"status", "healthy"},
        {"timestamp", std::chrono::duration_cast<std::chrono::seconds>(
            std::chrono::system_clock::now().time_since_epoch()).count()},
        {"services", {
            {"analytics_engine", analytics_engine_ ? "healthy" : "unavailable"},
            {"reporting_engine", reporting_engine_ ? "healthy" : "unavailable"},
            {"validation_engine", validation_engine_ ? "healthy" : "unavailable"},
            {"monitoring_engine", monitoring_engine_ ? "healthy" : "unavailable"}
        }},
        {"metrics", {
            {"deals_count", deals_.size()},
            {"scenarios_count", scenarios_.size()},
            {"port", port_},
            {"threads", threads_}
        }}
    };
    
    return createSuccessResponse(health);
}

crow::response ApiServer::getSystemMetrics() {
    json metrics = {
        {"system", {
            {"uptime_seconds", std::chrono::duration_cast<std::chrono::seconds>(
                std::chrono::steady_clock::now().time_since_epoch()).count()},
            {"memory_usage", "TODO: Implement memory tracking"},
            {"cpu_usage", "TODO: Implement CPU tracking"}
        }},
        {"api", {
            {"total_requests", "TODO: Implement request counting"},
            {"avg_response_time", "TODO: Implement response time tracking"},
            {"active_connections", "TODO: Implement connection tracking"}
        }},
        {"data", {
            {"deals_stored", deals_.size()},
            {"scenarios_stored", scenarios_.size()},
            {"cache_hit_rate", "TODO: Implement cache metrics"}
        }}
    };
    
    return createSuccessResponse(metrics);
}

crow::response ApiServer::getApiDocumentation() {
    json docs = {
        {"title", "Structura JSON DSL API"},
        {"version", "1.0.0"},
        {"description", "Comprehensive structured finance API with vector-based parameter support"},
        {"base_url", "http://localhost:" + std::to_string(port_)},
        {"endpoints", {
            {"deals", {
                {"POST /api/v1/deals", "Create new deal"},
                {"GET /api/v1/deals/{id}", "Get deal by ID"},
                {"PUT /api/v1/deals/{id}", "Update deal"},
                {"DELETE /api/v1/deals/{id}", "Delete deal"},
                {"GET /api/v1/deals", "List all deals"}
            }},
            {"scenarios", {
                {"POST /api/v1/deals/{id}/scenarios", "Run scenario analysis"},
                {"POST /api/v1/scenarios/multi-deal", "Multi-deal scenario analysis"}
            }},
            {"vectors", {
                {"POST /api/v1/vectors/seasonal", "Create seasonal pattern"},
                {"POST /api/v1/vectors/ramp", "Create ramp pattern"},
                {"POST /api/v1/vectors/shock", "Apply shock to vector"},
                {"POST /api/v1/vectors/interpolate", "Interpolate sparse vector"},
                {"POST /api/v1/vectors/validate", "Validate vector parameters"}
            }}
        }},
        {"vector_support", {
            {"description", "All forward parameters support 132-period vectors (11 years monthly)"},
            {"formats", json::array({"monthly_multipliers", "absolute_values", "seasonal_patterns"})},
            {"utilities", json::array({"interpolation", "shock_application", "validation"})}
        }}
    };
    
    return createSuccessResponse(docs);
}

// Placeholder implementations for route handlers
crow::response ApiServer::createDeal(const crow::request& req) {
    try {
        json deal_json = json::parse(req.body);
        
        std::string error;
        if (!validateDealJson(deal_json, error)) {
            return createErrorResponse("Invalid deal JSON", error, 400);
        }
        
        // Generate deal ID
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_int_distribution<> dis(100000, 999999);
        std::string dealId = "DEAL_" + std::to_string(dis(gen));
        
        // Store deal
        std::lock_guard<std::mutex> lock(deals_mutex_);
        deals_[dealId] = deal_json;
        
        json response = {
            {"message", "Deal created successfully"},
            {"dealId", dealId},
            {"deal", deal_json}
        };
        
        return createSuccessResponse(response, 201);
        
    } catch (const json::exception& e) {
        return createErrorResponse("Invalid JSON", e.what(), 400);
    } catch (const std::exception& e) {
        return createErrorResponse("Internal error", e.what(), 500);
    }
}

crow::response ApiServer::getDeal(const std::string& dealId) {
    std::lock_guard<std::mutex> lock(deals_mutex_);
    
    auto it = deals_.find(dealId);
    if (it == deals_.end()) {
        return createErrorResponse("Deal not found", "Deal ID: " + dealId, 404);
    }
    
    json response = {
        {"dealId", dealId},
        {"deal", it->second}
    };
    
    return createSuccessResponse(response);
}

crow::response ApiServer::listDeals() {
    std::lock_guard<std::mutex> lock(deals_mutex_);
    
    json deals_list = json::array();
    for (const auto& [id, deal] : deals_) {
        json deal_summary = {
            {"dealId", id},
            {"name", deal.value("name", "")},
            {"created", deal.value("created", "")},
            {"status", deal.value("status", "active")}
        };
        deals_list.push_back(deal_summary);
    }
    
    json response = {
        {"deals", deals_list},
        {"count", deals_.size()}
    };
    
    return createSuccessResponse(response);
}

// Vector utility implementations
crow::response ApiServer::createSeasonalVector(const crow::request& req) {
    try {
        json request_json = json::parse(req.body);
        
        double baseValue = request_json.value("baseValue", 1.0);
        std::vector<double> seasonalFactors = request_json.value("seasonalFactors", 
            std::vector<double>{1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0});
        int years = request_json.value("years", 11);
        
        VectorParameter seasonal = createSeasonalPattern(baseValue, seasonalFactors, years);
        
        json response = {
            {"message", "Seasonal vector created successfully"},
            {"vector", seasonal.toJson()}
        };
        
        return createSuccessResponse(response);
        
    } catch (const json::exception& e) {
        return createErrorResponse("Invalid JSON", e.what(), 400);
    } catch (const std::exception& e) {
        return createErrorResponse("Internal error", e.what(), 500);
    }
}

VectorParameter ApiServer::createSeasonalPattern(double baseValue, const std::vector<double>& seasonalFactors, int years) {
    VectorParameter param;
    param.type = "seasonal";
    param.baseValue = baseValue;
    param.description = "Seasonal pattern repeated for " + std::to_string(years) + " years";
    
    param.values.reserve(years * 12);
    for (int year = 0; year < years; ++year) {
        for (size_t month = 0; month < 12 && month < seasonalFactors.size(); ++month) {
            param.values.push_back(seasonalFactors[month]);
        }
    }
    
    // Ensure exactly 132 periods
    param.values.resize(132, 1.0);
    
    return param;
}

// Validation helpers
bool ApiServer::validateDealJson(const json& deal, std::string& error) {
    // Basic validation for now - we'll implement full schema validation later
    if (!deal.contains("name") || !deal["name"].is_string()) {
        error = "Deal must have a 'name' field (string)";
        return false;
    }
    
    if (!deal.contains("type") || !deal["type"].is_string()) {
        error = "Deal must have a 'type' field (string)";
        return false;
    }
    
    // Validate deal type
    std::string dealType = deal["type"];
    if (dealType != "consumer_abs" && dealType != "clo" && dealType != "commercial_mortgage" && 
        dealType != "auto_loan" && dealType != "credit_card" && dealType != "student_loan") {
        error = "Invalid deal type. Must be one of: consumer_abs, clo, commercial_mortgage, auto_loan, credit_card, student_loan";
        return false;
    }
    
    // Validate structure if present
    if (deal.contains("structure")) {
        if (!deal["structure"].is_object()) {
            error = "Deal structure must be an object";
            return false;
        }
        
        const auto& structure = deal["structure"];
        if (structure.contains("tranches") && !structure["tranches"].is_array()) {
            error = "Structure tranches must be an array";
            return false;
        }
    }
    
    // Validate assets if present
    if (deal.contains("assets")) {
        if (!deal["assets"].is_object()) {
            error = "Deal assets must be an object";
            return false;
        }
        
        const auto& assets = deal["assets"];
        if (assets.contains("total_balance") && !assets["total_balance"].is_number()) {
            error = "Assets total_balance must be a number";
            return false;
        }
        if (assets.contains("asset_count") && !assets["asset_count"].is_number()) {
            error = "Assets asset_count must be a number";
            return false;
        }
        if (assets.contains("weighted_average_rate") && !assets["weighted_average_rate"].is_number()) {
            error = "Assets weighted_average_rate must be a number";
            return false;
        }
    }
    
    return true;
}

bool ApiServer::validateVectorParameter(const json& param, std::string& error) {
    if (!param.contains("type") || !param["type"].is_string()) {
        error = "Vector parameter must have a 'type' field";
        return false;
    }
    
    if (param.contains("values") && param["values"].is_array()) {
        auto values = param["values"];
        if (values.size() != 132) {
            error = "Vector must contain exactly 132 values (11 years * 12 months)";
            return false;
        }
        
        for (const auto& value : values) {
            if (!value.is_number()) {
                error = "All vector values must be numbers";
                return false;
            }
        }
    }
    
    return true;
}

// Stub implementations for other handlers
crow::response ApiServer::updateDeal(const std::string& dealId, const crow::request& req) {
    return createErrorResponse("Not implemented yet", 501);
}

crow::response ApiServer::deleteDeal(const std::string& dealId) {
    return createErrorResponse("Not implemented yet", 501);
}

crow::response ApiServer::bulkCreateDeals(const crow::request& req) {
    return createErrorResponse("Not implemented yet", 501);
}

crow::response ApiServer::bulkAddAssets(const std::string& dealId, const crow::request& req) {
    return createErrorResponse("Not implemented yet", 501);
}

crow::response ApiServer::runScenarios(const std::string& dealId, const crow::request& req) {
    return createErrorResponse("Not implemented yet", 501);
}

crow::response ApiServer::runMultiDealScenarios(const crow::request& req) {
    return createErrorResponse("Not implemented yet", 501);
}

crow::response ApiServer::createRampVector(const crow::request& req) {
    return createErrorResponse("Not implemented yet", 501);
}

crow::response ApiServer::applyShockVector(const crow::request& req) {
    return createErrorResponse("Not implemented yet", 501);
}

crow::response ApiServer::interpolateVector(const crow::request& req) {
    return createErrorResponse("Not implemented yet", 501);
}

crow::response ApiServer::validateVector(const crow::request& req) {
    return createErrorResponse("Not implemented yet", 501);
}

// Analytics route stubs
void ApiServer::setupAnalyticsRoutes() {
    // TODO: Implement analytics routes
}

void ApiServer::setupReportingRoutes() {
    // TODO: Implement reporting routes
}

void ApiServer::setupValidationRoutes() {
    // TODO: Implement validation routes
}

void ApiServer::setupMonitoringRoutes() {
    // TODO: Implement monitoring routes
}

void ApiServer::setupWebSocketRoutes() {
    // TODO: Implement WebSocket routes
}

} // namespace api
} // namespace structura