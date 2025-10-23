#pragma once

#include "assets/asset_base.h"
#include <memory>
#include <optional>
#include <vector>
#include <map>

namespace Structura {

/**
 * Fixed asset capacity types for utilization-based revenue
 */
enum class CapacityType {
    FIXED_CAPACITY,           // Fixed capacity amount
    VARIABLE_CAPACITY,        // Capacity varies by period
    SEASONAL_CAPACITY,        // Seasonal capacity patterns
    DEGRADING_CAPACITY        // Capacity degrades over time
};

/**
 * Revenue generation models for fixed assets
 */
enum class RevenueModel {
    CAPACITY_UTILIZATION,     // Revenue = capacity × utilization rate × price
    FIXED_RENTAL,             // Fixed rental payments
    PERFORMANCE_BASED,        // Based on performance metrics
    HYBRID_MODEL              // Combination of models
};

/**
 * Fixed asset types for different industries
 */
enum class FixedAssetType {
    SOLAR_PANELS,             // Solar power generation
    WIND_TURBINES,            // Wind power generation
    HOTEL_ROOMS,              // Hotel/hospitality
    EV_CHARGING_STATIONS,     // Electric vehicle charging
    TELECOMMUNICATIONS,        // Cell towers, fiber networks
    MANUFACTURING_EQUIPMENT,   // Industrial machinery
    REAL_ESTATE_RENTAL,       // Commercial real estate
    TRANSPORTATION,           // Ships, planes, trucks
    MINING_EQUIPMENT,         // Mining and extraction
    MEDICAL_EQUIPMENT,        // Hospital/medical devices
    AGRICULTURAL_EQUIPMENT,   // Farming machinery
    DATA_CENTER_EQUIPMENT,    // Server farms, storage
    OTHER_EQUIPMENT           // Other types of equipment
};

/**
 * Capacity specification for fixed assets
 */
struct CapacitySpec {
    CapacityType type;
    Amount base_capacity;             // Base capacity amount
    std::string capacity_unit;        // MW, rooms, stations, etc.
    std::vector<std::pair<Date, Amount>> capacity_schedule; // Time-based capacity changes
    Amount degradation_rate;          // Annual degradation rate (for degrading assets)
    
    CapacitySpec(CapacityType type, Amount base_capacity, const std::string& unit)
        : type(type), base_capacity(base_capacity), capacity_unit(unit), 
          degradation_rate(0.0) {}
};

/**
 * Utilization curve for capacity-based revenue
 */
struct UtilizationCurve {
    std::vector<std::pair<Date, double>> utilization_points; // Date -> utilization rate
    double base_utilization_rate;     // Default utilization rate
    bool is_seasonal;                 // Whether utilization follows seasonal patterns
    std::map<int, double> seasonal_factors; // Month -> seasonal factor
    
    UtilizationCurve(double base_rate = 0.8) 
        : base_utilization_rate(base_rate), is_seasonal(false) {}
        
    double getUtilizationRate(Date date) const;
    void addUtilizationPoint(Date date, double rate);
    void setSeasonalPattern(const std::map<int, double>& factors);
};

/**
 * Price curve for revenue calculation
 */
struct PriceCurve {
    std::vector<std::pair<Date, Amount>> price_points; // Date -> price per unit
    Amount base_price;                // Base price per unit of capacity
    std::string price_unit;           // $/MWh, $/night, $/session, etc.
    bool is_inflation_adjusted;       // Whether prices adjust for inflation
    Rate inflation_rate;              // Annual inflation adjustment
    
    // Default constructor
    PriceCurve() : base_price(0.0), price_unit("unit"), 
                   is_inflation_adjusted(false), inflation_rate(0.0) {}
    
    PriceCurve(Amount base_price, const std::string& unit)
        : base_price(base_price), price_unit(unit), 
          is_inflation_adjusted(false), inflation_rate(0.0) {}
          
    Amount getPricePerUnit(Date date) const;
    void addPricePoint(Date date, Amount price);
    void setInflationAdjustment(Rate annual_inflation);
};

/**
 * Operating expense information
 */
struct OperatingExpenses {
    Amount fixed_monthly_cost;        // Fixed monthly operating cost
    Amount variable_cost_per_unit;    // Variable cost per unit of production
    Amount maintenance_reserve_rate;  // Monthly maintenance reserve rate
    std::vector<std::pair<Date, Amount>> scheduled_maintenance; // Scheduled major maintenance
    Amount insurance_cost;            // Annual insurance cost
    Amount property_tax_rate;         // Annual property tax rate
    
    OperatingExpenses() : fixed_monthly_cost(0.0), variable_cost_per_unit(0.0),
                         maintenance_reserve_rate(0.0), insurance_cost(0.0),
                         property_tax_rate(0.0) {}
};

/**
 * Fixed asset implementation for equipment and infrastructure financing
 */
class FixedAsset {
private:
    std::string asset_id_;
    FixedAssetType asset_type_;
    RevenueModel revenue_model_;
    Status status_;
    
    // Asset characteristics
    Amount original_cost_;            // Original acquisition cost
    Amount current_book_value_;       // Current book value
    Amount residual_value_;           // Expected residual value at end
    Date acquisition_date_;
    Date placed_in_service_date_;
    Date expected_end_of_life_;
    int useful_life_years_;
    
    // Capacity and revenue
    CapacitySpec capacity_;
    UtilizationCurve utilization_curve_;
    PriceCurve price_curve_;
    OperatingExpenses operating_expenses_;
    
    // Depreciation and amortization
    AmortRule depreciation_method_;   // StraightLine, DecliningBalance
    Rate depreciation_rate_;          // Annual depreciation rate
    Amount accumulated_depreciation_; // Total depreciation to date
    
    // Revenue and cash flow tracking
    Amount cumulative_revenue_;       // Total revenue generated
    Amount cumulative_expenses_;      // Total operating expenses
    Amount cumulative_net_cashflow_;  // Net cash flow to date
    Date last_cashflow_date_;
    
    // Performance metrics
    double average_utilization_rate_; // Historical average utilization
    Amount revenue_per_unit_capacity_; // Revenue efficiency metric
    double capacity_factor_;          // Actual vs theoretical capacity utilization
    
    // Location and environmental factors
    std::string location_;            // Geographic location
    std::map<std::string, double> environmental_factors_; // Weather, regulations, etc.
    
    // Financing structure
    std::optional<Obligor> asset_owner_; // Asset owner information
    bool is_leased_;                  // Whether asset is leased
    std::string lease_structure_;     // Operating lease, finance lease, etc.

public:
    FixedAsset(const std::string& id, FixedAssetType type, RevenueModel revenue_model,
               Amount original_cost, const CapacitySpec& capacity, Date acquisition_date,
               int useful_life_years);
    
    // Core asset interface
    void generateCashflow(Date from_date, Date to_date);
    void applyDepreciation(Date depreciation_date);
    Balance getCurrentValue() const;
    Amount calculateNetPresentValue(Rate discount_rate, int projection_years) const;
    void resetToOriginal();
    
    // Revenue generation
    Amount calculatePeriodRevenue(Date period_start, Date period_end) const;
    Amount calculateCapacityRevenue(Date date, double utilization_rate) const;
    double getCurrentUtilizationRate(Date date) const;
    Amount getCurrentPricePerUnit(Date date) const;
    
    // Operating expenses
    Amount calculatePeriodExpenses(Date period_start, Date period_end) const;
    Amount calculateFixedExpenses(Date date) const;
    Amount calculateVariableExpenses(Date date, Amount production_volume) const;
    void scheduleMaintenance(Date maintenance_date, Amount maintenance_cost);
    
    // Depreciation and book value
    void applyDepreciationMethod(Date valuation_date);
    Amount calculateDepreciationExpense(Date from_date, Date to_date) const;
    Amount calculateStraightLineDepreciation(int months_elapsed) const;
    Amount calculateDecliningBalanceDepreciation(int months_elapsed) const;
    
    // Capacity management
    void updateCapacity(Date effective_date, Amount new_capacity);
    void addCapacityUpgrade(Date upgrade_date, Amount additional_capacity, Amount upgrade_cost);
    Amount getEffectiveCapacity(Date date) const;
    void applyCapacityDegradation(Date degradation_date);
    
    // Utilization tracking
    void recordActualUtilization(Date date, double actual_utilization);
    void updateUtilizationCurve(const UtilizationCurve& new_curve);
    double calculateCapacityFactor(Date from_date, Date to_date) const;
    
    // Price management
    void updatePriceCurve(const PriceCurve& new_curve);
    void applyInflationAdjustment(Date adjustment_date);
    void addContractedPricing(Date start_date, Date end_date, Amount contract_price);
    
    // Performance analytics
    double calculateReturnOnAssets(Date from_date, Date to_date) const;
    double calculateEBITDA(Date from_date, Date to_date) const;
    Amount calculateFreeCashFlow(Date from_date, Date to_date) const;
    double calculatePaybackPeriod() const;
    
    // Risk assessment
    double calculateVolatilityRisk() const;
    double assessTechnologicalObsolescenceRisk() const;
    double calculateRegulatoryRisk() const;
    std::vector<std::string> identifyRiskFactors() const;
    
    // Environmental and sustainability
    void setEnvironmentalFactors(const std::map<std::string, double>& factors);
    double calculateCarbonFootprint() const;
    Amount calculateESGValue() const; // Environmental, Social, Governance value
    
    // Lease and financing
    void setLeaseStructure(const std::string& lease_type, Amount monthly_payment);
    Amount calculateLeasePayment(Rate interest_rate, int lease_term_months) const;
    bool isOperatingLease() const;
    bool isFinanceLease() const;
    
    // Valuation methods
    Amount calculateMarketValue(Date valuation_date) const;
    Amount calculateReplacementCost(Date valuation_date) const;
    Amount calculateLiquidationValue(Date valuation_date) const;
    Amount calculateInsuranceValue() const;
    
    // Getters
    const std::string& getAssetId() const { return asset_id_; }
    FixedAssetType getAssetType() const { return asset_type_; }
    RevenueModel getRevenueModel() const { return revenue_model_; }
    Status getStatus() const { return status_; }
    
    Amount getOriginalCost() const { return original_cost_; }
    Amount getCurrentBookValue() const { return current_book_value_; }
    Amount getResidualValue() const { return residual_value_; }
    Date getAcquisitionDate() const { return acquisition_date_; }
    Date getPlacedInServiceDate() const { return placed_in_service_date_; }
    Date getExpectedEndOfLife() const { return expected_end_of_life_; }
    int getUsefulLifeYears() const { return useful_life_years_; }
    
    const CapacitySpec& getCapacitySpec() const { return capacity_; }
    const UtilizationCurve& getUtilizationCurve() const { return utilization_curve_; }
    const PriceCurve& getPriceCurve() const { return price_curve_; }
    const OperatingExpenses& getOperatingExpenses() const { return operating_expenses_; }
    
    AmortRule getDepreciationMethod() const { return depreciation_method_; }
    Rate getDepreciationRate() const { return depreciation_rate_; }
    Amount getAccumulatedDepreciation() const { return accumulated_depreciation_; }
    
    Amount getCumulativeRevenue() const { return cumulative_revenue_; }
    Amount getCumulativeExpenses() const { return cumulative_expenses_; }
    Amount getCumulativeNetCashflow() const { return cumulative_net_cashflow_; }
    
    double getAverageUtilizationRate() const { return average_utilization_rate_; }
    Amount getRevenuePerUnitCapacity() const { return revenue_per_unit_capacity_; }
    double getCapacityFactor() const { return capacity_factor_; }
    
    const std::string& getLocation() const { return location_; }
    const std::optional<Obligor>& getAssetOwner() const { return asset_owner_; }
    bool isLeased() const { return is_leased_; }
    const std::string& getLeaseStructure() const { return lease_structure_; }
    
        // Setters
    void setStatus(Status status) { status_ = status; }
    void setBookValue(Amount book_value) { current_book_value_ = book_value; }
    void setResidualValue(Amount residual_value) { residual_value_ = residual_value; }
    void setOperatingExpenses(const OperatingExpenses& expenses) { operating_expenses_ = expenses; }
    void updateCapacityFactor(double new_factor) { capacity_factor_ = new_factor; }
    void setUtilizationCurve(const UtilizationCurve& curve) { utilization_curve_ = curve; }
    void setPriceCurve(const PriceCurve& curve) { price_curve_ = curve; }
    void setLocation(const std::string& location) { location_ = location; }
};

// Utility functions
std::string fixedAssetTypeToString(FixedAssetType type);
std::string revenueModelToString(RevenueModel model);
std::string capacityTypeToString(CapacityType type);

// Industry-specific asset creation helpers
FixedAsset createSolarPanelAsset(const std::string& id, Amount capacity_mw, 
                                Date installation_date, const std::string& location);
FixedAsset createHotelAsset(const std::string& id, int room_count, 
                           Amount cost_per_room, Date opening_date, const std::string& location);
FixedAsset createEVChargingStation(const std::string& id, int station_count,
                                  Amount cost_per_station, Date installation_date);

// Capacity and utilization calculation functions
double calculateSeasonalUtilization(Date date, const std::map<int, double>& seasonal_factors);
Amount calculateInflationAdjustedPrice(Amount base_price, Rate inflation_rate, int years);
double calculateCapacityDegradation(Amount original_capacity, Rate degradation_rate, int years);

} // namespace Structura