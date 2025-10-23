#include "fixed_asset.h"
#include <algorithm>
#include <cmath>
#include <stdexcept>
#include <sstream>
#include <numeric>

namespace Structura {

// UtilizationCurve implementation
double UtilizationCurve::getUtilizationRate(Date date) const {
    // Find the applicable utilization rate for the given date
    double rate = base_utilization_rate;
    
    // Check for specific date points
    for (const auto& point : utilization_points) {
        if (point.first <= date) {
            rate = point.second;
        } else {
            break; // Points should be in chronological order
        }
    }
    
    // Apply seasonal adjustment if applicable
    if (is_seasonal && !seasonal_factors.empty()) {
        int month = date.month();
        auto it = seasonal_factors.find(month);
        if (it != seasonal_factors.end()) {
            rate *= it->second;
        }
    }
    
    return std::min(1.0, std::max(0.0, rate)); // Clamp between 0 and 1
}

void UtilizationCurve::addUtilizationPoint(Date date, double rate) {
    utilization_points.emplace_back(date, rate);
    // Keep points sorted by date
    std::sort(utilization_points.begin(), utilization_points.end());
}

void UtilizationCurve::setSeasonalPattern(const std::map<int, double>& factors) {
    seasonal_factors = factors;
    is_seasonal = !factors.empty();
}

// PriceCurve implementation
Amount PriceCurve::getPricePerUnit(Date date) const {
    Amount price = base_price;
    
    // Find the applicable price for the given date
    for (const auto& point : price_points) {
        if (point.first <= date) {
            price = point.second;
        } else {
            break;
        }
    }
    
    // Apply inflation adjustment if applicable
    if (is_inflation_adjusted && inflation_rate > 0.0) {
        // Calculate years since base date (simplified)
        int years_elapsed = 1; // Would calculate actual years in practice
        price *= std::pow(1.0 + inflation_rate, years_elapsed);
    }
    
    return price;
}

void PriceCurve::addPricePoint(Date date, Amount price) {
    price_points.emplace_back(date, price);
    std::sort(price_points.begin(), price_points.end());
}

void PriceCurve::setInflationAdjustment(Rate annual_inflation) {
    inflation_rate = annual_inflation;
    is_inflation_adjusted = (annual_inflation > 0.0);
}

// FixedAsset Constructor
FixedAsset::FixedAsset(const std::string& id, FixedAssetType type, RevenueModel revenue_model,
                      Amount original_cost, const CapacitySpec& capacity, Date acquisition_date,
                      int useful_life_years)
    : asset_id_(id), asset_type_(type), revenue_model_(revenue_model), status_(Status::Current),
      original_cost_(original_cost), current_book_value_(original_cost), residual_value_(0.0),
      acquisition_date_(acquisition_date), placed_in_service_date_(acquisition_date),
      useful_life_years_(useful_life_years), capacity_(capacity),
      depreciation_method_(AmortRule::StraightLine), depreciation_rate_(1.0 / useful_life_years),
      accumulated_depreciation_(0.0), cumulative_revenue_(0.0), cumulative_expenses_(0.0),
      cumulative_net_cashflow_(0.0), average_utilization_rate_(0.0),
      revenue_per_unit_capacity_(0.0), capacity_factor_(0.0), is_leased_(false) {
    
    // Calculate expected end of life
    expected_end_of_life_ = Date(acquisition_date.dayOfMonth(),
                                acquisition_date.month(), 
                                acquisition_date.year() + useful_life_years);
    
    // Set default residual value (typically 10-20% of original cost)
    residual_value_ = original_cost * 0.15;
    
    // Initialize utilization curve with reasonable defaults
    utilization_curve_ = UtilizationCurve(0.75); // 75% base utilization
    
    // Initialize price curve based on asset type
    Amount default_price = 100.0; // Default price per unit
    switch (type) {
        case FixedAssetType::SOLAR_PANELS:
            default_price = 50.0; // $/MWh
            price_curve_ = PriceCurve(default_price, "$/MWh");
            break;
        case FixedAssetType::HOTEL_ROOMS:
            default_price = 120.0; // $/night
            price_curve_ = PriceCurve(default_price, "$/night");
            break;
        case FixedAssetType::EV_CHARGING_STATIONS:
            default_price = 0.35; // $/kWh
            price_curve_ = PriceCurve(default_price, "$/kWh");
            break;
        default:
            price_curve_ = PriceCurve(default_price, "$/unit");
            break;
    }
    
    last_cashflow_date_ = acquisition_date;
}

// Core asset interface implementations
void FixedAsset::generateCashflow(Date from_date, Date to_date) {
    if (from_date >= to_date || status_ != Status::Current) {
        return;
    }
    
    // Calculate revenue for the period
    Amount period_revenue = calculatePeriodRevenue(from_date, to_date);
    cumulative_revenue_ += period_revenue;
    
    // Calculate expenses for the period
    Amount period_expenses = calculatePeriodExpenses(from_date, to_date);
    cumulative_expenses_ += period_expenses;
    
    // Calculate net cash flow
    Amount net_cashflow = period_revenue - period_expenses;
    cumulative_net_cashflow_ += net_cashflow;
    
    last_cashflow_date_ = to_date;
}

void FixedAsset::applyDepreciation(Date depreciation_date) {
    if (accumulated_depreciation_ >= (original_cost_ - residual_value_)) {
        return; // Fully depreciated
    }
    
    applyDepreciationMethod(depreciation_date);
    current_book_value_ = original_cost_ - accumulated_depreciation_;
    
    if (current_book_value_ < residual_value_) {
        current_book_value_ = residual_value_;
        accumulated_depreciation_ = original_cost_ - residual_value_;
    }
}

Balance FixedAsset::getCurrentValue() const {
    return current_book_value_;
}

Amount FixedAsset::calculateNetPresentValue(Rate discount_rate, int projection_years) const {
    if (discount_rate <= 0.0 || projection_years <= 0) {
        return current_book_value_;
    }
    
    Amount npv = 0.0;
    Date current_date = last_cashflow_date_;
    
    // Project future cash flows
    for (int year = 1; year <= projection_years; ++year) {
        Date year_end = Date(current_date.dayOfMonth(), current_date.month(), current_date.year() + year);
        
        // Estimate annual revenue
        double annual_utilization = utilization_curve_.getUtilizationRate(year_end);
        Amount annual_capacity_revenue = getEffectiveCapacity(year_end) * annual_utilization * 
                                       price_curve_.getPricePerUnit(year_end) * 365.25;
        
        // Estimate annual expenses
        Amount annual_expenses = operating_expenses_.fixed_monthly_cost * 12 +
                               annual_capacity_revenue * 0.3; // Assume 30% variable costs
        
        Amount annual_net_cashflow = annual_capacity_revenue - annual_expenses;
        
        // Discount to present value
        Amount discounted_cashflow = annual_net_cashflow / std::pow(1.0 + discount_rate, year);
        npv += discounted_cashflow;
    }
    
    // Add terminal value (residual value discounted)
    Amount terminal_value = residual_value_ / std::pow(1.0 + discount_rate, projection_years);
    npv += terminal_value;
    
    return npv;
}

void FixedAsset::resetToOriginal() {
    current_book_value_ = original_cost_;
    accumulated_depreciation_ = 0.0;
    cumulative_revenue_ = 0.0;
    cumulative_expenses_ = 0.0;
    cumulative_net_cashflow_ = 0.0;
    status_ = Status::Current;
    last_cashflow_date_ = acquisition_date_;
}

// Revenue generation methods
Amount FixedAsset::calculatePeriodRevenue(Date period_start, Date period_end) const {
    switch (revenue_model_) {
        case RevenueModel::CAPACITY_UTILIZATION: {
            double utilization_rate = utilization_curve_.getUtilizationRate(period_end);
            Amount capacity = getEffectiveCapacity(period_end);
            Amount price_per_unit = price_curve_.getPricePerUnit(period_end);
            
            // Calculate days in period (simplified)
            int days_in_period = 30; // Would calculate actual days
            return capacity * utilization_rate * price_per_unit * days_in_period;
        }
        case RevenueModel::FIXED_RENTAL: {
            return price_curve_.getPricePerUnit(period_end); // Fixed rental amount
        }
        case RevenueModel::PERFORMANCE_BASED: {
            // Performance-based revenue calculation
            double performance_factor = utilization_curve_.getUtilizationRate(period_end);
            return getEffectiveCapacity(period_end) * performance_factor * 
                   price_curve_.getPricePerUnit(period_end);
        }
        case RevenueModel::HYBRID_MODEL: {
            // Combination of fixed and variable components
            Amount fixed_component = price_curve_.base_price * 0.3; // 30% fixed
            Amount variable_component = calculateCapacityRevenue(period_end, 
                                      utilization_curve_.getUtilizationRate(period_end)) * 0.7; // 70% variable
            return fixed_component + variable_component;
        }
        default:
            return 0.0;
    }
}

Amount FixedAsset::calculateCapacityRevenue(Date date, double utilization_rate) const {
    Amount capacity = getEffectiveCapacity(date);
    Amount price_per_unit = price_curve_.getPricePerUnit(date);
    return capacity * utilization_rate * price_per_unit;
}

double FixedAsset::getCurrentUtilizationRate(Date date) const {
    return utilization_curve_.getUtilizationRate(date);
}

Amount FixedAsset::getCurrentPricePerUnit(Date date) const {
    return price_curve_.getPricePerUnit(date);
}

// Operating expenses methods
Amount FixedAsset::calculatePeriodExpenses(Date period_start, Date period_end) const {
    Amount fixed_expenses = calculateFixedExpenses(period_end);
    
    // Estimate production volume for variable expenses
    double utilization_rate = utilization_curve_.getUtilizationRate(period_end);
    Amount production_volume = getEffectiveCapacity(period_end) * utilization_rate * 30; // 30 days
    Amount variable_expenses = calculateVariableExpenses(period_end, production_volume);
    
    return fixed_expenses + variable_expenses;
}

Amount FixedAsset::calculateFixedExpenses(Date date) const {
    return operating_expenses_.fixed_monthly_cost +
           operating_expenses_.maintenance_reserve_rate;
}

Amount FixedAsset::calculateVariableExpenses(Date date, Amount production_volume) const {
    return production_volume * operating_expenses_.variable_cost_per_unit;
}

// Depreciation methods
void FixedAsset::applyDepreciationMethod(Date valuation_date) {
    // Calculate months elapsed since acquisition
    int months_elapsed = 12; // Simplified - would calculate actual months
    
    Amount depreciation_expense = 0.0;
    
    switch (depreciation_method_) {
        case AmortRule::StraightLine:
            depreciation_expense = calculateStraightLineDepreciation(months_elapsed);
            break;
        case AmortRule::DecliningBalance:
            depreciation_expense = calculateDecliningBalanceDepreciation(months_elapsed);
            break;
        default:
            depreciation_expense = calculateStraightLineDepreciation(months_elapsed);
            break;
    }
    
    accumulated_depreciation_ += depreciation_expense;
}

Amount FixedAsset::calculateStraightLineDepreciation(int months_elapsed) const {
    Amount depreciable_base = original_cost_ - residual_value_;
    Amount monthly_depreciation = depreciable_base / (useful_life_years_ * 12);
    return monthly_depreciation;
}

Amount FixedAsset::calculateDecliningBalanceDepreciation(int months_elapsed) const {
    double annual_rate = depreciation_rate_ * 2.0; // Double declining balance
    double monthly_rate = annual_rate / 12.0;
    return current_book_value_ * monthly_rate;
}

// Capacity management
Amount FixedAsset::getEffectiveCapacity(Date date) const {
    Amount base_capacity = capacity_.base_capacity;
    
    // Apply degradation if applicable
    if (capacity_.type == CapacityType::DEGRADING_CAPACITY && capacity_.degradation_rate > 0.0) {
        int years_elapsed = date.year() - acquisition_date_.year();
        double degradation_factor = std::pow(1.0 - capacity_.degradation_rate, years_elapsed);
        base_capacity *= degradation_factor;
    }
    
    // Check for scheduled capacity changes
    for (const auto& schedule_point : capacity_.capacity_schedule) {
        if (schedule_point.first <= date) {
            base_capacity = schedule_point.second;
        }
    }
    
    return base_capacity;
}

void FixedAsset::updateCapacity(Date effective_date, Amount new_capacity) {
    capacity_.capacity_schedule.emplace_back(effective_date, new_capacity);
    std::sort(capacity_.capacity_schedule.begin(), capacity_.capacity_schedule.end());
}

// Performance analytics
double FixedAsset::calculateReturnOnAssets(Date from_date, Date to_date) const {
    if (current_book_value_ <= 0.0) return 0.0;
    
    Amount net_income = cumulative_net_cashflow_; // Simplified - would use period-specific income
    return net_income / current_book_value_;
}

Amount FixedAsset::calculateFreeCashFlow(Date from_date, Date to_date) const {
    // Free Cash Flow = Operating Cash Flow - Capital Expenditures
    Amount operating_cashflow = cumulative_net_cashflow_;
    Amount capex = 0.0; // Would include any capital improvements
    return operating_cashflow - capex;
}

// Utility function implementations
std::string fixedAssetTypeToString(FixedAssetType type) {
    switch (type) {
        case FixedAssetType::SOLAR_PANELS: return "Solar Panels";
        case FixedAssetType::WIND_TURBINES: return "Wind Turbines";
        case FixedAssetType::HOTEL_ROOMS: return "Hotel Rooms";
        case FixedAssetType::EV_CHARGING_STATIONS: return "EV Charging Stations";
        case FixedAssetType::TELECOMMUNICATIONS: return "Telecommunications";
        case FixedAssetType::MANUFACTURING_EQUIPMENT: return "Manufacturing Equipment";
        case FixedAssetType::REAL_ESTATE_RENTAL: return "Real Estate Rental";
        case FixedAssetType::TRANSPORTATION: return "Transportation";
        case FixedAssetType::MINING_EQUIPMENT: return "Mining Equipment";
        case FixedAssetType::MEDICAL_EQUIPMENT: return "Medical Equipment";
        case FixedAssetType::AGRICULTURAL_EQUIPMENT: return "Agricultural Equipment";
        case FixedAssetType::DATA_CENTER_EQUIPMENT: return "Data Center Equipment";
        case FixedAssetType::OTHER_EQUIPMENT: return "Other Equipment";
        default: return "Unknown";
    }
}

std::string revenueModelToString(RevenueModel model) {
    switch (model) {
        case RevenueModel::CAPACITY_UTILIZATION: return "Capacity Utilization";
        case RevenueModel::FIXED_RENTAL: return "Fixed Rental";
        case RevenueModel::PERFORMANCE_BASED: return "Performance Based";
        case RevenueModel::HYBRID_MODEL: return "Hybrid Model";
        default: return "Unknown";
    }
}

// Industry-specific asset creation helpers
FixedAsset createSolarPanelAsset(const std::string& id, Amount capacity_mw, 
                                Date installation_date, const std::string& location) {
    CapacitySpec capacity(CapacityType::DEGRADING_CAPACITY, capacity_mw, "MW");
    capacity.degradation_rate = 0.005; // 0.5% annual degradation
    
    Amount cost_per_mw = 1200000; // $1.2M per MW
    Amount total_cost = capacity_mw * cost_per_mw;
    
    FixedAsset solar_asset(id, FixedAssetType::SOLAR_PANELS, RevenueModel::CAPACITY_UTILIZATION,
                          total_cost, capacity, installation_date, 25); // 25-year life
    
    solar_asset.setLocation(location);
    
    // Set solar-specific utilization (capacity factor around 25%)
    UtilizationCurve solar_utilization(0.25);
    solar_asset.setUtilizationCurve(solar_utilization);
    
    return solar_asset;
}

FixedAsset createHotelAsset(const std::string& id, int room_count, 
                           Amount cost_per_room, Date opening_date, const std::string& location) {
    CapacitySpec capacity(CapacityType::FIXED_CAPACITY, room_count, "rooms");
    Amount total_cost = room_count * cost_per_room;
    
    FixedAsset hotel_asset(id, FixedAssetType::HOTEL_ROOMS, RevenueModel::CAPACITY_UTILIZATION,
                          total_cost, capacity, opening_date, 30); // 30-year life
    
    hotel_asset.setLocation(location);
    
    // Set hotel-specific utilization (typical occupancy 65-75%)
    UtilizationCurve hotel_utilization(0.70);
    // Add seasonal factors for hospitality
    std::map<int, double> seasonal_factors = {
        {6, 1.2}, {7, 1.3}, {8, 1.3}, // Summer peak
        {12, 1.1}, {1, 0.8}, {2, 0.8}  // Winter patterns
    };
    hotel_utilization.setSeasonalPattern(seasonal_factors);
    hotel_asset.setUtilizationCurve(hotel_utilization);
    
    return hotel_asset;
}

} // namespace Structura