#include "entities/deal.h"
#include <fstream>
#include <sstream>

namespace structura {

std::unique_ptr<Deal> Deal::fromJsonFile(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Cannot open file: " + filename);
    }
    
    nlohmann::json j;
    file >> j;
    return fromJsonString(j.dump());
}

std::unique_ptr<Deal> Deal::fromJsonString(const std::string& json_str) {
    nlohmann::json j = nlohmann::json::parse(json_str);
    
    const auto& deal_json = j["deal"];
    
    std::string deal_id = deal_json["id"];
    std::string deal_name = deal_json["name"];
    
    // Simple date parsing
    auto parse_date = [](const std::string& date_str) -> Date {
        int year = std::stoi(date_str.substr(0, 4));
        int month = std::stoi(date_str.substr(5, 2));
        int day = std::stoi(date_str.substr(8, 2));
        return DateUtils::makeDate(year, month, day);
    };
    
    Date closing = parse_date(deal_json["dates"]["closing"]);
    Date maturity = parse_date(deal_json["dates"]["stated_maturity"]);
    
    auto deal = std::make_unique<Deal>(deal_id, deal_name, closing, maturity);
    
    // Parse accounts
    if (deal_json.contains("accounts")) {
        for (const auto& acc_json : deal_json["accounts"]) {
            std::string id = acc_json["id"];
            std::string name = acc_json.value("name", id);
            Balance balance = acc_json.value("balance", 0.0);
            
            auto account = std::make_shared<Account>(id, name, balance);
            deal->addAccount(account);
        }
    }
    
    // Parse bonds
    if (deal_json.contains("bonds")) {
        for (const auto& bond_json : deal_json["bonds"]) {
            std::string id = bond_json["id"];
            std::string name = bond_json.value("name", id);
            Balance balance = bond_json.value("current_balance", 0.0);
            Balance original = bond_json.value("original_balance", balance);
            Rate rate = bond_json.value("rate", 0.0);
            
            Date maturity_date = maturity; // Default to deal maturity
            if (bond_json.contains("maturity_date")) {
                maturity_date = parse_date(bond_json["maturity_date"]);
            }
            
            auto bond = std::make_shared<Bond>(id, name, original, rate, maturity_date);
            // Set current balance if different from original
            if (balance != original) {
                bond->payPrincipal(original - balance);
            }
            deal->addBond(bond);
        }
    }
    
    // Parse fees
    if (deal_json.contains("fees")) {
        for (const auto& fee_json : deal_json["fees"]) {
            std::string id = fee_json["id"];
            std::string name = fee_json.value("name", id);
            Rate rate = fee_json.value("rate", 0.0);
            
            auto fee = std::make_shared<Fee>(id, name, rate);
            
            // Set due amount if specified
            if (fee_json.contains("due_amount")) {
                Balance due = fee_json["due_amount"];
                fee->calculateFee(due / std::max(rate, 1.0L)); // Rough calculation
            }
            
            deal->addFee(fee);
        }
    }
    
    return deal;
}

nlohmann::json Deal::toJson() const {
    nlohmann::json j;
    j["deal"]["id"] = id_;
    j["deal"]["name"] = name_;
    j["deal"]["dates"]["closing"] = DateUtils::toString(closing_date_);
    j["deal"]["dates"]["stated_maturity"] = DateUtils::toString(maturity_date_);
    
    // TODO: Serialize entities
    
    return j;
}

} // namespace structura