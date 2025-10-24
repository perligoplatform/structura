#include <gtest/gtest.h>
#include "deals/clo_deal.h"
#include "deals/consumer_abs_deal.h"

using namespace Structura;

class DealStructureTest : public ::testing::Test {
protected:
    void SetUp() override {
        deal_info_ = DealInfo("Test Deal", "TEST", 
                              DateUtils::makeDate(2024, 1, 1), 
                              DateUtils::makeDate(2029, 1, 1));
    }
    
    DealInfo deal_info_{"Default", "DEFAULT", DateUtils::makeDate(2020, 1, 1), DateUtils::makeDate(2025, 1, 1)};
};

TEST_F(DealStructureTest, CLODealBasicConstruction) {
    // Test that CLO deal headers compile and basic construction works
    EXPECT_NO_THROW({
        CLODeal clo_deal(deal_info_);
        EXPECT_EQ(clo_deal.getDealName(), "Test Deal");
        EXPECT_EQ(clo_deal.getDealType(), "TEST");
    });
}

TEST_F(DealStructureTest, ConsumerABSDealBasicConstruction) {
    // Test that Consumer ABS deal headers compile and basic construction works
    EXPECT_NO_THROW({
        ConsumerABSDeal abs_deal(deal_info_);
        EXPECT_EQ(abs_deal.getDealName(), "Test Deal");
        EXPECT_EQ(abs_deal.getDealType(), "TEST");
    });
}

TEST_F(DealStructureTest, CLOTrancheEnumConversions) {
    // Test CLO tranche type conversions
    EXPECT_EQ(cloTrancheTypeToString(CLOTrancheType::AAA), "AAA");
    EXPECT_EQ(cloTrancheTypeToString(CLOTrancheType::BB), "BB");
    EXPECT_EQ(cloTrancheTypeToString(CLOTrancheType::EQUITY), "EQUITY");
    
    EXPECT_EQ(stringToCLOTrancheType("AAA"), CLOTrancheType::AAA);
    EXPECT_EQ(stringToCLOTrancheType("BB"), CLOTrancheType::BB);
    EXPECT_EQ(stringToCLOTrancheType("EQUITY"), CLOTrancheType::EQUITY);
}

TEST_F(DealStructureTest, ConsumerABSTrancheEnumConversions) {
    // Test Consumer ABS tranche type conversions
    EXPECT_EQ(consumerABSTrancheTypeToString(ConsumerABSTrancheType::CLASS_A), "CLASS_A");
    EXPECT_EQ(consumerABSTrancheTypeToString(ConsumerABSTrancheType::CLASS_D), "CLASS_D");
    EXPECT_EQ(consumerABSTrancheTypeToString(ConsumerABSTrancheType::RESIDUAL), "RESIDUAL");
    
    EXPECT_EQ(stringToConsumerABSTrancheType("CLASS_A"), ConsumerABSTrancheType::CLASS_A);
    EXPECT_EQ(stringToConsumerABSTrancheType("CLASS_D"), ConsumerABSTrancheType::CLASS_D);
    EXPECT_EQ(stringToConsumerABSTrancheType("RESIDUAL"), ConsumerABSTrancheType::RESIDUAL);
}

TEST_F(DealStructureTest, CLOTrancheBasicOperations) {
    // Test basic CLO tranche operations
    CLOTranche tranche("AAA-1", CLOTrancheType::AAA, "AAA", 100000000.0, 0.025);
    
    EXPECT_EQ(tranche.tranche_id, "AAA-1");
    EXPECT_EQ(tranche.tranche_type, CLOTrancheType::AAA);
    EXPECT_EQ(tranche.notional_amount, 100000000.0);
    EXPECT_GT(tranche.calculateInterestPayment(), 0.0);
    EXPECT_FALSE(tranche.isFullyPaid());
}

TEST_F(DealStructureTest, ConsumerABSTrancheBasicOperations) {
    // Test basic Consumer ABS tranche operations  
    ConsumerABSTranche tranche("A-1", ConsumerABSTrancheType::CLASS_A, "AAA", 50000000.0, 0.03);
    
    EXPECT_EQ(tranche.tranche_id, "A-1");
    EXPECT_EQ(tranche.tranche_type, ConsumerABSTrancheType::CLASS_A);
    EXPECT_EQ(tranche.notional_amount, 50000000.0);
    EXPECT_GT(tranche.calculateInterestPayment(), 0.0);
    EXPECT_FALSE(tranche.isFullyPaid());
}