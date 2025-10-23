#include <gtest/gtest.h>
#include "core/types.h"

using namespace Structura;

TEST(DateTest, BasicConstruction) {
    Date date = DateUtils::makeDate(2024, 3, 15);
    
    EXPECT_EQ(DateUtils::year(date), 2024);
    EXPECT_EQ(DateUtils::month(date), 3);
    EXPECT_EQ(DateUtils::dayOfMonth(date), 15);
}

TEST(DateTest, Comparison) {
    Date date1 = DateUtils::makeDate(2024, 3, 15);
    Date date2 = DateUtils::makeDate(2024, 3, 16);
    Date date3 = DateUtils::makeDate(2024, 3, 15);
    
    EXPECT_TRUE(date1 < date2);
    EXPECT_FALSE(date2 < date1);
    EXPECT_TRUE(date1 == date3);
    EXPECT_FALSE(date1 == date2);
}

TEST(DateTest, StringConversion) {
    Date date = DateUtils::makeDate(2024, 3, 15);
    EXPECT_EQ(DateUtils::toString(date), "2024-03-15");
    
    Date date2 = DateUtils::makeDate(2024, 12, 1);
    EXPECT_EQ(DateUtils::toString(date2), "2024-12-01");
}

TEST(DateTest, Arithmetic) {
    Date date = DateUtils::makeDate(2024, 1, 15);
    Date later = date + 30;  // QuantLib supports + operator directly
    
    // QuantLib date arithmetic is accurate
    EXPECT_GT(DateUtils::month(later), DateUtils::month(date));
    
    int diff = later - date;  // QuantLib supports - operator directly
    EXPECT_EQ(diff, 30);
}
