#include <gtest/gtest.h>

// Basic test to verify the build system works
TEST(BasicTest, SimpleAssertion) {
    EXPECT_EQ(1 + 1, 2);
    EXPECT_TRUE(true);
}

TEST(BasicTest, StringTest) {
    std::string hello = "Hello";
    std::string world = "World";
    EXPECT_EQ(hello + " " + world, "Hello World");
}