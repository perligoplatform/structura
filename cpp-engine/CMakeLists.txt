cmake_minimum_required(VERSION 3.20)
project(StructuraCPP VERSION 1.0.0 LANGUAGES CXX)


# C++ Standard
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Compiler-specific options
if(MSVC)
    add_compile_options(/W4 /permissive-)
    add_compile_definitions(_CRT_SECURE_NO_WARNINGS)
else()
    add_compile_options(-Wall -Wextra -pedantic)
endif()

# Build type
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

# Optimization flags
set(CMAKE_CXX_FLAGS_RELEASE "-O3 -DNDEBUG")
set(CMAKE_CXX_FLAGS_DEBUG "-g -O0")

# Find packages
find_package(Threads REQUIRED)

# QuantLib integration - Updated configuration
set(QuantLib_ROOT /usr)
find_package(QuantLib QUIET)
if(NOT QuantLib_FOUND)
    # Fallback: find library and headers manually
    find_library(QuantLib_LIBRARY NAMES QuantLib libQuantLib PATHS /usr/lib /usr/local/lib)
    find_path(QuantLib_INCLUDE_DIR ql/quantlib.hpp PATHS /usr/include /usr/local/include)
    if(QuantLib_LIBRARY AND QuantLib_INCLUDE_DIR)
        set(QuantLib_FOUND TRUE)
        message(STATUS "Found QuantLib: ${QuantLib_LIBRARY}")
        message(STATUS "QuantLib headers: ${QuantLib_INCLUDE_DIR}")
    else()
        message(WARNING "QuantLib not found - some features will be disabled")
    endif()
endif()

# Third-party dependencies
include(FetchContent)

# nlohmann/json for JSON parsing
FetchContent_Declare(
    nlohmann_json
    GIT_REPOSITORY https://github.com/nlohmann/json.git
    GIT_TAG v3.11.2
)

FetchContent_MakeAvailable(nlohmann_json)

# Add vcpkg include directories for Crow (header-only)
include_directories(/usr/local/vcpkg/installed/x64-linux/include)

# Include directories
include_directories(src)

# Core library sources (clean implementation)
list(APPEND CORE_SOURCES 
    "src/core/types.cpp"
    "src/core/date_utils.cpp"
    "src/core/date_utils_enhanced.cpp"
    "src/core/enhanced_utils.cpp"
    "src/core/deal_base.cpp"
    "src/core/cashflow.cpp"
    "src/core/interest_rate.cpp"
    "src/entities/deal.cpp"
    "src/entities/fee.cpp"
    "src/entities/bond.cpp"
    "src/assets/asset_base.cpp"
    "src/assets/mortgage.cpp"
    "src/assets/auto_loan.cpp"
    "src/assets/credit_card.cpp"
    "src/assets/pool.cpp"
    "src/assets/student_loan.cpp"
    "src/assets/corporate_loan.cpp"
    "src/assets/consumer_installment.cpp"
    "src/assets/fixed_asset.cpp"
    "src/assets/receivable.cpp"
    "src/assets/projected_cash_flow.cpp"
    "src/deals/clo_deal.cpp"
    "src/deals/consumer_abs_deal.cpp"
    "src/analytics/analytics_engine.cpp"
    "src/reporting/reporting_engine.cpp"
    "src/validation/validation_engine.cpp"
    "src/monitoring/monitoring_engine.cpp"
    "src/api/api_server.cpp"
    ""
)

file(GLOB_RECURSE CORE_HEADERS
    "src/core/*.h"
    "src/entities/*.h"
    "src/assets/*.h"
    "src/deals/*.h"
    "src/calculations/*.h" 
    "src/rules/*.h"
    "src/analytics/*.h"
    "src/reporting/*.h"
    "src/validation/*.h"
    "src/monitoring/*.h"
)

add_library(structura_core STATIC ${CORE_SOURCES} ${CORE_HEADERS})

target_link_libraries(structura_core 
    nlohmann_json::nlohmann_json
    Threads::Threads
)

# Add QuantLib if found
if(QuantLib_FOUND)
    if(TARGET QuantLib::QuantLib)
        target_link_libraries(structura_core QuantLib::QuantLib)
    else()
        target_link_libraries(structura_core ${QuantLib_LIBRARY})
        target_include_directories(structura_core PRIVATE ${QuantLib_INCLUDE_DIR})
    endif()
    target_compile_definitions(structura_core PRIVATE HAVE_QUANTLIB=1)
endif()

target_include_directories(structura_core PUBLIC src)

# Demo executable
add_executable(structura_demo examples/simple_demo.cpp)
target_link_libraries(structura_demo structura_core)

# Simple workflow demo executable (working version)
add_executable(simple_workflow_demo examples/simple_workflow_demo.cpp)
target_link_libraries(simple_workflow_demo structura_core)

# Comprehensive workflow demo executable (advanced version)
add_executable(comprehensive_workflow_demo examples/comprehensive_workflow_demo.cpp)
target_link_libraries(comprehensive_workflow_demo structura_core)

# HTTP API Server executable
add_executable(structura_api_server 
    src/api/api_server_main.cpp
    src/api/api_server.cpp
)
target_link_libraries(structura_api_server 
    structura_core 
    pthread
)

# QuantLib test executable
add_executable(quantlib_test examples/quantlib_test.cpp)
if(QuantLib_FOUND)
    if(TARGET QuantLib::QuantLib)
        target_link_libraries(quantlib_test QuantLib::QuantLib)
    else()
        target_link_libraries(quantlib_test ${QuantLib_LIBRARY})
        target_include_directories(quantlib_test PRIVATE ${QuantLib_INCLUDE_DIR})
    endif()
    target_compile_definitions(quantlib_test PRIVATE HAVE_QUANTLIB=1)
endif()

# Tests with Google Test (optional)
option(BUILD_TESTS "Build tests" OFF)
if(BUILD_TESTS)
    include(FetchContent)
    FetchContent_Declare(
        googletest
        GIT_REPOSITORY https://github.com/google/googletest.git
        GIT_TAG v1.14.0
    )
    FetchContent_MakeAvailable(googletest)

    enable_testing()

    add_executable(structura_tests
        tests/basic_test.cpp
        tests/bond_test.cpp 
        tests/date_test.cpp
        tests/financial_types_test.cpp
        tests/account_test.cpp
        tests/asset_base_test.cpp
        tests/mortgage_test.cpp
        tests/auto_loan_test.cpp
        tests/pool_test.cpp
        tests/new_asset_test.cpp
        tests/cashflow_test.cpp
        tests/interest_rate_test.cpp
        tests/date_utils_enhanced_test.cpp
        tests/enhanced_utils_test.cpp
        tests/deal_structure_test.cpp
        tests/analytics_engine_test.cpp
        tests/reporting_engine_test.cpp
        tests/validation_engine_test.cpp
        tests/monitoring_engine_test.cpp
    )

    target_link_libraries(structura_tests PRIVATE
        structura_core
        gtest_main
    )

    include(GoogleTest)
    gtest_discover_tests(structura_tests)
endif()

# Installation
install(TARGETS structura_demo DESTINATION bin)
install(TARGETS structura_core DESTINATION lib)
install(DIRECTORY src/ DESTINATION include FILES_MATCHING PATTERN "*.h")
