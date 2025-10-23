#include <iostream>

#ifdef HAVE_QUANTLIB
#include <ql/time/date.hpp>
#include <ql/types.hpp>
using namespace QuantLib;
#endif

int main() {
    std::cout << "QuantLib Integration Test\n";
    std::cout << "========================\n";
    
#ifdef HAVE_QUANTLIB
    try {
        // Test basic QuantLib Date functionality  
        Date today(15, January, 2024);
        std::cout << "✅ QuantLib basic types are working!" << std::endl;
        std::cout << "   Today's date: " << today << std::endl;
        
        // Test interest rate type
        Rate rate = 0.05; // 5%
        std::cout << "   Interest rate: " << rate * 100 << "%" << std::endl;
        
        return 0;
    } catch (const std::exception& e) {
        std::cout << "❌ QuantLib error: " << e.what() << std::endl;
        return 1;
    }
#else
    std::cout << "❌ QuantLib not available - compiled without HAVE_QUANTLIB" << std::endl;
    return 1;
#endif
}