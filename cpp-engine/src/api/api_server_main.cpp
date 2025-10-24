#include "api_server.h"
#include <iostream>
#include <csignal>
#include <memory>

using namespace structura::api;

// Global pointer for signal handling
std::unique_ptr<ApiServer> g_server;

// Signal handler for graceful shutdown
void signalHandler(int signal) {
    std::cout << "\nReceived signal " << signal << ". Shutting down gracefully..." << std::endl;
    if (g_server) {
        g_server->stop();
    }
    exit(0);
}

void printWelcomeMessage() {
    std::cout << R"(
┌─────────────────────────────────────────────────────────────┐
│                                                             │
│              🏗️  STRUCTURA JSON DSL API SERVER              │
│                                                             │
│              High-Performance Structured Finance            │
│                    Vector-Based Parameters                  │
│                                                             │
└─────────────────────────────────────────────────────────────┘
)" << std::endl;
}

void printUsage() {
    std::cout << "Usage: structura_api_server [OPTIONS]\n"
              << "\nOptions:\n"
              << "  -p, --port PORT    Server port (default: 8080)\n"
              << "  -t, --threads NUM  Number of threads (default: 4)\n"
              << "  --no-cors          Disable CORS support\n"
              << "  -v, --verbose      Enable verbose logging\n"
              << "  -h, --help         Show this help message\n"
              << "\nExamples:\n"
              << "  structura_api_server                    # Start on port 8080\n"
              << "  structura_api_server -p 9000 -t 8       # Custom port and threads\n"
              << "  structura_api_server --no-cors          # Disable CORS\n"
              << std::endl;
}

int main(int argc, char* argv[]) {
    printWelcomeMessage();
    
    // Default configuration
    int port = 8080;
    size_t threads = 4;
    bool enable_cors = true;
    bool verbose = false;
    
    // Parse command line arguments
    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];
        
        if (arg == "-h" || arg == "--help") {
            printUsage();
            return 0;
        }
        else if (arg == "-p" || arg == "--port") {
            if (i + 1 < argc) {
                port = std::stoi(argv[++i]);
            } else {
                std::cerr << "Error: --port requires a value" << std::endl;
                return 1;
            }
        }
        else if (arg == "-t" || arg == "--threads") {
            if (i + 1 < argc) {
                threads = std::stoul(argv[++i]);
            } else {
                std::cerr << "Error: --threads requires a value" << std::endl;
                return 1;
            }
        }
        else if (arg == "--no-cors") {
            enable_cors = false;
        }
        else if (arg == "-v" || arg == "--verbose") {
            verbose = true;
        }
        else {
            std::cerr << "Unknown option: " << arg << std::endl;
            printUsage();
            return 1;
        }
    }
    
    // Validate configuration
    if (port < 1024 || port > 65535) {
        std::cerr << "Error: Port must be between 1024 and 65535" << std::endl;
        return 1;
    }
    
    if (threads < 1 || threads > 64) {
        std::cerr << "Error: Thread count must be between 1 and 64" << std::endl;
        return 1;
    }
    
    try {
        // Create and configure server
        g_server = std::make_unique<ApiServer>(port);
        g_server->setThreads(threads);
        g_server->enableCORS(enable_cors);
        
        if (verbose) {
            g_server->setLogLevel(crow::LogLevel::Debug);
        } else {
            g_server->setLogLevel(crow::LogLevel::Info);
        }
        
        // Setup signal handlers for graceful shutdown
        std::signal(SIGINT, signalHandler);
        std::signal(SIGTERM, signalHandler);
        
        std::cout << "Configuration:" << std::endl;
        std::cout << "  Port: " << port << std::endl;
        std::cout << "  Threads: " << threads << std::endl;
        std::cout << "  CORS: " << (enable_cors ? "enabled" : "disabled") << std::endl;
        std::cout << "  Verbose: " << (verbose ? "enabled" : "disabled") << std::endl;
        std::cout << std::endl;
        
        // Start the server
        g_server->start();
        
        std::cout << "\n🚀 Server is ready!" << std::endl;
        std::cout << "📚 API Documentation: http://localhost:" << port << "/api/docs" << std::endl;
        std::cout << "❤️  Health Check: http://localhost:" << port << "/api/health" << std::endl;
        std::cout << "📊 System Metrics: http://localhost:" << port << "/api/metrics" << std::endl;
        std::cout << "\nPress Ctrl+C to stop the server" << std::endl;
        
        // Wait for shutdown
        g_server->waitForShutdown();
        
    } catch (const std::exception& e) {
        std::cerr << "Fatal error: " << e.what() << std::endl;
        return 1;
    }
    
    std::cout << "Server shutdown complete. Goodbye! 👋" << std::endl;
    return 0;
}