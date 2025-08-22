#!/bin/bash

# run-integration-tests.sh - Run all efrit test suites

set -e

echo "=== Efrit Test Suite ==="
echo "Running comprehensive test coverage..."
echo

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

# Test results tracking
TESTS_RUN=0
TESTS_PASSED=0
FAILED_TESTS=()

run_test() {
    local test_file="$1"
    local test_name="$2"
    local optional="$3"
    
    echo -e "${BLUE}Running $test_name...${NC}"
    TESTS_RUN=$((TESTS_RUN + 1))
    
    # Use the correct load pattern with explicit file loading in dependency order
    if emacs --batch --load ../lisp/efrit-debug.el --load ../lisp/efrit-config.el --load ../lisp/efrit.el --load ../lisp/efrit-agent.el --load ../lisp/efrit-tools.el --load ../lisp/efrit-do.el --load ../lisp/efrit-multi-turn.el --load ../lisp/efrit-command.el --load ../lisp/efrit-chat.el --load "$test_file" 2>&1; then
        echo -e "${GREEN}✅ $test_name PASSED${NC}"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        if [ "$optional" = "optional" ]; then
            echo -e "${YELLOW}⚠️ $test_name SKIPPED (optional)${NC}"
            TESTS_RUN=$((TESTS_RUN - 1))  # Don't count skipped tests
        else
            echo -e "${RED}❌ $test_name FAILED${NC}"
            FAILED_TESTS+=("$test_name")
        fi
    fi
    echo
}

# Display test categories
echo "Test Categories:"
echo "  📦 Core Tests - Module loading and basic functionality"
echo "  🔧 Execution Tests - Local elisp execution engine"
echo "  📚 State Tests - History and context management"
echo "  🌐 API Tests - Full pipeline with real Claude API calls"
echo

# Run all test suites
echo "Starting test execution..."
echo

# Core functionality tests
run_test "test-basic-functionality.el" "Core Functionality"
run_test "test-remote-execution.el" "Remote Execution Engine"

# Local execution tests (renamed for clarity)
run_test "test-execution-scenarios.el" "Execution Scenarios"

# History functionality tests
run_test "test-history-functionality.el" "History Management"

# API integration tests (optional - requires API key and credits)
echo -e "${YELLOW}Checking for API integration tests...${NC}"
if [ -z "$EFRIT_SKIP_API_TESTS" ] && [ -n "$ANTHROPIC_API_KEY" ]; then
    echo -e "${YELLOW}⚠️  API tests will consume Anthropic credits!${NC}"
    echo -e "${YELLOW}   Set EFRIT_SKIP_API_TESTS=1 to skip these tests${NC}"
    echo
    run_test "test-api-integration.el" "API Integration"
else
    if [ -z "$ANTHROPIC_API_KEY" ]; then
        echo -e "${YELLOW}⚠️  Skipping API tests (no ANTHROPIC_API_KEY)${NC}"
    else
        echo -e "${YELLOW}⚠️  Skipping API tests (EFRIT_SKIP_API_TESTS is set)${NC}"
    fi
    echo
fi

# Print summary
echo "=== TEST SUMMARY ==="
echo "Tests run: $TESTS_RUN"
echo "Tests passed: $TESTS_PASSED"
echo "Tests failed: $((TESTS_RUN - TESTS_PASSED))"

if [ ${#FAILED_TESTS[@]} -eq 0 ]; then
    echo -e "${GREEN}"
    echo "🎉 ALL TESTS PASSED!"
    echo "   ✅ Core functionality working"
    echo "   ✅ Execution engine working"  
    echo "   ✅ History management working"
    if [ -z "$EFRIT_SKIP_API_TESTS" ] && [ -n "$ANTHROPIC_API_KEY" ]; then
        echo "   ✅ API integration working"
    else
        echo "   ⚠️  API integration not tested (skipped)"
    fi
    echo
    echo "🚀 efrit is ready for production use!"
    echo -e "${NC}"
    exit 0
else
    echo -e "${RED}"
    echo "❌ SOME TESTS FAILED:"
    for test in "${FAILED_TESTS[@]}"; do
        echo "   - $test"
    done
    echo
    echo "Please review the test output above for details."
    echo -e "${NC}"
    exit 1
fi
