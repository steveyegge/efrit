#!/usr/bin/env bash
# Run Efrit integration tests against production API

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

echo "==================================="
echo "Efrit Integration Tests"
echo "==================================="
echo "WARNING: These tests use the production API and will consume tokens!"
echo "Make sure your API key is configured in ~/.authinfo"
echo ""
read -p "Continue? (y/N) " -n 1 -r
echo ""

if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Tests cancelled."
    exit 1
fi

echo ""
echo "Running integration tests..."
echo ""

# Run the tests
emacs -Q --batch \
    -L "$PROJECT_ROOT/lisp" \
    -L "$SCRIPT_DIR" \
    -l efrit \
    -l efrit-integration-tests \
    -f efrit-run-integration-tests-batch \
    2>&1 | tee integration-test-results.log

# Check if tests passed
if grep -q "FAILED" integration-test-results.log; then
    echo ""
    echo "❌ Some tests failed. See integration-test-results.log for details."
    exit 1
else
    echo ""
    echo "✅ All integration tests passed!"
    exit 0
fi
