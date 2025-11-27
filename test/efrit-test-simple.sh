#!/bin/bash

# efrit-test-simple.sh - Simple integration test runner for efrit
# Tests basic load and compilation

echo "🚀 Starting efrit integration tests..."

# Check if emacs is available
if ! command -v emacs &> /dev/null; then
    echo "❌ Emacs not found. Please install Emacs to run tests."
    exit 1
fi

# Set up test environment
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
PROJECT_DIR="$SCRIPT_DIR/.."
cd "$SCRIPT_DIR"

echo "📁 Test directory: $SCRIPT_DIR"
echo "📁 Project directory: $PROJECT_DIR"

# Load path for new directory structure
LOAD_PATH="-L $PROJECT_DIR/lisp -L $PROJECT_DIR/lisp/core -L $PROJECT_DIR/lisp/interfaces -L $PROJECT_DIR/lisp/tools -L $PROJECT_DIR/lisp/support"

# Run syntax validation
echo "🔍 Checking efrit-chat.el syntax..."
if emacs --batch --eval "(check-parens)" "$PROJECT_DIR/lisp/core/efrit-chat.el" 2>/dev/null; then
    echo "✅ Syntax check passed"
else
    echo "❌ Syntax check failed"
    exit 1
fi

# Run byte-compilation test
echo "🔨 Testing byte-compilation..."
COMPILE_OUTPUT=$(cd "$PROJECT_DIR" && make compile 2>&1)
if echo "$COMPILE_OUTPUT" | grep -q "Error:"; then
    echo "❌ Byte-compilation errors found:"
    echo "$COMPILE_OUTPUT" | grep "Error:"
    exit 1
else
    echo "✅ Byte-compilation passed"
fi

# Basic functionality test
echo "🧪 Testing basic functionality..."

# Test efrit-tools loading
echo "🔧 Testing core tool functionality..."
if emacs --batch $LOAD_PATH --eval "(progn (require 'efrit-tools) (message \"efrit-tools loaded\") (message \"eval_sexp result: %s\" (efrit-tools-eval-sexp \"(+ 2 3)\")))" 2>&1 | grep -q "eval_sexp result:"; then
    echo "✅ Core tools working"
else
    echo "❌ Core tools failed to load"
    exit 1
fi

# Test efrit loading
echo "🔧 Testing efrit main module..."
if emacs --batch $LOAD_PATH --eval "(progn (require 'efrit) (message \"efrit loaded, version: %s\" (if (boundp 'efrit-version) efrit-version \"unknown\")))" 2>&1 | grep -q "efrit loaded"; then
    echo "✅ efrit module loads successfully"
else
    echo "❌ efrit module failed to load"
    exit 1
fi

# Test efrit-do loading and dispatch table
echo "🔧 Testing efrit-do and dispatch table..."
if emacs --batch $LOAD_PATH --eval "(progn (require 'efrit-do) (message \"efrit-do loaded, %d tools in dispatch table\" (length efrit-do--tool-dispatch-table)))" 2>&1 | grep -q "tools in dispatch table"; then
    echo "✅ efrit-do loads with dispatch table"
else
    echo "❌ efrit-do failed to load"
    exit 1
fi

# Summary
echo ""
echo "📊 Test Summary:"
echo "   ✅ Syntax validation passed"
echo "   ✅ Byte compilation successful"
echo "   ✅ Core tools working"
echo "   ✅ Main module loads"
echo "   ✅ efrit-do loads with dispatch table"
echo ""
echo "🎉 Efrit testing completed!"
echo "   All basic functionality tests passed."
echo "   For full API testing, ensure your API key is configured."
echo "   Run 'M-x efrit-chat' to test interactively."
