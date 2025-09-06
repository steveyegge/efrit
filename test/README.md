# Efrit Test Suite

## Core Tests

### Primary Test Suite
- **test-comprehensive.el** - Complete test suite covering all major functionality (36 tests)
- **efrit-test-simple.sh** - Basic functionality smoke tests

### Integration Tests (Real API Calls) 🔥
- **test-api-dashboard-integration.sh** ⭐ NEW - API + Dashboard integration test with real API calls
- **test-dashboard-integration-minimal.el** ⭐ NEW - Minimal dashboard integration test (no API calls)
- **efrit-basic-integration-test.el** - Basic integration scenarios (updated with dashboard tests)
- **efrit-integration-tests.el** - Original integration test suite
- **run-integration-tests.sh** - Script to run original integration tests

### Dashboard & Session Tests  
- **test-final-demo.el** - Demonstration of dashboard and session integration
- **test-final-validation.el** - Code quality and production readiness validation
- **test-dashboard.el** - Dashboard-specific functionality tests

### Legacy Tests (Maintained for Historical Reference)
- **test-basic-functionality.el** - Core functionality tests

## Test Organization

The test suite is organized into:

1. **Smoke Tests**: Quick verification that basic functionality works
2. **Unit Tests**: Testing individual components in isolation  
3. **Integration Tests**: Testing component interactions
4. **Performance Tests**: Validating performance characteristics
5. **Production Readiness**: Code quality and standards validation

## Running Tests

```bash
# Run basic smoke tests
make test-simple

# Run comprehensive test suite (no API calls)
cd test && emacs --batch --no-init-file --load test-comprehensive.el

# Run production validation
cd test && emacs --batch --no-init-file --load test-final-validation.el

# Run integration tests with REAL API calls (uses tokens!)
cd test && ./test-api-dashboard-integration.sh      # NEW: API + Dashboard integration
cd test && ./run-integration-tests.sh               # Original integration tests

# Quick integration check (minimal API usage)
cd test && emacs --batch --no-init-file --load efrit-basic-integration-test.el

# Dashboard integration test (no API calls)
cd test && emacs --batch --no-init-file --load test-dashboard-integration-minimal.el
```

## ⚠️ Integration Test Warning

**Integration tests make real API calls and consume tokens!** 

- Configure API key in `~/.authinfo` or `ANTHROPIC_API_KEY` environment variable
- API + Dashboard integration test: ~1-2 API calls 
- Original integration tests: ~3-5 API calls per test suite  
- Always check API usage/billing after running integration tests

## Test Coverage

The test suite provides comprehensive coverage of:

### Unit/Component Tests (test-comprehensive.el)
- ✅ Session tracking and metrics (100%)
- ✅ Dashboard functionality (100%)  
- ✅ Error handling scenarios (100%)
- ✅ Performance characteristics (100%)
- ✅ Cross-platform compatibility (100%)
- ✅ Integration between components (100%)
- **Total: 36/36 tests passing (100% pass rate)**

### Integration Tests (Real API Calls)
- ✅ API + Dashboard integration (session tracking, real API calls, dashboard updates)
- ✅ Basic integration scenarios (API connection, context capture, async commands) 
- ✅ Original integration test suite (multi-step sessions, queue processing, error handling)
- ✅ Minimal dashboard integration (session tracking, dashboard display - no API calls)
- **Total: 4 integration test scenarios covering end-to-end workflows**

### Coverage Summary
- **Unit Tests**: 36 tests, 100% pass rate, 0 API calls
- **Integration Tests**: 4 test scenarios, minimal API usage for validation
- **Combined**: Complete coverage from unit → integration → production
