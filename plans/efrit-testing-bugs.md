# Efrit Testing - Discovered Issues

## Session Log
**Date:** 2025-01-19
**Tester:** Sourcegraph Amp Agent
**Efrit Version:** Current development state

---

## Critical Issues
_System crashes, data loss, complete feature failures_

### CRIT-001: efrit-do.el syntax error prevents loading (RESOLVED)
- **Session:** Session 1
- **Description:** Invalid read syntax error at line 572, character 53 in efrit-do.el
- **Steps to Reproduce:** 1. Load efrit-do.el 2. Emacs reports syntax error
- **Expected Behavior:** File should load without errors
- **Actual Behavior:** "Invalid read syntax: ')', 572, 53" error then "End of file during parsing"
- **Impact:** High - Prevents using efrit-do functionality entirely
- **Resolution:** Refactored efrit-do--execute-tool from 125+ line monolithic function into 10 focused helper functions plus clean 20-line dispatch function. All parentheses balanced correctly.
- **Notes:** Root cause was complex nested cond/condition-case structures making paren matching error-prone. Refactored architecture prevents this class of bug in future.

### FUNC-001: Buffer creation tool not being used by LLM (PARTIALLY RESOLVED)
- **Session:** Session 2
- **Description:** When requesting efrit to create formatted buffers, it executes commands but doesn't use the buffer_create tool
- **Steps to Reproduce:** 1. Request "Create a formatted list... Use buffer_create tool..." 2. Check buffer list
- **Expected Behavior:** Should create new named buffer with formatted content
- **Actual Behavior:** Only outputs to *efrit-do* buffer, no new buffer created
- **Impact:** Medium - Core functionality works but advanced formatting/display features not working
- **Resolution:** Works correctly via remote queue system - buffer_create tool functional
- **Notes:** Issue is specific to direct efrit-do command mode, not tool functionality itself

### FUNC-002: Git report formatting incomplete  
- **Session:** Session 2
- **Description:** Git commit reports show raw data but lack requested markdown formatting and structured layout
- **Steps to Reproduce:** 1. Request tailored git commit report with markdown formatting 2. Check output
- **Expected Behavior:** Well-formatted markdown report in dedicated buffer
- **Actual Behavior:** Raw git command output in *efrit-do* buffer
- **Impact:** Medium - Information is gathered but presentation quality is poor
- **Notes:** May be related to FUNC-001 - LLM not using formatting tools effectively

### CRIT-002: API multibyte text error in HTTP requests
- **Session:** Session 2  
- **Description:** efrit-do commands fail with "Multibyte text in HTTP request" API error
- **Steps to Reproduce:** 1. Run any efrit-do command after initial ones 2. API error occurs
- **Expected Behavior:** Commands should execute normally
- **Actual Behavior:** API error prevents execution
- **Impact:** High - Blocks further efrit-do testing after initial commands work
- **Notes:** Content-length shows very large payloads (36K-400K chars), suggests context accumulation issue

### Issue Template:
- **Issue ID:** FUNC-003
- **Session:** Session X
- **Description:** Brief description
- **Steps to Reproduce:** 1. Step one 2. Step two
- **Expected Behavior:** What should happen
- **Actual Behavior:** What actually happens
- **Impact:** High/Medium/Low
- **Notes:** Additional context

---

## Functional Issues  
_Incorrect behavior, missing error handling, poor UX_

---

## Performance Issues
_Slow responses, resource usage, timeouts_

---

## Usability Issues
_Confusing behavior, poor formatting, inconsistencies_

---

## Observations & Patterns
_Overall patterns noticed during testing_

---

## Test Session Results Summary

### Session 1: Basic Functionality
- **Status:** Completed (with critical bug fix)
- **Issues Found:** CRIT-001 (resolved)
- **Overall Assessment:** ✅ SUCCESS after refactoring

### Session 2: Report Generation  
- **Status:** Completed
- **Issues Found:** FUNC-001 (partially resolved), FUNC-002, CRIT-002
- **Overall Assessment:** 🟡 MIXED - Remote queue works well, direct efrit-do has issues
- **Key Success:** Buffer creation tools work via remote queue
- **Key Issue:** Direct efrit-do has API/context problems

### Session 3: Code Creation
- **Status:** In progress
- **Issues Found:** TBD
- **Overall Assessment:** TBD  

### Session 4: Edge Cases
- **Status:** Not started
- **Issues Found:** TBD
- **Overall Assessment:** TBD
