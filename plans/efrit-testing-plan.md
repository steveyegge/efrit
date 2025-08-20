# Efrit Comprehensive Testing Plan

## Overview
Systematic testing of efrit's capabilities using AI-to-efrit communication channel.
Goal: Identify weak areas and bugs without fixing them yet.

## Testing Sessions (5-minute timeout per session)

### Session 1: Basic Functionality & TODO System
**Objectives:**
- Test basic efrit-do command execution
- Verify TODO list creation, updates, and persistence
- Test simple shell commands and elisp evaluation
- Check context gathering and system prompt functionality

**Test Cases:**
1. Simple elisp evaluation: `(+ 2 3)`
2. Basic shell command: `ls -la`  
3. TODO creation and management commands
4. Buffer creation for simple output
5. Context information gathering

### Session 2: Report Generation & Buffer Management
**Objectives:**
- Test sophisticated report generation (git commits)
- Verify buffer creation and formatting tools
- Check markdown formatting capabilities
- Test file list formatting

**Test Cases:**
1. Request: "Generate a tailored report on recent commits in this repository"
2. Request: "Create a formatted list of all .el files in the lisp/ directory"
3. Test buffer management - multiple buffers, naming conventions
4. Check markdown rendering quality
5. Verify proper use of new explicit tools (buffer_create, format_file_list)

### Session 3: Code Creation & Testing
**Objectives:**
- Test efrit's ability to write functional code
- Verify minor mode creation in *scratch* buffer
- Check code testing capabilities
- Evaluate efrit's debugging skills

**Test Cases:**
1. Request: "Write and test a minor mode for viewing efrit log output"
2. Should create mode definition, keybindings, and test functions
3. Verify log file parsing and display functionality
4. Check if efrit can debug its own code issues
5. Test cleanup after testing session

### Session 4: Edge Cases & Robustness
**Objectives:**
- Test error handling and recovery
- Check persistence across sessions
- Test complex multi-step workflows
- Identify brittleness in the system

**Test Cases:**
1. Invalid elisp code - error handling
2. Non-existent files/directories
3. Complex multi-buffer workflows
4. State persistence after efrit restart
5. Large output handling
6. Concurrent operation edge cases

## Bug Tracking Categories

### Critical Issues
- System crashes or hangs
- Data loss or corruption
- Complete feature failures

### Functional Issues  
- Incorrect behavior vs expected
- Missing error handling
- Poor user experience

### Performance Issues
- Slow response times
- Memory/resource usage
- Timeout problems

### Usability Issues
- Confusing behavior
- Poor formatting/display
- Inconsistent interfaces

## Success Criteria

**Basic Functionality:**
- ✅ Commands execute successfully
- ✅ TODO system works as designed
- ✅ Context gathering provides useful information

**Advanced Features:**
- ✅ Can generate well-formatted reports
- ✅ Creates appropriate buffers proactively  
- ✅ Writes functional code on request

**Robustness:**
- ✅ Handles errors gracefully
- ✅ Maintains state across sessions
- ✅ Recovers from failures

## Post-Testing Analysis
After all sessions, analyze patterns in discovered issues:
1. Most common failure modes
2. Areas needing architectural improvements
3. Priority ranking for fixes
4. Potential design changes needed
