# Efrit TODO Tracking

## Current Active TODOs

### Session 1: Elisp Syntax Validation (IN PROGRESS)
**Priority**: HIGH | **Assignee**: Current session | **Due**: This session

- [ ] **EFRIT-001**: Design `efrit-do--validate-elisp` function
  - **File**: `efrit-do.el`
  - **Estimate**: 15 minutes
  - **Dependencies**: None
  - **Success**: Function exists and can parse elisp strings

- [ ] **EFRIT-002**: Integrate validation with execution flow  
  - **File**: `efrit-do.el` 
  - **Function**: `efrit-do--execute-tool`
  - **Estimate**: 20 minutes
  - **Dependencies**: EFRIT-001
  - **Success**: Invalid elisp is caught before eval

- [ ] **EFRIT-003**: Add user-friendly error reporting
  - **File**: `efrit-do.el`
  - **Estimate**: 10 minutes  
  - **Dependencies**: EFRIT-002
  - **Success**: Clear syntax error messages shown

- [ ] **EFRIT-004**: Write validation tests
  - **File**: `test-execution-scenarios.el` 
  - **Estimate**: 20 minutes
  - **Dependencies**: EFRIT-001
  - **Success**: Test suite covers syntax validation

- [ ] **EFRIT-005**: Test original "wyvern buffer" bug
  - **File**: Manual testing
  - **Estimate**: 5 minutes
  - **Dependencies**: EFRIT-002
  - **Success**: Original command is caught and handled

- [ ] **EFRIT-006**: Update documentation  
  - **File**: Function docstrings
  - **Estimate**: 10 minutes
  - **Dependencies**: All above
  - **Success**: New functions documented

---

## Planned TODOs (Future Sessions)

### Session 2: Basic Retry Logic
- [ ] **EFRIT-007**: Design retry mechanism
- [ ] **EFRIT-008**: Implement Claude error feedback  
- [ ] **EFRIT-009**: Add retry counter and limits
- [ ] **EFRIT-010**: Test retry with syntax errors

### Session 3: Error Context Enhancement  
- [ ] **EFRIT-011**: Build rich error context
- [ ] **EFRIT-012**: Include Emacs state in error reports
- [ ] **EFRIT-013**: Format context for Claude consumption  
- [ ] **EFRIT-014**: Test context quality with Claude

### Session 4: Runtime Error Recovery
- [ ] **EFRIT-015**: Catch runtime errors (not just syntax)
- [ ] **EFRIT-016**: Handle undefined function errors
- [ ] **EFRIT-017**: Handle arithmetic errors  
- [ ] **EFRIT-018**: Test full error recovery pipeline

---

## Completed TODOs

*(None yet - this is the first session)*

---

## TODO Categories

### 🔧 **IMPLEMENTATION** - Core functionality development
- Code writing, function implementation
- Integration with existing systems
- Performance optimization

### 🧪 **TESTING** - Test development and validation  
- Unit tests, integration tests
- Manual testing procedures
- Test automation

### 📚 **DOCUMENTATION** - Documentation updates
- Function docstrings
- User documentation  
- Architecture documentation

### 🐛 **BUG FIXES** - Addressing specific issues
- Syntax error handling
- Runtime error recovery
- Edge case handling

### 🎯 **VALIDATION** - Verification and quality assurance
- Feature verification
- Performance validation
- User experience testing

---

## TODO Lifecycle

```
PLANNED → IN PROGRESS → REVIEW → TESTING → COMPLETE
   ↓           ↓           ↓         ↓         ↓
Created    Assigned    Code      Tests     Merged
           & Started   Written   Pass      & Docs
```

### Status Definitions
- **PLANNED**: Identified but not started
- **IN PROGRESS**: Currently being worked on  
- **REVIEW**: Code complete, needs review
- **TESTING**: In testing phase
- **COMPLETE**: Done and verified

---

## Session Handoff Protocol

### End of Session Checklist
- [ ] Update TODO status for all items worked on
- [ ] Mark completed items with completion date
- [ ] Create TODOs for next session  
- [ ] Update SESSION_NOTES.md with progress
- [ ] Commit changes with clear messages

### Start of Session Checklist  
- [ ] Review SESSION_NOTES.md for context
- [ ] Check TODO_TRACKING.md for assigned items
- [ ] Confirm current session objectives
- [ ] Set up development environment
- [ ] Run existing tests to ensure clean state

---

## Estimates & Velocity Tracking

### Session 1 Estimates
| Task | Estimated | Actual | Variance |
|------|-----------|--------|----------|
| EFRIT-001 | 15 min | TBD | TBD |
| EFRIT-002 | 20 min | TBD | TBD |  
| EFRIT-003 | 10 min | TBD | TBD |
| EFRIT-004 | 20 min | TBD | TBD |
| EFRIT-005 | 5 min | TBD | TBD |
| EFRIT-006 | 10 min | TBD | TBD |
| **TOTAL** | **80 min** | **TBD** | **TBD** |

*(Update after session completion)*

---

## Dependencies & Blockers

### Current Blockers  
*(None identified)*

### Future Dependencies
- **EFRIT-008** depends on **EFRIT-007** (retry mechanism design)
- **EFRIT-011** depends on **EFRIT-007** (need retry framework first)
- Agent mode features depend on robust efrit-do foundation

---

*This file is updated continuously during development. Check it at the start and end of each session.*
