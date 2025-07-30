# Efrit Development Roadmap

## Vision: Trimodal Problem-Solving Architecture

Transform Efrit from a functional but fragile AI assistant into a robust, CLI-agent-class problem solver while preserving its current strengths.

### Current State (v0.2.1)
- ✅ Working chat interface (`efrit-chat`)  
- ✅ Working one-off commands (`efrit-do`)
- ✅ Basic multi-turn conversations
- ✅ Comprehensive test suite
- ✅ **NEW**: Elisp syntax validation in `efrit-do`
- ✅ **NEW**: Intelligent retry logic with Claude error feedback
- ✅ **NEW**: Configurable retry limits and error handling

### Target State: Three Complementary Modes

```
┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│ CHAT MODE   │    │ ONE-OFF     │    │ AGENT MODE  │
│ efrit-chat  │    │ efrit-do    │    │ efrit-agent │
├─────────────┤    ├─────────────┤    ├─────────────┤
│ Preserved   │    │ Enhanced    │    │ New Build   │
│ Current     │    │ Error       │    │ Aggressive  │
│ Behavior    │    │ Recovery    │    │ Takeover    │
│             │    │ One Session │    │ Until Done  │
│ Q&A Focus   │    │ Quick Tasks │    │ Complex     │
│ No Agendas  │    │ Smart Retry │    │ Problems    │
└─────────────┘    └─────────────┘    └─────────────┘
```

## Development Philosophy

### Incremental & Testable
- **One tiny improvement per session**
- **Preserve existing functionality** 
- **Test each change thoroughly**
- **Can be interrupted and resumed**

### Modal Architecture  
- **Chat**: Conversational, non-aggressive
- **Do**: Enhanced fire-and-forget with smart recovery
- **Agent**: Persistent problem-solving until complete

### Infrastructure Sharing
- Common context gathering
- Common tool protocol  
- Common error handling foundations
- Shared TODO/org-mode integration

## Work Breakdown: Incremental Sessions

### Phase 1: Foundation (Sessions 1-4)
**Goal**: Make efrit-do robust and reliable

#### Session 1: Elisp Syntax Validation ✅ **COMPLETE**
- **File**: `efrit-do.el`
- **Function**: `efrit-do--validate-elisp`
- **Goal**: Detect syntax errors before execution
- **Test**: Original "wyvern buffer" command should be caught
- **Lines**: ~20 lines of code
- **Success**: Invalid elisp detected, not executed

#### Session 2: Basic Retry Logic ✅ **COMPLETE**  
- **File**: `efrit-do.el`
- **Functions**: `efrit-do--extract-error-info`, `efrit-do--extract-executed-code`, enhanced `efrit-do`
- **Goal**: Send errors back to Claude for fixes
- **Test**: Broken elisp gets fixed and retried (configurable retry count)
- **Lines**: ~80 lines of code
- **Success**: Full retry cycle working with error feedback

#### Session 3: Error Context Enhancement ⭐ **NEXT**
- **File**: `efrit-do.el` 
- **Function**: `efrit-do--build-error-context`
- **Goal**: Rich error information for Claude (buffer context, recent commands, etc.)
- **Test**: Claude gets full context for better fixes
- **Lines**: ~30 lines of code
- **Success**: Better fix quality from Claude

#### Session 4: Runtime Error Recovery
- **File**: `efrit-do.el`
- **Function**: `efrit-do--handle-runtime-errors`
- **Goal**: Catch and recover from runtime errors too
- **Test**: Division by zero, undefined functions recovered
- **Lines**: ~25 lines of code
- **Success**: Both syntax and runtime errors handled

### Phase 2: Enhanced One-Off Mode (Sessions 5-8)
**Goal**: Smart efrit-do that can handle complex commands

#### Session 5: Multi-Step Detection
- **Goal**: Detect when commands need multiple steps
- **Feature**: Optional TODO creation for complex tasks
- **Test**: "Debug my buffer list problem" creates plan

#### Session 6: TODO Integration
- **Goal**: org-mode TODO lists for complex efrit-do tasks
- **Feature**: Visual progress tracking
- **Test**: Can see TODO progress in org buffer

#### Session 7: Solution Verification  
- **Goal**: Test that solutions actually work
- **Feature**: Verification step after fixes
- **Test**: Buffer list fix is verified to work

#### Session 8: Performance Optimization
- **Goal**: Keep responsive feel despite retry logic
- **Feature**: Fast error detection, async retries
- **Test**: <2 second response for valid commands

### Phase 3: Agent Mode (Sessions 9-16)
**Goal**: New persistent problem-solving mode

#### Session 9: Agent Mode Foundation
- **File**: `efrit-agent.el` (enhance existing)
- **Goal**: Persistent conversation until problem solved
- **Test**: Takes over Emacs until task complete

#### Session 10: Advanced TODO Management
- **Goal**: Sophisticated org-mode integration
- **Feature**: Task decomposition, progress tracking
- **Test**: Complex problems broken into visible steps

#### Session 11: Tool Composition
- **Goal**: Chain multiple operations
- **Feature**: Multi-step solutions
- **Test**: Can debug multi-layered problems

#### Session 12: Solution Verification
- **Goal**: Test that solutions actually work
- **Feature**: Automated testing of fixes
- **Test**: Doesn't claim success until verified

#### Sessions 13-16: Advanced Features
- Learning from patterns
- Performance optimization
- User interaction enhancement
- Integration polish

### Phase 4: Production Readiness (Sessions 17+)
- Documentation completion
- Performance tuning
- Edge case handling
- User experience polish

## Session Success Criteria

Each session must deliver:
1. **Working code** that passes tests
2. **Preserved functionality** - nothing breaks
3. **Visible improvement** - user can see the difference
4. **Documentation** - what was built and why
5. **Test coverage** - new functionality is tested

## Risk Mitigation

### Preserving Current Functionality
- Extensive test suite already exists
- All changes additive, not modifications
- Feature flags for new behavior
- Rollback capability

### Managing Scope Creep
- Strict session boundaries
- One deliverable per session
- Test-driven development
- No "while we're at it" features

### Handling Interruptions
- Detailed session notes
- Clear continuation points
- Independent session objectives
- Progress tracking in git

## Current Session: Session 1

**Objective**: Implement elisp syntax validation for efrit-do
**File**: `/Users/stevey/src/efrit/efrit-do.el`
**Function**: `efrit-do--validate-elisp`
**Success Criteria**: Invalid elisp is caught before execution
**Test Case**: The "wyvern buffer" command with syntax error
**Estimated Time**: 30-60 minutes
**Next Steps**: See SESSION_NOTES.md for detailed implementation

---
*This roadmap is a living document. Update after each session with progress and learnings.*
