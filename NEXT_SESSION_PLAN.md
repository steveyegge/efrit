# Next Session Plan: Session 5

## Ultimate Goal
Have this entire conversation within Emacs Efrit instead of Sourcegraph Amp.

## Current State Assessment

### ✅ What's Working Well
- **efrit-do**: Robust one-off command execution with retry logic
- **efrit-chat**: Basic conversational interface exists  
- **Foundation**: Solid error handling, context gathering, API integration
- **Testing**: Comprehensive test coverage (100% pass rate)

### 🎯 Gap Analysis
To support full conversations like this session, we need:

1. **Enhanced Multi-Turn Conversations**
   - Current efrit-chat has basic multi-turn support
   - Need robust conversation state management
   - Need conversation persistence across Emacs sessions

2. **Tool Integration in Chat Mode**
   - efrit-do has excellent tool execution (eval_sexp, shell commands)
   - efrit-chat needs to inherit these capabilities
   - Need seamless integration of code execution within conversation

3. **Rich Context in Conversations**  
   - efrit-do has rich error context from Session 3
   - efrit-chat needs similar context awareness
   - Need file system, buffer, and project context in chat

## Recommended Next Session: Enhanced Chat Mode

### Session 5 Objective
**"Enhance efrit-chat with efrit-do's tool execution capabilities"**

### Why This Path?
1. **Direct progress toward goal**: This session was about conversation + code execution
2. **Builds on solid foundation**: efrit-do's robust tool execution is proven
3. **Small incremental step**: Integrate existing capabilities, don't rebuild
4. **Clear success criteria**: Can execute elisp and shell commands within chat

### Session 5 Tasks
1. **Integrate eval_sexp in efrit-chat** 
   - Chat mode can execute elisp code blocks
   - Use efrit-do's error handling and retry logic
   
2. **Add conversation context awareness**
   - Apply Session 3's rich context to chat mode
   - Include buffer state, file system info
   
3. **Test conversation + execution**
   - Verify complex multi-turn conversations work
   - Test code execution within conversation flow

### Alternative Path: Shell Error Detection
If you prefer to complete the error handling story first:
- **Session 5**: Extend retry logic to detect shell command errors
- **Session 6**: Enhanced chat mode

## Success Metrics for Session 5
- [ ] efrit-chat can execute elisp code in conversation
- [ ] Conversation state persists across multiple exchanges  
- [ ] Rich context (files, buffers) available in chat mode
- [ ] Can have a mini-version of this conversation in Emacs

## Estimate
**Duration**: 60-90 minutes
**Complexity**: Medium (integration of existing proven components)
**Risk**: Low (building on solid foundation)

---

*This plan prioritizes the ultimate goal while maintaining incremental progress.*
