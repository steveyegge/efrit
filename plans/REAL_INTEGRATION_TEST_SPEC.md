# Real Integration Test Specification

## Mission: Create a CORRECT Token-Burning Integration Test

### Current Problem
- The previous integration test was **FAKE** - it only tested elisp execution via `efrit-protocol-execute-tool`
- **No tokens were burned** - Claude API was never called 
- efrit-do-async gets stuck in TODO loops and never reaches the API
- Session logs show `"api-calls":0` and warnings like "todo_get_instructions called 14 times in a row - possible loop!"

### Required Test Behavior

#### 1. **Automatic Warning Generation**
```bash
# Test must load 3 elisp files missing lexical-binding cookies
# Emacs MUST automatically generate warnings in *Warnings* buffer
# If warnings don't appear automatically, we're testing the wrong thing
```

#### 2. **Real efrit-do-async Call**
```elisp
;; This should burn tokens by calling Claude API
(efrit-do-async "Fix the lexical-binding warnings in the *Warnings* buffer")
```

#### 3. **Claude Discovery Process**
- Claude must discover the warnings in *Warnings* buffer autonomously
- Claude must locate the problematic files  
- Claude must write elisp code to add lexical-binding cookies
- Claude must execute that code to fix the files

#### 4. **Token Burning Verification**
- Session logs must show `"api-calls": > 0`
- Network traffic to Claude API must occur
- Should cost actual money/tokens

#### 5. **End-to-End Validation**
- All 3 files must be modified with proper `;;; -*- lexical-binding: t; -*-` headers
- *Warnings* buffer should clear after fixes
- Leave files modified (don't revert yet) so we can see when it works

### Test Files Structure
```
test/integration/
├── test-real-integration.el        # The actual test
├── broken-file-1.el               # Missing lexical-binding
├── broken-file-2.el               # Missing lexical-binding  
├── broken-file-3.el               # Missing lexical-binding
└── README.md                      # Test documentation
```

### Key Issues to Debug
1. **TODO Loop Problem**: efrit-do-async gets stuck in `todo_get_instructions` loops
2. **API Never Called**: Sessions show 0 API calls despite running for minutes
3. **Security Blocking**: May need temporary security bypass for file modifications

### Success Criteria
- [ ] Emacs automatically shows lexical-binding warnings in *Warnings* buffer
- [ ] efrit-do-async actually calls Claude API (burns tokens)
- [ ] Claude discovers warnings and locates files autonomously
- [ ] All 3 files get fixed with lexical-binding headers
- [ ] Session logs show > 0 API calls
- [ ] Real money spent on tokens

### Failure Modes Seen
- ❌ Fake tests that pre-write the elisp code
- ❌ Tests that call `efrit-protocol-execute-tool` directly (no API)
- ❌ TODO loops preventing API calls
- ❌ Security systems blocking file modifications
- ❌ Tests that don't generate real Emacs warnings

## Next Steps
1. Create the correct test that generates real warnings
2. Debug why efrit-do-async never calls Claude API
3. Fix TODO loop issues preventing API calls
4. Verify real token burning occurs
