# North Star: ChatGPT-Shell Upgrade via Efrit

## Mission
Use Efrit as an autonomous debugging and configuration companion to upgrade the user's chatgpt-shell package from several versions behind to current, resolving straight.el conflicts and configuration issues.

## Test Scenario
- Package: chatgpt-shell (managed by straight.el)
- Problem: Several versions behind with potential conflicts
- Configuration: ~/.emacs.d and associated startup files
- Goal: Efrit autonomously diagnoses and fixes the issue

## Discovered Capability Gaps

### Critical Bugs (Blocking autonomous operation)
1. **Missing Queue Processor** (`efrit-remote-queue-process` function doesn't exist)
   - Remote queue can monitor but not process external requests
   - Need: Simple function to accept JSON request and return JSON response

2. **Emacs Spawn Timeout** (efrit-streamlined-send hangs when spawned)
   - Direct spawning with chat commands times out after 300s
   - Likely: Missing dependencies or UI blocking in batch mode

### Required Capabilities for Success
- [ ] File-based AI communication (partially working)
- [ ] Autonomous Emacs configuration analysis 
- [ ] Package manager interaction (straight.el)
- [ ] Version comparison and conflict resolution
- [ ] Multi-session persistence for complex debugging

## Development Strategy
1. **Fix blocking bugs** - Enable basic AI-to-Efrit communication
2. **Test iteratively** - Use chatgpt-shell as proving ground
3. **Document failures** - Each failure becomes a capability requirement
4. **Build incrementally** - Add features as needed for the use case

## Success Criteria
Efrit can autonomously:
1. Analyze ~/.emacs.d configuration for chatgpt-shell setup
2. Identify version conflicts and package management issues
3. Propose specific remediation steps
4. Execute the upgrade plan with minimal human intervention
5. Verify the upgrade worked correctly

This becomes our proving ground for building a true autonomous debugging companion.
