# Efrit Dashboard Design

## Overview
A centralized dashboard interface for monitoring efrit sessions, TODO progress, and system state.

## Core Features

### 📋 TODO Management Panel
- **Active TODOs**: Current items with status indicators (☐⟳☑)
- **Completed TODOs**: Session history of finished items with timestamps
- **Progress Metrics**: Completion rates, time tracking, velocity
- **Priority Distribution**: Visual breakdown of high/medium/low priority items

### 🔗 Session State Panel  
- **Connection Status**: API connectivity, model info, rate limits
- **Current Context**: Active buffers, working directory, recent commands
- **Performance Metrics**: Response times, token usage, retry counts
- **Debug Log**: Unified efrit-log output with levels and subsystems

### 📊 Work Summary Panel
- **Commands Executed**: History with results and timing
- **Buffers Created**: Links to efrit-generated reports and analysis
- **Files Modified**: Track changes made during session
- **Tools Used**: Usage statistics (eval_sexp, shell_exec, todo_*)

### 🪟 Quick Actions
- **Buffer Navigation**: Jump to efrit logs, results, reports
- **State Management**: Clear TODOs, reset context, save session
- **Debug Access**: View unified log (efrit-log-show), raw API calls, tool executions

## Implementation Approach

### Dashboard Buffer
- Dedicated `*efrit-dashboard*` buffer with custom major mode
- Auto-refreshing content (configurable intervals)
- Keybindings for quick navigation and actions
- Org-mode style folding for information density

### Data Collection
- Extend existing context and TODO systems
- Add session metrics tracking to efrit-do
- Integrate with efrit-agent session management
- Persist dashboard state across Emacs sessions

### Buffer Integration
- Clickable links to related buffers and files
- Smart buffer management (auto-close old reports)
- Buffer lifecycle tracking (created, modified, accessed)

## User Experience

### Access Methods
- `efrit-dashboard` command to open/refresh
- Auto-display on complex task completion
- Optional persistent side panel mode
- Integration with existing efrit-do commands

### Visual Design
- Clean, information-dense layout
- Color coding for status and priority levels
- Progress bars for completion tracking
- Minimal refresh flicker

## Technical Requirements

### New Files
- `lisp/efrit-dashboard.el` - Main dashboard implementation
- `lisp/efrit-session-tracker.el` - Session state management
- Dashboard major mode with custom keybindings

### Enhancements to Existing Code
- Add metrics collection to efrit-do
- Extend TODO system with timing data
- Buffer lifecycle hooks for tracking
- API call logging and metrics

## Future Enhancements
- Export session reports (markdown, org)
- Session comparison and analysis
- TODO templates for common task patterns
- Integration with external project management tools

---

*This dashboard will provide efrit users with complete visibility into AI assistant operations, making complex multi-step tasks more manageable and transparent.*
