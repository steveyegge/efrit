# Zero Client-Side Intelligence Refactor Plan

## 🚨 Violations Found in Recent Code

We've inadvertently added client-side intelligence that violates efrit's core "pure executor" principle. These need refactoring:

### 1. Report Auto-Detection (efrit-do.el:540)
```elisp
;; VIOLATION: Pattern matching and decision-making
(let ((is-report (string-match-p "\\breport\\b\\|\\blist\\b\\|\\bshow\\b\\|\\buntracked\\b" command)))
```
**Fix**: Remove pattern matching. Let Claude explicitly call buffer creation tools.

### 2. Content Analysis for Buffer Creation (efrit-do.el:505-515) 
```elisp
;; VIOLATION: Content analysis and formatting decisions
(if (string-match-p "^[a-zA-Z0-9_-]+/" content)
    ;; Looks like file paths - format as list
```
**Fix**: Remove content analysis. Provide separate formatting tools Claude can choose.

### 3. TODO Sorting Logic (efrit-do.el:220-235)
```elisp
;; VIOLATION: Complex sorting and presentation logic
(let ((sorted-todos (sort (copy-sequence efrit-do--current-todos) ...)))
```
**Fix**: Make TODO tools pure data operations. Let Claude format via prompts.

## ✅ Refactor Approach

### Replace Intelligence with Tools
```elisp
;; BEFORE: Smart auto-detection
(efrit-do--display-result command result error-p)

;; AFTER: Explicit tool calls Claude makes
(efrit-tools-create-buffer "Report: Untracked Files" content 'markdown-mode)
(efrit-tools-format-file-list files)
```

### New Pure Tools Needed
- `buffer_create` - Create named buffer with content and mode
- `format_file_list` - Format strings as file list
- `format_todo_list` - Format TODOs with specified sorting
- `display_in_buffer` - Simple buffer display control

### System Prompt Updates
Guide Claude to use explicit buffer creation:
```
- For reports and lists, use buffer_create tool with descriptive names
- Use format_file_list for path-like content
- Use format_todo_list with explicit sorting preferences
```

## 🎯 Success Criteria
- ✅ No pattern matching or content analysis in efrit code
- ✅ Claude achieves same UX via explicit tool usage  
- ✅ All formatting decisions made by Claude via tool combinations
- ✅ Efrit returns to pure "dumb executor" model

## Files to Refactor
- `lisp/efrit-do.el` - Remove smart display logic
- `lisp/efrit-tools.el` - Add new pure tools
- System prompts - Guide Claude to use explicit tools

---
*Priority: HIGH - Core architectural principle violation*
