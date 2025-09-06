# TODO List Implementation Plan

## Phase 1: Core Infrastructure (Already Partially Complete)

Looking at efrit-do.el, we already have:
- ✓ Basic TODO data structure (`efrit-do-todo-item`)
- ✓ TODO management functions (`efrit-do--add-todo`, `efrit-do--update-todo-status`)
- ✓ TODO tools in the schema (`todo_add`, `todo_update`, `todo_show`)
- ✓ TODO display functions

**Still Needed:**
1. Enhanced TODO analysis capabilities
2. Integration with session completion logic
3. TODO state persistence in work log

## Phase 2: Enhanced TODO Management

### 2.1 Add New Tools
```elisp
;; Add to efrit-do--tools-schema:

(("name" . "todo_analyze")
 ("description" . "Analyze a command and create TODO items for each step needed.")
 ("input_schema" . (("type" . "object")
                   ("properties" . (("command" . (("type" . "string")
                                                  ("description" . "The command to analyze")))
                                   ("context" . (("type" . "string")
                                                ("description" . "Additional context")))))
                   ("required" . ["command"]))))

(("name" . "todo_status")
 ("description" . "Get summary of TODO list: total, pending, in-progress, completed.")
 ("input_schema" . (("type" . "object")
                   ("properties" . ()))))

(("name" . "todo_next")
 ("description" . "Get the next pending TODO item to work on.")
 ("input_schema" . (("type" . "object")
                   ("properties" . ()))))

(("name" . "todo_complete_check")
 ("description" . "Check if all TODOs are completed. Returns true if all done.")
 ("input_schema" . (("type" . "object")
                   ("properties" . ()))))
```

### 2.2 Implement Tool Handlers
```elisp
(defun efrit-do--handle-todo-analyze (tool-input)
  "Analyze command and suggest TODO breakdown."
  ;; For now, return guidance for Claude to create TODOs manually
  (format "\n[Analyze the command and use todo_add to create items for each step]"))

(defun efrit-do--handle-todo-status ()
  "Return TODO list status summary."
  (let ((total (length efrit-do--current-todos))
        (pending (seq-count (lambda (todo) 
                             (eq (efrit-do-todo-item-status todo) 'todo))
                           efrit-do--current-todos))
        (in-progress (seq-count (lambda (todo)
                                 (eq (efrit-do-todo-item-status todo) 'in-progress))
                               efrit-do--current-todos))
        (completed (seq-count (lambda (todo)
                               (eq (efrit-do-todo-item-status todo) 'completed))
                             efrit-do--current-todos)))
    (format "\n[TODO Status: %d total, %d pending, %d in-progress, %d completed]"
            total pending in-progress completed)))

(defun efrit-do--handle-todo-next ()
  "Get next pending TODO."
  (let ((next-todo (seq-find (lambda (todo)
                              (eq (efrit-do-todo-item-status todo) 'todo))
                            efrit-do--current-todos)))
    (if next-todo
        (format "\n[Next TODO: %s (ID: %s)]" 
                (efrit-do-todo-item-content next-todo)
                (efrit-do-todo-item-id next-todo))
      "\n[No pending TODOs]")))

(defun efrit-do--handle-todo-complete-check ()
  "Check if all TODOs are completed."
  (let ((incomplete (seq-find (lambda (todo)
                               (not (eq (efrit-do-todo-item-status todo) 'completed)))
                             efrit-do--current-todos)))
    (if incomplete
        "\n[TODOs incomplete - work remains]"
      "\n[All TODOs completed!]")))
```

## Phase 3: Session Protocol Integration

### 3.1 Update Session Protocol Instructions
Modify `efrit-do--session-protocol-instructions` to include:

```elisp
"TODO-BASED WORKFLOW:\n"
"1. INITIAL COMMAND ANALYSIS:\n"
"   - Use todo_analyze to understand the task\n" 
"   - Create TODO items for each discrete step\n"
"   - Use todo_add to build your task list\n\n"

"2. PROGRESS TRACKING:\n"
"   - Check todo_status at the start of each continuation\n"
"   - Use todo_next to get the next item to work on\n"
"   - Update TODOs: mark current as completed, next as in-progress\n\n"

"3. COMPLETION DETECTION:\n"
"   - Use todo_complete_check before continuing\n"
"   - If all TODOs completed, use session_complete\n"
"   - This prevents infinite loops!\n\n"

"4. TODO BEST PRACTICES:\n"
"   - Break complex tasks into specific, actionable items\n"
"   - One TODO per file/warning/issue when fixing multiple items\n"
"   - Include verification TODOs (e.g., 'Verify all warnings fixed')\n"
```

### 3.2 Include TODO State in Work Log
Modify `efrit-async--update-session` to capture TODO state:

```elisp
;; Add TODO snapshot to work log
(let ((todo-state (when efrit-do--current-todos
                    (mapcar (lambda (todo)
                             (list (efrit-do-todo-item-id todo)
                                   (efrit-do-todo-item-status todo)))
                           efrit-do--current-todos))))
  ;; Include in work log entry
  (push (list result elisp todo-state) 
        (efrit-session-work-log efrit-async--active-session)))
```

## Phase 4: Progress Display Integration

### 4.1 Show TODOs in Progress Buffer
Update `efrit-progress.el` to display TODO status:

```elisp
(defun efrit-progress-show-todos ()
  "Display current TODOs in progress buffer."
  (when efrit-do--current-todos
    (let ((buffer (get-buffer "*Efrit Progress*")))
      (when buffer
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "\n\n=== TODO List ===\n")
          (insert (efrit-do--format-todos-for-display))
          (insert "\n"))))))
```

## Phase 5: Testing Strategy

### 5.1 Test Scenarios
1. **Simple Task**: "Create a new buffer" (no TODOs needed)
2. **Multi-Step Task**: "Fix all warnings" (multiple TODOs)
3. **Complex Task**: "Refactor this function" (hierarchical TODOs)
4. **Interrupted Task**: Start task, cancel, resume with TODO state

### 5.2 Success Criteria
- No more infinite loops in warning fix scenario
- Clear progress visibility in progress buffer
- Sessions complete automatically when TODOs done
- TODO state persists across continuations

## Implementation Order

1. **Day 1**: Implement new TODO tools and handlers
2. **Day 2**: Update session protocol instructions
3. **Day 3**: Integrate TODO state with work log
4. **Day 4**: Update progress display
5. **Day 5**: Testing and refinement

## Rollback Plan

If issues arise:
1. TODO tools are optional - old behavior still works
2. Can disable TODO checking in session protocol
3. All changes are backward compatible