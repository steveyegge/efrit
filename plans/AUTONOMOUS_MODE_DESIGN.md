# Efrit Agent Mode Architecture

## Design Principles

1. **Self-Aware via LLM** - Efrit knows it's an agent, tracks its own state, delegates reasoning to backend LLM
2. **Signal-Based Protocol** - Client decodes LLM signals (TODO updates, action types, completion status)
3. **Model-Agnostic Backend** - Can switch between Claude, GPT, local models
4. **Aggressive Autonomy** - Only stops when succeeded or genuinely stuck needing user input
5. **TODO-Driven Progress** - Maintains detailed markdown plans, updated before/after every action

## Mode Hierarchy

```
efrit-do           → Command execution (one-shot and multi-turn with AI guidance)
efrit-chat         → Conversational with mixed tasks/questions
efrit-agent        → Aggressive problem-solving until complete (NEW)
```

## Autonomous Mode Protocol

### Request Format
```json
{
  "goal": "Upgrade my chatgpt-shell package",
  "context": "It's several versions behind, managed by straight.el",
  "session_id": "upgrade-20250814",
  "max_iterations": 50,
  "model_backend": "claude-3.5-sonnet"
}
```

### LLM Signal Protocol

The LLM communicates back via structured JSON with these signals:

```json
{
  "status": "planning|executing|reflecting|stuck|complete",
  "todos": [
    {"id": "investigate-version", "status": "completed", "priority": "high"},
    {"id": "check-conflicts", "status": "in-progress", "priority": "medium"}
  ],
  "next_action": {
    "type": "eval|shell|file_read|ai_consult|user_input",
    "content": "(shell-command-to-string \"git pull\")",
    "rationale": "Need to update the repository to latest version"
  },
  "self_assessment": "I found the package is 83 commits behind. Next I'll attempt a git pull.",
  "completion_estimate": "3 more actions needed",
  "user_input_needed": null
}
```

### Core Loop Implementation

```elisp
(defun efrit-agent-solve (goal &optional context session-id)
  "Autonomous problem-solving mode."
  (let ((session (efrit-agent--init-session goal context session-id)))
    (while (not (efrit-agent--session-complete-p session))
      
      ;; Get next action from LLM
      (let* ((prompt (efrit-agent--build-prompt session))
             (response (efrit-agent--consult-llm prompt))
             (signal (efrit-agent--parse-signal response)))
        
        ;; Update TODO list
        (efrit-agent--update-todos session (plist-get signal :todos))
        
        ;; Handle different signal types
        (pcase (plist-get signal :status)
          ('stuck 
           (efrit-agent--request-user-input session signal))
          ('complete 
           (efrit-agent--finalize-session session))
          (_ 
           ;; Execute the next action
           (let ((action (plist-get signal :next_action))
                 (result (efrit-agent--execute-action action)))
             (efrit-agent--record-result session action result)
             
             ;; Self-reflection prompt to LLM
             (efrit-agent--request-reflection session action result))))))))
```

## Key Features

### 1. **Self-Awareness Through Prompting**
```
You are Efrit, an autonomous Emacs agent. You maintain a TODO list and work 
systematically toward goals. You have access to elisp evaluation, shell commands, 
and file operations. You can consult other AI models for complex reasoning.

Current Goal: [user goal]
Session State: [current todos, actions taken, context]
Available Tools: [tool descriptions]

Respond with your next action as JSON...
```

### 2. **Aggressive Execution Strategy**  
- **No partial stops** - Keep going until done or stuck
- **Error recovery** - Try multiple approaches automatically
- **Resource utilization** - Use all available tools
- **Escalation** - Consult more powerful models if stuck

### 3. **TODO-Driven Transparency**
- Every response updates the TODO list
- User can see progress in real-time
- Session state persists across interruptions
- Markdown reports generated automatically

### 4. **Model Backend Abstraction**
```elisp
(defcustom efrit-agent-backend "claude-3.5-sonnet"
  "Default model backend for agent mode."
  :type '(choice (const "claude-3.5-sonnet")
                 (const "gpt-4") 
                 (const "local-llama")
                 (string :tag "Custom API endpoint")))
```

## Implementation Plan

1. **`efrit-agent.el`** - Core agent loop
2. **`efrit-agent-memory.el`** - Session state management  
3. **`efrit-agent-backend.el`** - Model abstraction layer
4. **`efrit-agent-ui.el`** - TODO display and progress UI

## Test Case: ChatGPT-Shell Upgrade

With this architecture, the user would simply run:
```elisp
(efrit-agent-solve "Upgrade my chatgpt-shell package")
```

And Efrit would autonomously:
1. Investigate current setup
2. Identify the 83-commit gap  
3. Plan the upgrade approach
4. Execute git pull / straight.el rebuild
5. Test the upgrade worked
6. Report completion

**Does this architecture align with your vision for autonomous Efrit?**
