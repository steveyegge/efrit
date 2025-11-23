---
name: Feature Request
about: Suggest a new feature or enhancement for Efrit
title: "[FEATURE] "
labels: ["feature"]
assignees: []
---

## Feature Description

**Clear, concise description of the proposed feature**

<!-- Example: "Add tool to read PDF files and extract text for Claude analysis" -->

## Problem Statement

**What problem does this solve? What workflow does it enable?**

<!-- Example: "Currently, users can't easily analyze PDF documents with Claude because Efrit has no PDF reading capability" -->

## Proposed Solution

**How should this work?**

<!-- Describe the user experience, API, or workflow -->

## Pure Executor Alignment

**CRITICAL**: All features must align with Efrit's [Zero Client-Side Intelligence principle](https://github.com/steveyegge/efrit/blob/main/ARCHITECTURE.md).

**Does this feature add client-side intelligence?**

- [ ] No - This is pure execution (context gathering, tool execution, result relay)
- [ ] Unsure - Needs architectural review
- [ ] Yes - This requires pattern matching or decision logic *(will be rejected)*

**Explanation of how this preserves Pure Executor principle:**

<!--
Examples of ALLOWED features:
✅ New tool for gathering context (buffer_pdf_text, directory_tree)
✅ New execution capability (eval_python, shell_async)
✅ Better result relay (structured error messages)
✅ State persistence (session history, logs)

Examples of PROHIBITED features:
❌ Automatic error parsing or pattern matching
❌ Task-specific heuristics or suggestions
❌ Pre-written code solutions or templates
❌ Decision logic about which tool to use next
-->

## Implementation Sketch

**Optional: How might this be implemented?**

```elisp
;; Example code or pseudocode
(defun efrit-tools-proposed-feature (arg)
  "Implementation sketch..."
  )
```

**Which module should this live in?**

- [ ] `efrit-tools.el` (new tool for Claude to call)
- [ ] `efrit-do.el` (core execution logic)
- [ ] `efrit-chat.el` (user interface)
- [ ] `efrit-remote-queue.el` (AI-to-AI communication)
- [ ] New module (describe below)
- [ ] Other:

## Alternatives Considered

**Have you considered other approaches?**

<!-- What alternatives did you think about? Why is this the best approach? -->

## Additional Context

<!-- Add any other context, mockups, or examples -->

## Checklist

- [ ] I've read [ARCHITECTURE.md](https://github.com/steveyegge/efrit/blob/main/ARCHITECTURE.md) and understand the Pure Executor principle
- [ ] This feature does NOT add client-side intelligence
- [ ] I've checked existing issues to avoid duplicates
- [ ] I've described the problem clearly
- [ ] I've proposed a concrete solution
