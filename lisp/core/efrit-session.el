;;; efrit-session.el --- Session and context management for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Unified session management for Efrit.
;;
;; This module is a facade that aggregates the session subsystem:
;; - efrit-session-core: Data structures, lifecycle, registry, queue
;; - efrit-session-worklog: Work log management, compression, budget integration
;; - efrit-session-metrics: Metrics tracking (commands, TODOs, API calls, etc.)
;; - efrit-session-context: Unified context system, context ring
;; - efrit-session-transcript: Transcript persistence and session resume
;;
;; Following the Zero Client-Side Intelligence principle, these modules
;; only RECORD what happened - they do not make decisions about what
;; to do next.  That's Claude's job.
;;
;; For backward compatibility, this file provides all the public symbols
;; from the submodules.  New code should prefer requiring specific
;; submodules directly when possible.

;;; Code:

;; Load all session submodules
(require 'efrit-session-core)
(require 'efrit-session-worklog)
(require 'efrit-session-metrics)
(require 'efrit-session-context)
(require 'efrit-session-transcript)
(require 'efrit-repl-session)
(require 'efrit-repl-loop)

;; Initialize the context system on load
(efrit-context-init)

;;; Public API: Session Continuation (from efrit-session-worklog)
;;
;; These functions are part of the public session API and are used by the
;; async execution loop to maintain session state across multiple Claude API
;; calls. Applications should use only these functions, not the implementation
;; details in efrit-session-worklog.el.

;; Declare public session API functions for external visibility
(declare-function efrit-session-add-tool-results
                  "efrit-session-worklog"
                  (session tool-results))

(declare-function efrit-session-build-tool-result
                  "efrit-session-worklog"
                  (tool-id result &optional is-error))

(declare-function efrit-session-get-api-messages-for-continuation
                  "efrit-session-worklog"
                  (session))

(provide 'efrit-session)

;;; efrit-session.el ends here
