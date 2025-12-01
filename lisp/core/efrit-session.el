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

;; Initialize the context system on load
(efrit-context-init)

(provide 'efrit-session)

;;; efrit-session.el ends here
