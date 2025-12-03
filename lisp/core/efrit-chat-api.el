;;; efrit-chat-api.el --- Claude API communication for Efrit chat -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/steveyegge/efrit

;;; Commentary:
;; API communication functions for both classic and streamlined chat modes.
;; 
;; This file is now a compatibility shim that loads the refactored modules:
;; - efrit-chat-common.el: Shared variables, circuit breaker, tool helpers
;; - efrit-chat-classic.el: Classic multi-turn chat API
;; - efrit-chat-streamlined.el: Streamlined single/multi-turn API
;;
;; All public symbols from the original efrit-chat-api.el are still available
;; through this shim for backward compatibility.

;;; Code:

;; Load all the refactored modules
(require 'efrit-chat-common)
(require 'efrit-chat-classic)
(require 'efrit-chat-streamlined)

(provide 'efrit-chat-api)

;;; efrit-chat-api.el ends here
