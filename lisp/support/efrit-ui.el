;;; efrit-ui.el --- Unified UI, monitoring, and performance for Efrit -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steven Yegge

;; Author: Steven Yegge
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module consolidates all UI, monitoring, and performance functionality:
;; - Dashboard with session/TODO management (efrit-ui-dashboard)
;; - Performance optimization (caching, memory management) (efrit-ui-performance)
;; - Live TODO buffer and mode line indicator (efrit-ui-todos)
;; - Face definitions (efrit-ui-faces)
;;
;; Progress display lives in core/efrit-progress.el (event stream) and
;; interfaces/efrit-progress-buffer.el (per-session buffers); the legacy
;; efrit-ui-progress.el duplicate was removed (ef-d89).

;;; Code:

(require 'efrit-ui-faces)
(require 'efrit-ui-performance)
(require 'efrit-ui-dashboard)
(require 'efrit-ui-todos)

(defvar efrit-data-directory (expand-file-name "~/.emacs.d/.efrit/")
  "Directory for efrit data storage.")

;;; Initialization

;; Start cleanup timer
(efrit-performance-start-cleanup-timer)

(provide 'efrit-ui)

;;; efrit-ui.el ends here
