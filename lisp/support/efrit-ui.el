;;; efrit-ui.el --- Unified UI, monitoring, and performance for Efrit -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steven Yegge

;; Author: Steven Yegge
;; Version: 0.4.0
;; Package-Requires: ((emacs "28.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module consolidates all UI, monitoring, and performance functionality:
;; - Real-time progress display (efrit-ui-progress)
;; - Dashboard with session/TODO management (efrit-ui-dashboard)
;; - Performance optimization (caching, memory management) (efrit-ui-performance)
;; - Live TODO buffer and mode line indicator (efrit-ui-todos)
;; - Face definitions (efrit-ui-faces)

;; Replaces: efrit-progress.el, efrit-performance.el, efrit-dashboard.el

;;; Code:

(require 'efrit-ui-faces)
(require 'efrit-ui-progress)
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
