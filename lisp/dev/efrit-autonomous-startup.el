;;; efrit-autonomous-startup.el --- Autonomous AI development environment startup -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Efrit Development Team
;; Keywords: ai, development, autonomous
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;;
;; This file configures an isolated Emacs instance for autonomous AI development.
;; It auto-loads Efrit, starts the remote queue system, and sets up an environment
;; where AI agents can develop and enhance Efrit without human intervention.
;;
;; Usage:
;;   emacs --daemon=efrit-ai --load efrit-autonomous-startup.el
;;
;; The AI agent communicates via the file queue system in the efrit data directory

;;; Code:

;; Variable declarations
(defvar efrit-remote-queue-directory nil)
(defvar efrit-remote-queue-timer nil)
(defvar efrit-autonomous-mode nil)
(defvar efrit-debug-mode nil)

;; Function declarations
(declare-function efrit-remote-queue-start "efrit-remote-queue")
(declare-function efrit-remote-queue-stop "efrit-remote-queue")
(declare-function efrit-remote-queue-status "efrit-remote-queue")
;; Compile-time visibility for efrit-config helpers used below
(declare-function efrit-config-workspace-dir "efrit-config")
(declare-function efrit-config-data-file "efrit-config")

(require 'package)

;; Minimal package setup for autonomous mode
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Load Efrit core system FIRST to set up load paths
;; efrit-source-dir points to lisp/ (parent of dev/)
(defvar efrit-source-dir
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Directory containing Efrit source files (lisp/).")

(message "Efrit Autonomous: Loading Efrit from %s" efrit-source-dir)

;; Add Efrit source to load path BEFORE requiring any efrit modules
(add-to-list 'load-path efrit-source-dir)
(add-to-list 'load-path (expand-file-name "core" efrit-source-dir))
(add-to-list 'load-path (expand-file-name "interfaces" efrit-source-dir))
(add-to-list 'load-path (expand-file-name "support" efrit-source-dir))

;; NOW we can require efrit-config
(require 'efrit-config)

;; Configure autonomous environment
(message "Efrit Autonomous: Starting AI development environment...")

;; Set up isolated working directory
(defvar efrit-autonomous-work-dir nil
  "Working directory for autonomous AI development.")

;; Note: workspace dir is initialized after efrit-config is loaded below.

;; Disable unnecessary UI elements for daemon mode
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Initialize directories using efrit-config (already loaded above)
(setq efrit-autonomous-work-dir (efrit-config-workspace-dir))
(defvar efrit-autonomous-queue-dir (efrit-config-data-file "queue-ai")
  "Queue directory for autonomous AI communication.")

;; Ensure workspace exists
(unless (file-directory-p efrit-autonomous-work-dir)
  (make-directory efrit-autonomous-work-dir t))

;; Set default directory to workspace
(setq default-directory efrit-autonomous-work-dir)

(message "Efrit Autonomous: Using workspace %s" efrit-autonomous-work-dir)
(message "Efrit Autonomous: Using queue %s" efrit-autonomous-queue-dir)

;; Override queue directory for AI isolation (MUST be set before loading remote-queue)
(setq efrit-remote-queue-directory efrit-autonomous-queue-dir)

;; Load Efrit modules in dependency order
(condition-case err
    (progn
      (require 'efrit-tools)
      (require 'efrit-do)
      (require 'efrit-chat)
      (require 'efrit-remote-queue)
      (require 'efrit)
      (message "Efrit modules loaded successfully"))
  (error 
   (message "Error loading Efrit modules: %s" (error-message-string err))))

;; Create queue directories
(let ((requests-dir (expand-file-name "requests" efrit-autonomous-queue-dir))
      (responses-dir (expand-file-name "responses" efrit-autonomous-queue-dir)))
  (unless (file-directory-p requests-dir)
    (make-directory requests-dir t))
  (unless (file-directory-p responses-dir)
    (make-directory responses-dir t)))

;; Start the remote queue system
;; Note: We call it directly here instead of relying on auto-start
;; to ensure it's running before any AI agent communication
(message "Efrit Autonomous: Starting remote queue system...")
(efrit-remote-queue-start)

;; Configure autonomous development settings
(setq efrit-autonomous-mode t)
(setq efrit-debug-mode t) ; Enable detailed logging for AI

;; Set up development environment
(setq user-full-name "AI Agent"
      user-mail-address "ai@autonomous.dev")

;; Configure backup and auto-save for safety
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" efrit-autonomous-work-dir))))
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-saves/" efrit-autonomous-work-dir) t)))

;; Enable key development modes
(when (fboundp 'electric-pair-mode) (electric-pair-mode 1))
(when (fboundp 'show-paren-mode) (show-paren-mode 1))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Create status monitoring functions
(defun efrit-autonomous-status ()
  "Show status of autonomous AI development environment."
  (interactive)
  (with-current-buffer (get-buffer-create "*Efrit Autonomous Status*")
    (erase-buffer)
    (insert "=== Efrit Autonomous AI Development Environment ===\n\n")
    (insert (format "Status: %s\n" (if efrit-remote-queue-timer "RUNNING" "STOPPED")))
    (insert (format "Queue Directory: %s\n" efrit-autonomous-queue-dir))
    (insert (format "Work Directory: %s\n" efrit-autonomous-work-dir))
    (insert (format "Efrit Source: %s\n" efrit-source-dir))
    (insert (format "Uptime: %s\n" (current-time-string)))
    (insert "\n=== Queue Status ===\n")
    (insert (format "Queue running: %s\n" (if efrit-remote-queue-timer "Yes" "No")))
    (insert (format "Requests pending: %s\n" 
                     (length (directory-files (expand-file-name "requests" efrit-autonomous-queue-dir) nil "^req_.*\\.json$"))))
    (insert (format "Responses available: %s\n"
                     (length (directory-files (expand-file-name "responses" efrit-autonomous-queue-dir) nil "^resp_.*\\.json$"))))
    (display-buffer (current-buffer))))

(defun efrit-autonomous-shutdown ()
  "Gracefully shutdown autonomous AI development environment."
  (interactive)
  (message "Efrit Autonomous: Shutting down...")
  (when (bound-and-true-p efrit-remote-queue-timer)
    (efrit-remote-queue-stop))
  (message "Efrit Autonomous: Shutdown complete"))

;; Set up shutdown hooks
(add-hook 'kill-emacs-hook 'efrit-autonomous-shutdown)

;; Completion message
(message "Efrit Autonomous: AI development environment ready!")
(message "Efrit Autonomous: Queue system active at %s" efrit-autonomous-queue-dir)
(message "Efrit Autonomous: AI can now communicate via JSON files")

;; Log startup completion
(let ((log-file (expand-file-name "startup.log" efrit-autonomous-work-dir)))
  (with-temp-file log-file
    (insert (format "Efrit Autonomous startup completed at %s\n" (current-time-string)))
    (insert (format "Queue directory: %s\n" efrit-autonomous-queue-dir))
    (insert (format "Work directory: %s\n" efrit-autonomous-work-dir))
    (insert "Status: READY\n")))

(provide 'efrit-autonomous-startup)
;;; efrit-autonomous-startup.el ends here
