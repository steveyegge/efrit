;;; test-warnings-setup.el --- Create test warnings buffer -*- lexical-binding: t -*-

;; Load the necessary Efrit modules
(add-to-list 'load-path (expand-file-name "lisp" default-directory))

(require 'efrit-common)
(require 'efrit-config) 
(require 'efrit-log)
(require 'efrit-context)
(require 'efrit-tools)
(require 'efrit-do)
(require 'efrit-protocol)
(require 'efrit-progress)
(require 'efrit-async)

;; Enable debug logging
(setq efrit-log-level 'debug)

;; Create warnings buffer with test warnings
(with-current-buffer (get-buffer-create "*Warnings*")
  (erase-buffer)
  (insert "Warning: file1.el:1: Warning: file `file1.el' lacks lexical-binding directive
Warning: file2.el:5: Warning: file `file2.el' lacks lexical-binding directive  
Warning: file3.el:10: Warning: file `file3.el' lacks lexical-binding directive
Warning: test-file.el:1: Warning: file `test-file.el' lacks lexical-binding directive
Warning: another-file.el:3: Warning: file `another-file.el' lacks lexical-binding directive")
  (goto-char (point-min)))

(message "Created *Warnings* buffer with 5 lexical-binding warnings")

;; Show the warnings buffer
(pop-to-buffer "*Warnings*")
(message "Ready to test: M-x efrit-do-async RET fix warnings in *Warnings* buffer RET")