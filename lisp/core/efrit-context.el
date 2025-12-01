;;; efrit-context.el --- Context builder using EIEIO and builder pattern -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; This module provides an EIEIO-based context builder for constructing
;; API context alists. It replaces ad-hoc context building with a composable,
;; type-safe builder pattern.
;;
;; The builder makes it easy to:
;; - Add messages in a controlled way
;; - Include/exclude metadata conditionally
;; - Validate context structure
;; - Convert to API format

;;; Code:

(require 'eieio)

;;; Context class

(defclass efrit-api-context ()
  ((messages :initarg :messages
             :initform (vector)
             :type vector
             :documentation "Vector of messages for Claude API")
   (metadata :initarg :metadata
             :initform (make-hash-table :test 'equal)
             :type hash-table
             :documentation "Hash table of context metadata")
   (environment :initarg :environment
                :initform (make-hash-table :test 'equal)
                :type hash-table
                :documentation "Hash table of environment variables")
   (include-metadata :initarg :include-metadata
                     :initform nil
                     :type boolean
                     :documentation "Whether to include metadata in output"))
  :documentation "Represents API context with messages and metadata.
Use efrit-context-builder to construct instances.")

(cl-defmethod efrit-api-context-get-messages ((ctx efrit-api-context))
  "Return the messages vector from CTX."
  (oref ctx messages))

(cl-defmethod efrit-api-context-get-metadata ((ctx efrit-api-context))
  "Return the metadata hash table from CTX."
  (oref ctx metadata))

(cl-defmethod efrit-api-context-get-environment ((ctx efrit-api-context))
  "Return the environment hash table from CTX."
  (oref ctx environment))

(cl-defmethod efrit-api-context-include-metadata-p ((ctx efrit-api-context))
  "Return whether metadata should be included in API output."
  (oref ctx include-metadata))

(cl-defmethod efrit-api-context-valid-p ((ctx efrit-api-context))
  "Validate that CTX has required fields.
Returns (valid-p . error-msg) where valid-p is t or nil."
  (let ((messages (efrit-api-context-get-messages ctx)))
    (if (or (null messages) (zerop (length messages)))
        (cons nil "Context requires at least one message")
      (cons t nil))))

(cl-defmethod efrit-api-context-to-api-format ((ctx efrit-api-context))
  "Convert CTX to alist suitable for Claude API.
Returns alist with messages field (and optionally metadata field)."
  (let* ((messages (efrit-api-context-get-messages ctx))
         (result `(("messages" . ,(vconcat messages)))))
    (when (efrit-api-context-include-metadata-p ctx)
      (let ((metadata (efrit-api-context-get-metadata ctx)))
        (unless (zerop (hash-table-count metadata))
          (push `("metadata" . ,metadata) result))))
    result))

;;; Context builder class

(defclass efrit-context-builder-impl ()
  ((messages :initarg :messages
             :initform (vector)
             :type vector
             :documentation "Accumulated messages (will be reversed to oldest-first)")
   (metadata :initarg :metadata
             :initform (make-hash-table :test 'equal)
             :type hash-table
             :documentation "Accumulated metadata")
   (environment :initarg :environment
                :initform (make-hash-table :test 'equal)
                :type hash-table
                :documentation "Accumulated environment")
   (include-metadata :initarg :include-metadata
                     :initform nil
                     :type boolean
                     :documentation "Whether to include metadata in output"))
  :documentation "Implementation class for context builder.
Use efrit-context-builder function to create instances.")

(defun efrit-context-builder ()
  "Create a new context builder.
Returns a builder object for use with builder methods."
  (make-instance 'efrit-context-builder-impl))

(cl-defmethod efrit-context-builder-add-message ((builder efrit-context-builder-impl) role content)
  "Add a message to BUILDER with ROLE (symbol) and CONTENT (string).
Returns BUILDER for method chaining.
ROLE should be user or assistant symbol."
  (let ((msg-vector (oref builder messages))
        (new-msg `(("role" . ,(symbol-name role))
                   ("content" . ,content))))
    ;; Build messages in reverse order for efficiency, will reverse on build
    (oset builder messages (vconcat (vector new-msg) msg-vector)))
  builder)

(cl-defmethod efrit-context-builder-add-messages ((builder efrit-context-builder-impl) messages)
  "Add multiple MESSAGES (list of plists) to BUILDER.
Each message should be a plist like (:role user :content \"...\").
Returns BUILDER for method chaining."
  (dolist (msg messages)
    (let ((role (plist-get msg :role))
          (content (plist-get msg :content)))
      (when (and role content)
        (efrit-context-builder-add-message builder role content))))
  builder)

(cl-defmethod efrit-context-builder-add-metadata ((builder efrit-context-builder-impl) key value)
  "Add metadata entry (KEY . VALUE) to BUILDER.
Returns BUILDER for method chaining."
  (puthash key value (oref builder metadata))
  builder)

(cl-defmethod efrit-context-builder-set-metadata ((builder efrit-context-builder-impl) metadata-hash)
  "Set the entire metadata hash table for BUILDER to METADATA-HASH.
Returns BUILDER for method chaining."
  (oset builder metadata metadata-hash)
  builder)

(cl-defmethod efrit-context-builder-add-environment ((builder efrit-context-builder-impl) key value)
  "Add environment entry (KEY . VALUE) to BUILDER.
Returns BUILDER for method chaining."
  (puthash key value (oref builder environment))
  builder)

(cl-defmethod efrit-context-builder-set-environment ((builder efrit-context-builder-impl) env-hash)
  "Set the entire environment hash table for BUILDER to ENV-HASH.
Returns BUILDER for method chaining."
  (oset builder environment env-hash)
  builder)

(cl-defmethod efrit-context-builder-include-metadata ((builder efrit-context-builder-impl) include-p)
  "Set whether BUILDER should include metadata in API output.
INCLUDE-P should be t or nil. Returns BUILDER for method chaining."
  (oset builder include-metadata include-p)
  builder)

(cl-defmethod efrit-context-builder-build ((builder efrit-context-builder-impl))
  "Build and return an efrit-api-context from BUILDER.
Returns a fully constructed context object."
  (make-instance
   'efrit-api-context
   :messages (oref builder messages)  ; Already in correct order from building backwards
   :metadata (oref builder metadata)
   :environment (oref builder environment)
   :include-metadata (oref builder include-metadata)))

;;; Convenience functions

(defun efrit-context-create-from-messages (messages &optional include-metadata)
  "Create an efrit-api-context from MESSAGES.
MESSAGES should be a list of plists with :role and :content.
If INCLUDE-METADATA is non-nil, metadata will be included in API output."
  (let ((builder (efrit-context-builder)))
    (efrit-context-builder-add-messages builder messages)
    (when include-metadata
      (efrit-context-builder-include-metadata builder t))
    (efrit-context-builder-build builder)))

(provide 'efrit-context)

;;; efrit-context.el ends here
