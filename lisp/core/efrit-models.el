;;; efrit-models.el --- Discover, probe and select models -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; `efrit-default-model' is a free-form string, and the failure mode
;; when it is wrong for the endpoint is an opaque API error such as
;;
;;   API Error (api_error): no keys found that support model: X
;;   API Error (not_found_error): model: X
;;
;; This module gives efrit a way to answer "then which models *do*
;; work here?":
;;
;; - `efrit-models-list' asks the endpoint's GET /v1/models (served
;;   by Anthropic and by most Anthropic-compatible gateways).  When
;;   that is unavailable it falls back to `efrit-models-fallback'.
;; - `efrit-models-probe' sends a one-token request with a given model
;;   and returns `ok' or the error string.
;; - `efrit-select-model' is the interactive command: completing-read
;;   over the list (with a "probe all" option that filters to the
;;   ones that actually answer), then sets `efrit-default-model' for
;;   the session and offers to persist it with `customize-save-variable'.
;;
;; `efrit-models-model-error-p' recognises the error strings, so the
;; doctor and the loop can route the user here instead of showing raw
;; JSON.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'url)
(require 'efrit-common)

(declare-function efrit-api-request-async "efrit-api")
(declare-function efrit-api-build-headers "efrit-api")
(declare-function efrit-api-cacheable-system "efrit-api")
(defvar efrit-default-model)
(defvar efrit-api-prompt-caching)

(defgroup efrit-models nil
  "Model discovery and selection."
  :group 'efrit
  :prefix "efrit-models-")

(defcustom efrit-models-fallback
  '("claude-sonnet-4-5" "claude-opus-4-1" "claude-sonnet-4-20250514"
    "claude-3-7-sonnet-latest" "claude-3-5-haiku-latest")
  "Model ids offered when the endpoint has no /v1/models.
Anthropic's public ids; gateways frequently use different ones, so
this is only a starting point for `efrit-select-model'."
  :type '(repeat string)
  :group 'efrit-models)

(defcustom efrit-models-probe-timeout 15
  "Seconds to wait for one model probe."
  :type 'integer
  :group 'efrit-models)

;;; Error recognition

(defconst efrit-models--error-regexps
  '("no keys found that support model"      ; Bifrost-style gateways
    "not_found_error.*model"                ; Anthropic 404
    "model[: ]+[^ ]+ +\\(?:is \\)?not found"
    "model .* does not exist"               ; OpenAI-style gateways
    "unknown model" "unsupported model" "invalid model"
    "model_not_found")
  "Substrings/regexps that mean: the model id is not accepted here.")

(defun efrit-models-model-error-p (message)
  "Non-nil if MESSAGE (an API/HTTP error string) is about an unknown model."
  (and (stringp message)
       (cl-some (lambda (re) (string-match-p re message))
                efrit-models--error-regexps)))

;;; Listing

(defun efrit-models--models-url ()
  (concat (efrit-common-get-base-url) "/v1/models"))

(defun efrit-models--parse-list (body)
  "Extract model ids from a /v1/models BODY (Anthropic or OpenAI shape)."
  (condition-case nil
      (let* ((obj (json-parse-string body :object-type 'hash-table
                                     :array-type 'list))
             (data (or (gethash "data" obj) (gethash "models" obj))))
        (delq nil
              (mapcar (lambda (m)
                        (and (hash-table-p m)
                             (or (gethash "id" m) (gethash "name" m))))
                      data)))
    (error nil)))

(defun efrit-models-list (&optional timeout)
  "Return model ids from the endpoint's /v1/models, or nil.
Synchronous; waits at most TIMEOUT (default 10) seconds.  Never
signals: a missing route, auth failure or parse failure yields nil so
callers fall back to `efrit-models-fallback'."
  (require 'efrit-api)
  (condition-case nil
      (let* ((url-request-method "GET")
             (url-request-extra-headers
              (efrit-api-build-headers (efrit-common-get-api-key)))
             (buf (url-retrieve-synchronously (efrit-models--models-url)
                                              t nil (or timeout 10))))
        (when buf
          (unwind-protect
              (with-current-buffer buf
                (goto-char (point-min))
                (when (and (re-search-forward "^HTTP/[0-9.]+ 2" (line-end-position) t)
                           (search-forward "\n\n" nil t))
                  (efrit-models--parse-list
                   (decode-coding-region (point) (point-max) 'utf-8 t))))
            (kill-buffer buf))))
    (error nil)))

(defun efrit-models-candidates ()
  "Model ids to offer: the endpoint's list if it has one, else the fallback.
The current `efrit-default-model' is always included, first."
  (let ((remote (efrit-models-list)))
    (delete-dups
     (cons efrit-default-model
           (or (and remote (sort (copy-sequence remote) #'string<))
               (copy-sequence efrit-models-fallback))))))

;;; Probing

(defun efrit-models-probe (model)
  "Send a one-token request with MODEL.  Return `ok' or an error string.
Synchronous with a deadline of `efrit-models-probe-timeout'."
  (require 'efrit-api)
  (let* ((efrit-api-prompt-caching nil)
         (req `(("model" . ,model)
                ("max_tokens" . 1)
                ("messages" . [(("role" . "user") ("content" . "ping"))])))
         (result nil) (done nil))
    (condition-case err
        (progn
          (efrit-api-request-async
           req
           (lambda (_resp) (setq result 'ok done t))
           (lambda (msg) (setq result msg done t)))
          (with-timeout (efrit-models-probe-timeout
                         (unless done (setq result "timed out")))
            (while (not done) (accept-process-output nil 0.1))))
      (error (setq result (error-message-string err))))
    result))

(defun efrit-models-probe-all (models)
  "Probe each of MODELS; return an alist (MODEL . RESULT) in order."
  (let ((n 0) (total (length models)))
    (mapcar (lambda (m)
              (cl-incf n)
              (message "efrit: probing model %d/%d: %s" n total m)
              (cons m (efrit-models-probe m)))
            models)))

;;; Selection

(defun efrit-models--persist-maybe (model)
  "Set MODEL for this session and offer to save it in the custom file."
  (setq efrit-default-model model)
  (when (and (not noninteractive)
             (y-or-n-p (format "Save efrit-default-model = %S in your custom file? " model)))
    (customize-save-variable 'efrit-default-model model))
  (message "efrit-default-model is now %s" model))

;;;###autoload
(defun efrit-select-model (&optional probe)
  "Choose `efrit-default-model' from what the endpoint offers.
Lists /v1/models when the endpoint has it, else a built-in list.
With prefix argument PROBE (or when the current model is failing),
each candidate is probed with a one-token request first and only the
ones that answer are offered.  The choice is set for the session and
optionally saved with `customize-save-variable'."
  (interactive "P")
  (let* ((candidates (efrit-models-candidates))
         (choices
          (if probe
              (let ((results (efrit-models-probe-all candidates)))
                (or (mapcar #'car (cl-remove-if-not (lambda (r) (eq (cdr r) 'ok)) results))
                    (user-error "None of %d candidate models answered; check the endpoint and key (M-x efrit-doctor)"
                                (length candidates))))
            candidates))
         (choice (completing-read
                  (format "Model%s: " (if probe " (verified)" ""))
                  choices nil nil nil nil
                  (if (member efrit-default-model choices) efrit-default-model (car choices)))))
    (when (and (not probe) (not (member choice candidates)))
      ;; Free-form entry: check it before committing
      (let ((r (efrit-models-probe choice)))
        (unless (eq r 'ok)
          (user-error "Model %s did not answer: %s" choice r))))
    (efrit-models--persist-maybe choice)
    choice))

(provide 'efrit-models)

;;; efrit-models.el ends here
