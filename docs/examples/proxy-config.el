;;; proxy-config.el --- efrit through an Anthropic-compatible gateway -*- lexical-binding: t; -*-

;; Example only.  Replace every example.com value with your own.  Keep
;; real hostnames, key-minting endpoints and credentials out of any
;; file you might commit; put them in ~/.authinfo.gpg or a private,
;; untracked file that this one loads.

;;; Direct Anthropic (the default) ---------------------------------------
;;
;; Nothing to set.  Put the key in ~/.authinfo.gpg:
;;   machine api.anthropic.com login personal password sk-ant-...
;; or export ANTHROPIC_API_KEY.

;;; Through a gateway that speaks the Anthropic Messages API ---------------
;;
;; Many LLM gateways (LiteLLM, Bifrost, Portkey, OpenRouter, ...) expose
;; Anthropic's /v1/messages under a prefix and authenticate with a
;; bearer token of their own format.  Two settings cover that:

(defun my-gateway-key ()
  "Return the gateway token.  Fetch it however your site does it:
from auth-source, an environment variable, or a key-minting service.
Cache it if minting is slow; re-mint on a 401."
  (or (getenv "MY_GATEWAY_TOKEN")
      (auth-source-pick-first-password :host "gateway.example.com")))

(use-package efrit
  :load-path "~/src/efrit/lisp"
  ;; With :load-path nothing loads until a :commands entry runs, so
  ;; register every command up front from the generated autoloads file
  ;; (run `make autoloads' in the checkout once; it also puts the
  ;; lisp/ subdirectories on load-path).  Without this, M-x efrit-doctor
  ;; is unknown until M-x efrit has loaded efrit.el.
  :init (load "efrit-autoloads" t t)
  :bind (("C-c e" . efrit-do)
         ("C-c E" . efrit))
  :custom
  ;; Base only: efrit appends /v1/messages.  A trailing slash is fine.
  (efrit-api-base-url "https://gateway.example.com/anthropic")
  ;; 'bearer sends Authorization: Bearer TOKEN and skips the sk- format check.
  (efrit-api-auth-scheme 'bearer)
  (efrit-api-key #'my-gateway-key)
  ;; Gateways often use their own model ids.
  (efrit-default-model "claude-sonnet-4-5")
  ;; Optional extra headers, e.g. for attribution/telemetry.
  (efrit-api-custom-headers '(("x-example-client" . "efrit")))
  ;; Flip to nil if the gateway rejects cache_control blocks
  ;; (M-x efrit-doctor with C-u tells you).
  (efrit-api-prompt-caching t))

;; Verify every layer, including a live one-token request:
;;   C-u M-x efrit-doctor

;;; proxy-config.el ends here
