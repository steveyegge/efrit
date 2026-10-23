;;; test-instructions.el --- layered AGENTS.md / CLAUDE.md loading -*- lexical-binding: t; -*-

;;; Commentary:
;; Builds a small directory tree under a temp dir:
;;
;;   org/AGENTS.md                (ancestor)
;;   org/proj/CLAUDE.md           (project; AGENTS.md absent here)
;;   org/proj/CLAUDE.local.md     (local override)
;;   org/proj/docs/style.md       (@imported by CLAUDE.md)
;;
;; and checks that all of them reach the prompt in that order, that
;; only the first matching name per directory is used, that imports
;; resolve relative to the importing file, that cycles and missing
;; imports are noted, and that the size caps keep the most specific
;; file.  User files are pointed at the temp dir so the real home
;; directory never leaks into a test.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-instructions)

(defvar efrit-project-root)

(defun test-instr--write (path text)
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert text)))

(defmacro test-instr--with-tree (&rest body)
  "Run BODY with `top', `org', `proj' bound to fresh temp dirs and the
project root set to `proj'.  User files and ancestor walking are
scoped so nothing outside `top' is read."
  (declare (indent 0))
  `(let* ((top (file-name-as-directory (make-temp-file "efrit-instr-" t)))
          (org (file-name-as-directory (expand-file-name "org" top)))
          (proj (file-name-as-directory (expand-file-name "proj" org)))
          (efrit-project-root proj)
          (efrit-instructions-user-files (list (expand-file-name "user/CLAUDE.md" top)))
          (efrit-instructions-ancestors t)
          (efrit-instructions-max-file-size 50000)
          (efrit-instructions-max-total-size 120000)
          (efrit-instructions-max-import-depth 3))
     (make-directory proj t)
     (unwind-protect (progn ,@body)
       (delete-directory top t))))

(defun test-instr--layers (located)
  (mapcar #'cdr located))

(ert-deftest test-instr-locate-orders-user-ancestor-project-local ()
  (test-instr--with-tree
    (test-instr--write (expand-file-name "user/CLAUDE.md" top) "user rules")
    (test-instr--write (expand-file-name "AGENTS.md" org) "org rules")
    (test-instr--write (expand-file-name "CLAUDE.md" proj) "proj rules")
    (test-instr--write (expand-file-name "CLAUDE.local.md" proj) "local rules")
    (let ((located (efrit-instructions-locate proj)))
      ;; ancestors above `org' (top, /tmp, /) have no files, so exactly these
      (should (equal (test-instr--layers located) '(user ancestor project local)))
      (should (equal (file-name-nondirectory (car (nth 1 located))) "AGENTS.md"))
      (should (equal (file-name-nondirectory (car (nth 2 located))) "CLAUDE.md")))))

(ert-deftest test-instr-first-name-wins-within-a-directory ()
  (test-instr--with-tree
    (test-instr--write (expand-file-name "AGENTS.md" proj) "agents")
    (test-instr--write (expand-file-name "CLAUDE.md" proj) "claude")
    (let ((located (efrit-instructions-locate proj)))
      (should (= (length located) 1))
      (should (equal (file-name-nondirectory (caar located)) "AGENTS.md")))
    ;; .efrit/AGENTS.md outranks both
    (test-instr--write (expand-file-name ".efrit/AGENTS.md" proj) "efrit-specific")
    (should (string-suffix-p ".efrit/AGENTS.md" (caar (efrit-instructions-locate proj))))))

(ert-deftest test-instr-text-labels-sections-most-specific-last ()
  (test-instr--with-tree
    (test-instr--write (expand-file-name "AGENTS.md" org) "ORG-RULE")
    (test-instr--write (expand-file-name "CLAUDE.md" proj) "PROJ-RULE")
    (test-instr--write (expand-file-name "CLAUDE.local.md" proj) "LOCAL-RULE")
    (let ((text (efrit-instructions-text proj)))
      (should (string-prefix-p "PROJECT-SPECIFIC INSTRUCTIONS" text))
      (should (string-match-p "more specific) file wins" text))
      (should (< (string-match "ORG-RULE" text)
                 (string-match "PROJ-RULE" text)))
      (should (< (string-match "PROJ-RULE" text)
                 (string-match "LOCAL-RULE" text)))
      (should (string-match-p "### .*org/AGENTS.md (from a directory above the project)" text))
      (should (string-match-p "### .*proj/CLAUDE.md (project instructions)" text))
      (should (string-match-p "### .*CLAUDE.local.md (local, unversioned overrides)" text)))))

(ert-deftest test-instr-none-found-is-nil-and-empty-for-prompt ()
  (test-instr--with-tree
    (should-not (efrit-instructions-locate proj))
    (should-not (efrit-instructions-text proj))
    (should (equal (efrit-instructions-for-prompt proj) ""))))

(ert-deftest test-instr-ancestors-can-be-disabled ()
  (test-instr--with-tree
    (test-instr--write (expand-file-name "AGENTS.md" org) "org")
    (test-instr--write (expand-file-name "CLAUDE.md" proj) "proj")
    (let ((efrit-instructions-ancestors nil))
      (should (equal (test-instr--layers (efrit-instructions-locate proj)) '(project))))))

(ert-deftest test-instr-imports-resolve-relative-to-importer ()
  (test-instr--with-tree
    (test-instr--write (expand-file-name "docs/style.md" proj) "STYLE-BODY")
    (test-instr--write (expand-file-name "CLAUDE.md" proj)
                       "Top.\n@docs/style.md\nBottom. Mail me @home, not an import.\n")
    (let ((text (efrit-instructions-text proj)))
      (should (string-match-p "STYLE-BODY" text))
      (should (string-match-p "<!-- imported from .*docs/style.md -->" text))
      (should (string-match-p "Mail me @home" text))
      (should-not (string-match-p "import not found: .*home" text)))))

(ert-deftest test-instr-import-missing-and-cycle-are-noted ()
  (test-instr--with-tree
    (test-instr--write (expand-file-name "a.md" proj) "A\n@b.md\n")
    (test-instr--write (expand-file-name "b.md" proj) "B\n@a.md\n@nothere.md\n")
    (test-instr--write (expand-file-name "CLAUDE.md" proj) "@a.md\n")
    (let ((text (efrit-instructions-text proj)))
      (should (string-match-p "\\bA\\b" text))
      (should (string-match-p "\\bB\\b" text))
      (should (string-match-p "import skipped: .*a.md is already being imported" text))
      (should (string-match-p "import not found: .*nothere.md" text)))))

(ert-deftest test-instr-import-depth-is-bounded ()
  (test-instr--with-tree
    (test-instr--write (expand-file-name "1.md" proj) "L1\n@2.md\n")
    (test-instr--write (expand-file-name "2.md" proj) "L2\n@3.md\n")
    (test-instr--write (expand-file-name "3.md" proj) "L3\n@4.md\n")
    (test-instr--write (expand-file-name "4.md" proj) "L4-SHOULD-NOT-APPEAR\n")
    (test-instr--write (expand-file-name "CLAUDE.md" proj) "@1.md\n")
    (let* ((efrit-instructions-max-import-depth 3)
           (text (efrit-instructions-text proj)))
      (should (string-match-p "L3" text))
      (should-not (string-match-p "L4-SHOULD-NOT-APPEAR" text)))))

(ert-deftest test-instr-file-cap-truncates-with-note ()
  (test-instr--with-tree
    (test-instr--write (expand-file-name "CLAUDE.md" proj) (make-string 1000 ?x))
    (let* ((efrit-instructions-max-file-size 100)
           (text (efrit-instructions-text proj)))
      (should (string-match-p (regexp-quote efrit-instructions-truncation-note) text))
      (should (< (length text) 600)))))

(ert-deftest test-instr-total-cap-drops-least-specific-first ()
  (test-instr--with-tree
    (test-instr--write (expand-file-name "AGENTS.md" org) (concat "ORG " (make-string 400 ?o)))
    (test-instr--write (expand-file-name "CLAUDE.md" proj) (concat "PROJ " (make-string 400 ?p)))
    (let* ((efrit-instructions-max-total-size 700)
           (text (efrit-instructions-text proj)))
      ;; the org section became a stub; the project section survived whole
      (should (string-match-p "\\[omitted: instructions block exceeds" text))
      (should (string-match-p (make-string 400 ?p) text))
      (should-not (string-match-p (make-string 400 ?o) text)))))

(ert-deftest test-instr-prompt-includes-block ()
  "The command system prompt carries the assembled block."
  (require 'efrit-do)
  (test-instr--with-tree
    (test-instr--write (expand-file-name "CLAUDE.md" proj) "USE-TABS-NEVER")
    (let ((prompt (efrit-do--command-system-prompt)))
      (should (string-match-p "PROJECT-SPECIFIC INSTRUCTIONS" prompt))
      (should (string-match-p "USE-TABS-NEVER" prompt)))))

(provide 'test-instructions)
;;; test-instructions.el ends here
