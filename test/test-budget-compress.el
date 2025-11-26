;;; test-budget-compress.el --- Tests for budget compression -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for efrit-budget compression functions.

;;; Code:

(require 'ert)
(require 'efrit-budget)

(ert-deftest test-compress-project-files ()
  "Test project_files compression."
  (let ((result '((status . success)
                  (data . ((total_files . 150)
                           (root . "/project")
                           (files . [((path_relative . "lisp/foo.el"))
                                    ((path_relative . "lisp/bar.el"))
                                    ((path_relative . "test/test-foo.el"))
                                    ((path_relative . "docs/README.md"))]))))))
    (let ((compressed (efrit-budget-compress-project-files result)))
      (should (stringp compressed))
      (should (string-match-p "150 files" compressed))
      (should (string-match-p "\\.el" compressed)))))

(ert-deftest test-compress-search-content ()
  "Test search_content compression."
  (let ((result '((status . success)
                  (data . ((total_fetched . 25)
                           (matches . [((file_relative . "a.el"))
                                      ((file_relative . "a.el"))
                                      ((file_relative . "b.el"))
                                      ((file_relative . "c.el"))]))))))
    (let ((compressed (efrit-budget-compress-search-content result)))
      (should (stringp compressed))
      (should (string-match-p "25 matches" compressed))
      (should (string-match-p "3 files" compressed)))))

(ert-deftest test-compress-read-file ()
  "Test read_file compression."
  (let ((result '((status . success)
                  (data . ((path_relative . "lisp/core/efrit-budget.el")
                           (total_lines . 500)
                           (size . 15000)
                           (truncated . :json-false))))))
    (let ((compressed (efrit-budget-compress-read-file result)))
      (should (stringp compressed))
      (should (string-match-p "efrit-budget\\.el" compressed))
      (should (string-match-p "500 lines" compressed)))))

(ert-deftest test-compress-vcs-status-clean ()
  "Test vcs_status compression for clean repo."
  (let ((result '((status . success)
                  (data . ((current_branch . "main")
                           (is_clean . t)
                           (ahead . 2)
                           (behind . 0)
                           (staged_files . [])
                           (unstaged_files . [])
                           (untracked_files . []))))))
    (let ((compressed (efrit-budget-compress-vcs-status result)))
      (should (stringp compressed))
      (should (string-match-p "main" compressed))
      (should (string-match-p "clean" compressed))
      (should (string-match-p "ahead 2" compressed)))))

(ert-deftest test-compress-vcs-status-dirty ()
  "Test vcs_status compression for dirty repo."
  (let ((result '((status . success)
                  (data . ((current_branch . "feature/test")
                           (is_clean . :json-false)
                           (staged_files . [((path . "a.el"))])
                           (unstaged_files . [((path . "b.el")) ((path . "c.el"))])
                           (untracked_files . [((path . "new.el"))]))))))
    (let ((compressed (efrit-budget-compress-vcs-status result)))
      (should (stringp compressed))
      (should (string-match-p "feature/test" compressed))
      (should (string-match-p "1 staged" compressed))
      (should (string-match-p "2 modified" compressed)))))

(ert-deftest test-compress-vcs-diff ()
  "Test vcs_diff compression."
  (let ((result '((status . success)
                  (data . ((summary . ((files_changed . 5)
                                       (insertions . 234)
                                       (deletions . 89)))
                           (diff_type . "staged")
                           (files . [((path . "a.el"))
                                    ((path . "b.el"))
                                    ((path . "c/d.el"))
                                    ((path . "e.el"))
                                    ((path . "f.el"))]))))))
    (let ((compressed (efrit-budget-compress-vcs-diff result)))
      (should (stringp compressed))
      (should (string-match-p "5 files" compressed))
      (should (string-match-p "+234/-89" compressed)))))

(ert-deftest test-compress-vcs-log ()
  "Test vcs_log compression."
  (let ((result '((status . success)
                  (data . ((count . 15)
                           (commits . [((hash . "abc1234") (message . "Fix bug"))
                                      ((hash . "def5678") (message . "Add feature"))
                                      ((hash . "ghi9012") (message . "Refactor"))]))))))
    (let ((compressed (efrit-budget-compress-vcs-log result)))
      (should (stringp compressed))
      (should (string-match-p "15 commits" compressed))
      (should (string-match-p "abc1234" compressed)))))

(ert-deftest test-compress-dispatch ()
  "Test compression dispatch function."
  (let ((result '((status . success) (data . ((total_files . 100))))))
    (let ((compressed (efrit-budget-compress-for-history "project_files" result)))
      (should (stringp compressed))
      (should (string-match-p "100 files" compressed))))
  ;; Unknown tool
  (let ((compressed (efrit-budget-compress-for-history "unknown_tool" nil)))
    (should (string= "[unknown_tool result]" compressed))))

(ert-deftest test-compress-max-length ()
  "Test that compression respects max length."
  ;; This shouldn't exceed 500 chars
  (let ((result '((status . success)
                  (data . ((count . 999)
                           (commits . [((hash . "abc1234") (message . "A very long commit message that goes on and on"))
                                      ((hash . "def5678") (message . "Another very long commit message"))
                                      ((hash . "ghi9012") (message . "Yet another long message"))]))))))
    (let ((compressed (efrit-budget-compress-for-history "vcs_log" result)))
      (should (<= (length compressed) efrit-budget-compress-max-chars)))))

(provide 'test-budget-compress)

;;; test-budget-compress.el ends here
