;;; test-custom-functions.el --- ERT tests for custom utility functions -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Tests for custom functions and macros defined in README.org.
;; These are pure or near-pure functions that can be tested without
;; a graphical display or network access.

;;; Code:

(require 'test-helper)

;; ---------- Define functions under test ----------
;; Copied from README.org so tests run without loading the full config.

(defmacro when-linux (&rest body)
  "Execute BODY only on GNU/Linux systems."
  `(when (eq system-type 'gnu/linux) ,@body))

(defmacro when-windows (&rest body)
  "Execute BODY only on Windows systems."
  `(when (eq system-type 'windows-nt) ,@body))

(defmacro when-mac (&rest body)
  "Execute BODY only on macOS systems."
  `(when (eq system-type 'darwin) ,@body))

(defmacro func-ignore (fnc)
  "Return function that ignore its arguments and invokes FNC"
  `(lambda (&rest _rest)
     (funcall ,fnc)))

(defun my/shell-quote (str)
  "Quote STR for safe use in POSIX shell single-quoted strings.
Handles embedded single quotes by replacing ' with '\"'\"'."
  (concat "'" (replace-regexp-in-string "'" "'\"'\"'" str) "'"))

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
logical line."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(defvar my/dropbox-path nil)

(defun my/dropbox-expand (path)
  "Expand PATH relative to `my/dropbox-path'."
  (expand-file-name path my/dropbox-path))

(defun my/org-expand (path)
  "Expand PATH relative to `org-directory'."
  (expand-file-name path org-directory))

;; ============================================================
;; Tests: my/shell-quote
;; ============================================================

(ert-deftest test-custom/shell-quote-simple ()
  "Simple string is wrapped in single quotes."
  (should (equal (my/shell-quote "hello") "'hello'")))

(ert-deftest test-custom/shell-quote-empty ()
  "Empty string produces empty single-quoted string."
  (should (equal (my/shell-quote "") "''")))

(ert-deftest test-custom/shell-quote-with-spaces ()
  "Spaces are preserved inside quotes."
  (should (equal (my/shell-quote "hello world") "'hello world'")))

(ert-deftest test-custom/shell-quote-with-single-quote ()
  "Embedded single quote is properly escaped."
  (should (equal (my/shell-quote "it's") "'it'\"'\"'s'")))

(ert-deftest test-custom/shell-quote-multiple-single-quotes ()
  "Multiple single quotes are each escaped."
  (let ((result (my/shell-quote "a'b'c")))
    (should (equal result "'a'\"'\"'b'\"'\"'c'"))))

(ert-deftest test-custom/shell-quote-double-quotes-unchanged ()
  "Double quotes need no escaping in single-quoted context."
  (should (equal (my/shell-quote "say \"hi\"") "'say \"hi\"'")))

(ert-deftest test-custom/shell-quote-special-chars ()
  "Shell special characters are safely quoted."
  (should (equal (my/shell-quote "$(rm -rf /)") "'$(rm -rf /)'")))

;; ============================================================
;; Tests: func-ignore
;; ============================================================

(ert-deftest test-custom/func-ignore-calls-function ()
  "Returned lambda calls the original function."
  (let ((called nil))
    (funcall (func-ignore (lambda () (setq called t))))
    (should called)))

(ert-deftest test-custom/func-ignore-returns-value ()
  "Returned lambda propagates the return value."
  (should (equal 42 (funcall (func-ignore (lambda () 42))))))

(ert-deftest test-custom/func-ignore-ignores-args ()
  "Extra arguments are ignored without error."
  (let ((called nil))
    (funcall (func-ignore (lambda () (setq called t)))
             :arg1 :arg2 :arg3)
    (should called)))

;; ============================================================
;; Tests: unfill-region
;; ============================================================

(ert-deftest test-custom/unfill-region-joins-lines ()
  "Multi-line paragraph is joined into one line."
  (with-temp-buffer
    (insert "This is a long sentence\nthat spans multiple\nlines.")
    (unfill-region (point-min) (point-max))
    (should (equal (buffer-string)
                   "This is a long sentence that spans multiple lines."))))

(ert-deftest test-custom/unfill-region-preserves-paragraphs ()
  "Separate paragraphs remain separate after unfilling."
  (with-temp-buffer
    (insert "First paragraph\nstill first.\n\nSecond paragraph\nstill second.")
    (unfill-region (point-min) (point-max))
    (let ((content (buffer-string)))
      (should (string-match-p "First paragraph still first\\." content))
      (should (string-match-p "Second paragraph still second\\." content))
      ;; Blank line between paragraphs is preserved
      (should (string-match-p "\n\n" content)))))

(ert-deftest test-custom/unfill-region-empty ()
  "Empty region produces no error."
  (with-temp-buffer
    (unfill-region (point-min) (point-max))
    (should (equal (buffer-string) ""))))

;; ============================================================
;; Tests: platform macros (runtime behavior)
;; ============================================================

(ert-deftest test-custom/when-linux-expands-correctly ()
  "when-linux macro expands to check gnu/linux."
  (let ((expanded (macroexpand-1 '(when-linux (foo) (bar)))))
    (should (equal expanded
                   '(when (eq system-type 'gnu/linux) (foo) (bar))))))

(ert-deftest test-custom/when-windows-expands-correctly ()
  "when-windows macro expands to check windows-nt."
  (let ((expanded (macroexpand-1 '(when-windows (foo)))))
    (should (equal expanded
                   '(when (eq system-type 'windows-nt) (foo))))))

(ert-deftest test-custom/when-mac-expands-correctly ()
  "when-mac macro expands to check darwin."
  (let ((expanded (macroexpand-1 '(when-mac (foo)))))
    (should (equal expanded
                   '(when (eq system-type 'darwin) (foo))))))

(ert-deftest test-custom/when-linux-executes-on-linux ()
  "when-linux body runs when system-type is gnu/linux."
  (let ((system-type 'gnu/linux))
    (should (equal (eval '(when-linux 42) t) 42))))

(ert-deftest test-custom/when-linux-skips-on-other ()
  "when-linux body does NOT run on non-linux systems."
  (let ((system-type 'windows-nt))
    (should-not (eval '(when-linux 42) t))))

(ert-deftest test-custom/when-windows-executes-on-windows ()
  "when-windows body runs when system-type is windows-nt."
  (let ((system-type 'windows-nt))
    (should (equal (eval '(when-windows 42) t) 42))))

(ert-deftest test-custom/when-mac-executes-on-mac ()
  "when-mac body runs when system-type is darwin."
  (let ((system-type 'darwin))
    (should (equal (eval '(when-mac 42) t) 42))))

;; ============================================================
;; Tests: my/dropbox-expand
;; ============================================================

(ert-deftest test-custom/dropbox-expand-relative ()
  "Relative path is expanded under dropbox directory."
  (let ((my/dropbox-path "/home/user/Dropbox"))
    (should (equal (my/dropbox-expand "notes")
                   "/home/user/Dropbox/notes"))))

(ert-deftest test-custom/dropbox-expand-nested ()
  "Nested relative path is expanded correctly."
  (let ((my/dropbox-path "/home/user/Dropbox"))
    (should (equal (my/dropbox-expand "a/b/c.txt")
                   "/home/user/Dropbox/a/b/c.txt"))))

(ert-deftest test-custom/dropbox-expand-absolute-unchanged ()
  "Absolute path overrides the base directory."
  (let ((my/dropbox-path "/home/user/Dropbox"))
    (should (equal (my/dropbox-expand "/tmp/file.txt")
                   "/tmp/file.txt"))))

;; ============================================================
;; Tests: my/org-expand
;; ============================================================

(ert-deftest test-custom/org-expand-relative ()
  "Relative path is expanded under org-directory."
  (let ((org-directory "/home/user/org"))
    (should (equal (my/org-expand "inbox.org")
                   "/home/user/org/inbox.org"))))

(ert-deftest test-custom/org-expand-nested ()
  "Nested path is expanded correctly."
  (let ((org-directory "/home/user/org"))
    (should (equal (my/org-expand "archive/2024.org")
                   "/home/user/org/archive/2024.org"))))

(ert-deftest test-custom/org-expand-absolute-unchanged ()
  "Absolute path overrides org-directory."
  (let ((org-directory "/home/user/org"))
    (should (equal (my/org-expand "/tmp/notes.org")
                   "/tmp/notes.org"))))

(provide 'test-custom-functions)
;;; test-custom-functions.el ends here
