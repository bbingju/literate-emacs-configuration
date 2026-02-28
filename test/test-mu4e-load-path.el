;;; test-mu4e-load-path.el --- ERT tests for my/find-mu4e-load-path -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Tests for the mu4e load-path auto-detection function.
;; Uses cl-letf to mock executable-find and file-expand-wildcards.

;;; Code:

(require 'test-helper)
(require 'cl-lib)

;; ---------- Define function under test ----------

(defun my/find-mu4e-load-path ()
  "Find mu4e elisp directory based on mu binary location."
  (when-let* ((mu-bin (executable-find "mu"))
              (mu-prefix (file-name-directory
                          (directory-file-name
                           (file-name-directory mu-bin)))))
    (car (file-expand-wildcards
          (expand-file-name "share/emacs/site-lisp/*/mu4e*" mu-prefix)))))

;; ============================================================
;; Tests: my/find-mu4e-load-path
;; ============================================================

(ert-deftest test-mu4e/load-path-mu-not-installed ()
  "Return nil when mu binary is not found."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_cmd) nil)))
    (should-not (my/find-mu4e-load-path))))

(ert-deftest test-mu4e/load-path-mu4e-dir-found ()
  "Return mu4e path when mu is installed and mu4e directory exists."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_cmd) "/usr/bin/mu"))
            ((symbol-function 'file-expand-wildcards)
             (lambda (_pattern)
               '("/usr/share/emacs/site-lisp/elpa-src/mu4e-1.12.0"))))
    (should (equal (my/find-mu4e-load-path)
                   "/usr/share/emacs/site-lisp/elpa-src/mu4e-1.12.0"))))

(ert-deftest test-mu4e/load-path-mu4e-dir-not-found ()
  "Return nil when mu is installed but mu4e elisp directory is missing."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_cmd) "/usr/local/bin/mu"))
            ((symbol-function 'file-expand-wildcards)
             (lambda (_pattern) nil)))
    (should-not (my/find-mu4e-load-path))))

(provide 'test-mu4e-load-path)
;;; test-mu4e-load-path.el ends here
