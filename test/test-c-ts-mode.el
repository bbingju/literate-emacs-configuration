;;; test-c-ts-mode.el --- ERT tests for my-c-ts-mode.el -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Tests for custom C/C++ tree-sitter mode functions.
;; Uses cl-letf to mock external dependencies.

;;; Code:

(require 'test-helper)
(require 'cl-lib)

;; Define functions under test without triggering mode hooks.

(defcustom my/c-ts-mode-format-on-save nil
  "If non-nil, automatically run clang-format on save when possible."
  :type 'boolean
  :group 'my/c-ts)

(defun my/clang-format-available-p ()
  "Return non-nil if clang-format can be used in this buffer."
  (and (featurep 'clang-format)
       (executable-find "clang-format")
       (locate-dominating-file default-directory ".clang-format")))

(defun my/toggle-c-ts-format-on-save ()
  "Toggle automatic clang-format on save for C/C++ buffers."
  (interactive)
  (setq my/c-ts-mode-format-on-save (not my/c-ts-mode-format-on-save))
  (message "clang-format on save %s"
           (if my/c-ts-mode-format-on-save "enabled" "disabled")))

;; ============================================================
;; Tests: my/clang-format-available-p
;; ============================================================

(ert-deftest test-c-ts/clang-format-all-conditions-met ()
  "Returns non-nil when all three conditions are satisfied."
  (cl-letf (((symbol-function 'featurep)
             (lambda (feat) (eq feat 'clang-format)))
            ((symbol-function 'executable-find)
             (lambda (_cmd) "/usr/bin/clang-format"))
            ((symbol-function 'locate-dominating-file)
             (lambda (_dir _file) "/project/")))
    (should (my/clang-format-available-p))))

(ert-deftest test-c-ts/clang-format-no-feature ()
  "Returns nil when clang-format feature is not loaded."
  (cl-letf (((symbol-function 'featurep)
             (lambda (_feat) nil))
            ((symbol-function 'executable-find)
             (lambda (_cmd) "/usr/bin/clang-format"))
            ((symbol-function 'locate-dominating-file)
             (lambda (_dir _file) "/project/")))
    (should-not (my/clang-format-available-p))))

(ert-deftest test-c-ts/clang-format-no-executable ()
  "Returns nil when clang-format binary is not found."
  (cl-letf (((symbol-function 'featurep)
             (lambda (feat) (eq feat 'clang-format)))
            ((symbol-function 'executable-find)
             (lambda (_cmd) nil))
            ((symbol-function 'locate-dominating-file)
             (lambda (_dir _file) "/project/")))
    (should-not (my/clang-format-available-p))))

(ert-deftest test-c-ts/clang-format-no-config-file ()
  "Returns nil when .clang-format file is not found."
  (cl-letf (((symbol-function 'featurep)
             (lambda (feat) (eq feat 'clang-format)))
            ((symbol-function 'executable-find)
             (lambda (_cmd) "/usr/bin/clang-format"))
            ((symbol-function 'locate-dominating-file)
             (lambda (_dir _file) nil)))
    (should-not (my/clang-format-available-p))))

;; ============================================================
;; Tests: my/toggle-c-ts-format-on-save
;; ============================================================

(ert-deftest test-c-ts/toggle-nil-to-t ()
  "Toggling from nil sets variable to t."
  (let ((my/c-ts-mode-format-on-save nil))
    (my/toggle-c-ts-format-on-save)
    (should (eq my/c-ts-mode-format-on-save t))))

(ert-deftest test-c-ts/toggle-t-to-nil ()
  "Toggling from t sets variable to nil."
  (let ((my/c-ts-mode-format-on-save t))
    (my/toggle-c-ts-format-on-save)
    (should (eq my/c-ts-mode-format-on-save nil))))

(ert-deftest test-c-ts/toggle-round-trip ()
  "Double toggle returns to original value."
  (let ((my/c-ts-mode-format-on-save nil))
    (my/toggle-c-ts-format-on-save)
    (my/toggle-c-ts-format-on-save)
    (should (eq my/c-ts-mode-format-on-save nil))))

(provide 'test-c-ts-mode)
;;; test-c-ts-mode.el ends here
