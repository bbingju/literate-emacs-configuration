;;; test-c-ts-mode.el --- ERT tests for my-c-ts-mode.el -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Tests for custom C/C++ tree-sitter mode functions.
;; Uses cl-letf to mock external dependencies.

;;; Code:

(require 'test-helper)
(require 'cl-lib)

(load (expand-file-name "lisp/my-c-ts-mode.el" test-project-root) nil t)

(defmacro test-c-ts/with-clang-format (content &rest body)
  "Write CONTENT to a temp .clang-format file bound as `it', run BODY."
  (declare (indent 1))
  `(let ((it (make-temp-file "clang-format-")))
     (unwind-protect
         (progn
           (with-temp-file it (insert ,content))
           ,@body)
       (delete-file it))))

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

;; ============================================================
;; Tests: my/clang-format-parse-config
;; ============================================================

(ert-deftest test-c-ts/parse-all-fields ()
  "Parser captures all four recognised top-level keys."
  (test-c-ts/with-clang-format
      "---\nLanguage: Cpp\nTabWidth: 4\nIndentWidth: 4\nUseTab: Always\nColumnLimit: 100\n...\n"
    (should (equal (my/clang-format-parse-config it)
                   '((TabWidth    . 4)
                     (IndentWidth . 4)
                     (UseTab      . "Always")
                     (ColumnLimit . 100))))))

(ert-deftest test-c-ts/parse-missing-fields ()
  "Keys not present in the file are absent from the result."
  (test-c-ts/with-clang-format "IndentWidth: 2\n"
    (should (equal (my/clang-format-parse-config it)
                   '((IndentWidth . 2))))))

(ert-deftest test-c-ts/parse-ignores-nested ()
  "Indented (nested) keys must not be captured even if their name matches."
  ;; ColumnLimit nested under a hypothetical parent must be ignored;
  ;; only the top-level TabWidth is captured here.
  (test-c-ts/with-clang-format
      "BraceWrapping:\n  AfterClass: false\n  ColumnLimit: 999\nTabWidth: 8\n"
    (should (equal (my/clang-format-parse-config it)
                   '((TabWidth . 8))))))

(ert-deftest test-c-ts/parse-use-tab-values ()
  "UseTab is returned as a trimmed string (boolean conversion is in the writer)."
  (test-c-ts/with-clang-format "UseTab: Never\n"
    (should (equal (my/clang-format-parse-config it)
                   '((UseTab . "Never")))))
  (test-c-ts/with-clang-format "UseTab:    ForIndentation   \n"
    (should (equal (my/clang-format-parse-config it)
                   '((UseTab . "ForIndentation"))))))

(ert-deftest test-c-ts/call-hierarchy-converts-ranges-with-eglot ()
  "Call hierarchy delegates fallback and call-site ranges to Eglot."
  (let* ((selection '(:start (:line 2 :character 5)
                      :end (:line 2 :character 8)))
         (call-range '(:start (:line 4 :character 7)
                       :end (:line 4 :character 10)))
         (item '(:name "origin" :uri "file:///origin.c"))
         (ranged-target
          (list :name "callee" :uri "file:///callee.c"
                :selectionRange selection))
         (fallback-target
          (list :name "fallback" :uri "file:///fallback.c"
                :selectionRange selection))
         (items (vector item))
         (calls (vector
                 (list :to ranged-target :fromRanges (vector call-range))
                 (list :to fallback-target :fromRanges [])))
         converted
         shown)
    (cl-letf (((symbol-function 'eglot--current-server-or-lose)
               (lambda () 'test-server))
              ((symbol-function 'eglot--TextDocumentPositionParams)
               (lambda () '(:position t)))
              ((symbol-function 'jsonrpc-request)
               (lambda (_server method _params)
                 (pcase method
                   (:textDocument/prepareCallHierarchy items)
                   (:callHierarchy/outgoingCalls calls))))
              ((symbol-function 'eglot--xref-make-match)
               (lambda (name uri range)
                 (let ((result (list name uri range)))
                   (setq converted (append converted (list result)))
                   result)))
              ((symbol-function 'xref-show-xrefs)
               (lambda (fetcher _display-action)
                 (setq shown (funcall fetcher)))))
      (my/eglot--call-hierarchy 'outgoing)
      (should
       (equal converted
              (list (list "callee" "file:///origin.c" call-range)
                    (list "fallback" "file:///fallback.c" selection))))
      (should (equal shown converted)))))

(provide 'test-c-ts-mode)
;;; test-c-ts-mode.el ends here
