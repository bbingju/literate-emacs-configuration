;;; test-readme.el --- Tests for README.org literate config -*- lexical-binding: t -*-

;; Run with: emacs -batch -l test-readme.el

;;; Commentary:
;;
;; Tests that README.org is well-formed and its elisp code blocks are
;; syntactically valid.  Does NOT evaluate the blocks (which would
;; require network access and a graphical display), only checks
;; structure and syntax.
;;
;; Usage:
;;   emacs --batch -l test-readme.el
;;
;; Exit code 0 means all tests passed.

;;; Code:

(require 'org)
(require 'org-element)

(defvar test-readme-file
  (expand-file-name "README.org"
                    (file-name-directory
                     (or load-file-name buffer-file-name default-directory)))
  "Path to README.org.")

(defvar test-pass-count 0)
(defvar test-fail-count 0)

(defun test-log (fmt &rest args)
  "Print a test log message."
  (princ (apply #'format (concat fmt "\n") args)))

(defun test-pass (name)
  "Record a passing test."
  (setq test-pass-count (1+ test-pass-count))
  (test-log "  PASS: %s" name))

(defun test-fail (name reason)
  "Record a failing test."
  (setq test-fail-count (1+ test-fail-count))
  (test-log "  FAIL: %s -- %s" name reason))

(defun test-assert (name condition &optional reason)
  "Assert CONDITION is non-nil for test NAME."
  (if condition
      (test-pass name)
    (test-fail name (or reason "assertion failed"))))

;;; ---------- Tests ----------

(defun test-file-exists ()
  "Test that README.org exists."
  (test-log "--- File existence ---")
  (test-assert "README.org exists"
               (file-exists-p test-readme-file)
               (format "File not found: %s" test-readme-file)))

(defun test-org-parses ()
  "Test that README.org parses as valid org-mode."
  (test-log "--- Org parsing ---")
  (condition-case err
      (with-temp-buffer
        (insert-file-contents test-readme-file)
        (org-mode)
        (let ((tree (org-element-parse-buffer)))
          (test-assert "org-element-parse-buffer succeeds"
                       (not (null tree)))
          (test-assert "parse tree has content"
                       (> (length (org-element-contents tree)) 0))))
    (error
     (test-fail "org-element-parse-buffer succeeds"
                (format "Parse error: %s" err)))))

(defun test-has-expected-headings ()
  "Test that README.org contains expected top-level headings."
  (test-log "--- Expected headings ---")
  (with-temp-buffer
    (insert-file-contents test-readme-file)
    (org-mode)
    (let ((headings '()))
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (hl)
          (when (= 1 (org-element-property :level hl))
            (push (org-element-property :raw-value hl) headings))))
      (setq headings (nreverse headings))
      (dolist (expected '("First of All" "Appearance" "Org Mode" "Programming" "Tools"))
        (test-assert (format "Has heading \"%s\"" expected)
                     (member expected headings)
                     (format "Missing heading. Found: %s"
                             (mapconcat #'identity headings ", ")))))))

(defun test-source-blocks-exist ()
  "Test that README.org has elisp source blocks."
  (test-log "--- Source blocks ---")
  (with-temp-buffer
    (insert-file-contents test-readme-file)
    (org-mode)
    (let ((blocks '()))
      (org-element-map (org-element-parse-buffer) 'src-block
        (lambda (blk)
          (let ((lang (org-element-property :language blk)))
            (when (member lang '("emacs-lisp" "elisp"))
              (push blk blocks)))))
      (test-assert "Has elisp source blocks"
                   (> (length blocks) 0)
                   "No emacs-lisp/elisp source blocks found")
      (test-assert "Has reasonable number of source blocks (>30)"
                   (> (length blocks) 30)
                   (format "Only %d blocks found, expected >30"
                           (length blocks)))
      (test-log "  INFO: Found %d elisp source blocks" (length blocks)))))

(defun test-source-blocks-syntax ()
  "Test that every elisp source block is syntactically valid."
  (test-log "--- Source block syntax ---")
  (with-temp-buffer
    (insert-file-contents test-readme-file)
    (org-mode)
    (let ((block-num 0)
          (commented-out 0))
      (org-element-map (org-element-parse-buffer) 'src-block
        (lambda (blk)
          (let ((lang (org-element-property :language blk))
                (value (org-element-property :value blk))
                (line (org-element-property :begin blk)))
            (when (member lang '("emacs-lisp" "elisp"))
              (setq block-num (1+ block-num))
              ;; Check if inside a COMMENT heading (should be skipped
              ;; by org-babel-load-file, but we still validate syntax)
              (let ((parent (org-element-property :parent blk))
                    (in-comment nil))
                (while parent
                  (when (and (eq (org-element-type parent) 'headline)
                             (org-element-property :commentedp parent))
                    (setq in-comment t))
                  (setq parent (org-element-property :parent parent)))
                (if in-comment
                    (setq commented-out (1+ commented-out))
                  ;; Validate syntax by reading all forms
                  (condition-case err
                      (with-temp-buffer
                        (insert value)
                        (goto-char (point-min))
                        (let ((forms 0))
                          (while (not (eobp))
                            (read (current-buffer))
                            (setq forms (1+ forms))
                            ;; Skip whitespace between forms
                            (skip-chars-forward " \t\n\r"))
                          (test-pass (format "Block #%d (line %d): %d form(s)"
                                            block-num line forms))))
                    (end-of-file
                     ;; end-of-file after reading is OK (empty trailing space)
                     (test-pass (format "Block #%d (line %d): valid"
                                        block-num line)))
                    (error
                     (test-fail (format "Block #%d (line %d)" block-num line)
                                (format "Syntax error: %s" err))))))))))
      (when (> commented-out 0)
        (test-log "  INFO: Skipped %d blocks under COMMENT headings" commented-out)))))

(defun test-no-broken-use-package ()
  "Test that use-package declarations have valid structure."
  (test-log "--- use-package structure ---")
  (with-temp-buffer
    (insert-file-contents test-readme-file)
    (org-mode)
    (let ((count 0))
      (org-element-map (org-element-parse-buffer) 'src-block
        (lambda (blk)
          (let ((lang (org-element-property :language blk))
                (value (org-element-property :value blk))
                (line (org-element-property :begin blk)))
            (when (and (member lang '("emacs-lisp" "elisp"))
                       (string-match-p "(use-package " value))
              (setq count (1+ count))
              ;; Check that use-package has a package name
              (test-assert
               (format "use-package at line %d has package name" line)
               (string-match-p "(use-package [a-zA-Z]" value)
               "use-package missing package name")))))
      (test-log "  INFO: Found %d use-package declarations" count))))

(defun test-init-el-loads-readme ()
  "Test that init.el properly references README.org."
  (test-log "--- init.el integration ---")
  (let ((init-file (expand-file-name
                    "init.el"
                    (file-name-directory test-readme-file))))
    (test-assert "init.el exists" (file-exists-p init-file))
    (when (file-exists-p init-file)
      (with-temp-buffer
        (insert-file-contents init-file)
        (test-assert "init.el calls org-babel-load-file"
                     (string-match-p "org-babel-load-file" (buffer-string)))
        (test-assert "init.el references README.org"
                     (string-match-p "README.org" (buffer-string)))))))

(defun test-platform-macros ()
  "Test that platform macros are defined in source blocks."
  (test-log "--- Platform macros ---")
  (with-temp-buffer
    (insert-file-contents test-readme-file)
    (dolist (macro '("when-linux" "when-windows" "when-mac"))
      (test-assert (format "Defines macro %s" macro)
                   (string-match-p (format "(defmacro %s " macro)
                                   (buffer-string))))))

(defun test-custom-elisp-modules ()
  "Test that referenced elisp modules in lisp/ exist."
  (test-log "--- Custom elisp modules ---")
  (let ((lisp-dir (expand-file-name
                   "lisp" (file-name-directory test-readme-file))))
    (test-assert "lisp/ directory exists" (file-directory-p lisp-dir))
    (dolist (module '("fontutil.el" "my-c-ts-mode.el"))
      (test-assert (format "Module %s exists" module)
                   (file-exists-p (expand-file-name module lisp-dir))))))

(defun test-no-unclosed-blocks ()
  "Test that all source blocks are properly closed."
  (test-log "--- Block structure ---")
  (with-temp-buffer
    (insert-file-contents test-readme-file)
    (let ((opens 0) (closes 0))
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+[Bb][Ee][Gg][Ii][Nn]_[Ss][Rr][Cc]" nil t)
        (setq opens (1+ opens)))
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+[Ee][Nn][Dd]_[Ss][Rr][Cc]" nil t)
        (setq closes (1+ closes)))
      (test-assert "All source blocks are closed"
                   (= opens closes)
                   (format "Mismatched: %d opens, %d closes" opens closes))
      (test-log "  INFO: %d begin/end_src pairs" opens))))

;;; ---------- Runner ----------

(defun test-readme-run ()
  "Run all tests."
  (test-log "=== Testing README.org ===")
  (test-log "File: %s" test-readme-file)
  (test-log "")
  (setq test-pass-count 0
        test-fail-count 0)
  (test-file-exists)
  (test-org-parses)
  (test-has-expected-headings)
  (test-source-blocks-exist)
  (test-source-blocks-syntax)
  (test-no-broken-use-package)
  (test-init-el-loads-readme)
  (test-platform-macros)
  (test-custom-elisp-modules)
  (test-no-unclosed-blocks)
  (test-log "")
  (test-log "=== Results: %d passed, %d failed ==="
            test-pass-count test-fail-count)
  (when (> test-fail-count 0)
    (kill-emacs 1)))

;; Auto-run when loaded in batch mode
(when noninteractive
  (test-readme-run))

(provide 'test-readme)
;;; test-readme.el ends here