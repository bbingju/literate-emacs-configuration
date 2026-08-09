;;; test-readme-structure.el --- ERT tests for README.org structure -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Tests that README.org is well-formed and its elisp code blocks are
;; syntactically valid.  Does NOT evaluate the blocks (which would
;; require network access and a graphical display), only checks
;; structure and syntax.

;;; Code:

(require 'test-helper)

(defvar my/flutter-bin-dir nil)

(defun test-readme-eval-defun (name)
  "Find and evaluate the defun named NAME in README.org."
  (catch 'found
    (with-temp-buffer
      (insert-file-contents test-readme-file)
      (org-mode)
      (org-element-map (org-element-parse-buffer) 'src-block
        (lambda (block)
          (when (member (org-element-property :language block)
                        '("emacs-lisp" "elisp"))
            (with-temp-buffer
              (insert (org-element-property :value block))
              (goto-char (point-min))
              (condition-case nil
                  (while t
                    (let ((form (read (current-buffer))))
                      (when (and (consp form)
                                 (eq (car form) 'defun)
                                 (eq (cadr form) name))
                        (eval form t)
                        (throw 'found t))))
                (end-of-file nil)))))))
    (ert-fail (format "Cannot find defun %S in README.org" name))))

(ert-deftest test-readme/file-exists ()
  "README.org exists."
  (should (file-exists-p test-readme-file)))

(ert-deftest test-readme/org-parses ()
  "README.org parses as valid org-mode."
  (with-temp-buffer
    (insert-file-contents test-readme-file)
    (org-mode)
    (let ((tree (org-element-parse-buffer)))
      (should tree)
      (should (> (length (org-element-contents tree)) 0)))))

(ert-deftest test-readme/has-expected-headings ()
  "README.org contains expected top-level headings."
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
        (should (member expected headings))))))

(ert-deftest test-readme/source-blocks-exist ()
  "README.org has a reasonable number of elisp source blocks."
  (with-temp-buffer
    (insert-file-contents test-readme-file)
    (org-mode)
    (let ((blocks '()))
      (org-element-map (org-element-parse-buffer) 'src-block
        (lambda (blk)
          (when (member (org-element-property :language blk)
                        '("emacs-lisp" "elisp"))
            (push blk blocks))))
      (should (> (length blocks) 0))
      (should (> (length blocks) 30)))))

(ert-deftest test-readme/source-blocks-syntax ()
  "Every elisp source block is syntactically valid."
  (with-temp-buffer
    (insert-file-contents test-readme-file)
    (org-mode)
    (org-element-map (org-element-parse-buffer) 'src-block
      (lambda (blk)
        (let ((lang (org-element-property :language blk))
              (value (org-element-property :value blk))
              (line (org-element-property :begin blk)))
          (when (member lang '("emacs-lisp" "elisp"))
            ;; Skip blocks under COMMENT headings
            (let ((parent (org-element-property :parent blk))
                  (in-comment nil))
              (while parent
                (when (and (eq (org-element-type parent) 'headline)
                           (org-element-property :commentedp parent))
                  (setq in-comment t))
                (setq parent (org-element-property :parent parent)))
              (unless in-comment
                (condition-case err
                    (with-temp-buffer
                      (insert value)
                      (goto-char (point-min))
                      (while (not (eobp))
                        (read (current-buffer))
                        (skip-chars-forward " \t\n\r")))
                  (end-of-file nil)  ; trailing whitespace is OK
                  (error
                   (ert-fail (format "Syntax error in block at line %d: %s"
                                     line err))))))))))))

(ert-deftest test-readme/no-broken-use-package ()
  "use-package declarations have a valid package name."
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
              (should (string-match-p "(use-package [a-zA-Z]" value))))))
      (should (> count 0)))))

(ert-deftest test-readme/init-el-loads-readme ()
  "init.el properly references README.org."
  (let ((init-file (expand-file-name "init.el" test-project-root)))
    (should (file-exists-p init-file))
    (with-temp-buffer
      (insert-file-contents init-file)
      (let ((content (buffer-string)))
        (should (string-match-p "org-babel-load-file" content))
        (should (string-match-p "README.org" content))))))

(ert-deftest test-readme/platform-macros ()
  "Platform macros are defined in source blocks."
  (with-temp-buffer
    (insert-file-contents test-readme-file)
    (let ((content (buffer-string)))
      (dolist (macro '("when-linux" "when-windows" "when-mac"))
        (should (string-match-p (format "(defmacro %s " macro) content))))))

(ert-deftest test-readme/custom-elisp-modules ()
  "Referenced elisp modules in lisp/ exist."
  (let ((lisp-dir (expand-file-name "lisp" test-project-root)))
    (should (file-directory-p lisp-dir))
    (dolist (module '("fontutil.el" "my-c-ts-mode.el"))
      (should (file-exists-p (expand-file-name module lisp-dir))))))

(ert-deftest test-readme/no-unclosed-blocks ()
  "All source blocks are properly closed (matched begin/end_src)."
  (with-temp-buffer
    (insert-file-contents test-readme-file)
    (let ((opens 0) (closes 0))
      (goto-char (point-min))
      (while (re-search-forward
              "^[ \t]*#\\+[Bb][Ee][Gg][Ii][Nn]_[Ss][Rr][Cc]" nil t)
        (setq opens (1+ opens)))
      (goto-char (point-min))
      (while (re-search-forward
              "^[ \t]*#\\+[Ee][Nn][Dd]_[Ss][Rr][Cc]" nil t)
        (setq closes (1+ closes)))
      (should (= opens closes)))))

(ert-deftest test-readme/lowercase-block-markers ()
  "All source block markers use lowercase (no #+BEGIN_SRC or #+END_SRC).
Blocks under COMMENT headings are excluded."
  (with-temp-buffer
    (insert-file-contents test-readme-file)
    (org-mode)
    (org-element-map (org-element-parse-buffer) 'src-block
      (lambda (blk)
        (let ((parent (org-element-property :parent blk))
              (in-comment nil))
          (while parent
            (when (and (eq (org-element-type parent) 'headline)
                       (org-element-property :commentedp parent))
              (setq in-comment t))
            (setq parent (org-element-property :parent parent)))
          (unless in-comment
            (save-excursion
              (goto-char (org-element-property :begin blk))
              (let ((case-fold-search nil))
                (when (looking-at "^[ \t]*#\\+\\(BEGIN_SRC\\|END_SRC\\)")
                  (ert-fail
                   (format "Uppercase block marker at line %d: %s"
                           (line-number-at-pos) (match-string 0))))))))))))

(ert-deftest test-readme/no-duplicate-use-package ()
  "No package is declared via use-package more than once (outside COMMENT headings)."
  (with-temp-buffer
    (insert-file-contents test-readme-file)
    (org-mode)
    (let ((packages '()))
      (org-element-map (org-element-parse-buffer) 'src-block
        (lambda (blk)
          (let ((lang (org-element-property :language blk))
                (value (org-element-property :value blk)))
            ;; Skip blocks under COMMENT headings
            (let ((parent (org-element-property :parent blk))
                  (in-comment nil))
              (while parent
                (when (and (eq (org-element-type parent) 'headline)
                           (org-element-property :commentedp parent))
                  (setq in-comment t))
                (setq parent (org-element-property :parent parent)))
              (unless in-comment
                (when (member lang '("emacs-lisp" "elisp"))
                  (with-temp-buffer
                    (insert value)
                    (goto-char (point-min))
                    (while (re-search-forward
                            "(use-package \\([a-zA-Z][a-zA-Z0-9_-]*\\)" nil t)
                      (push (match-string 1) packages)))))))))
      (let ((seen '()))
        (dolist (pkg packages)
          (when (member pkg seen)
            (ert-fail (format "Duplicate use-package declaration: %s" pkg)))
          (push pkg seen))))))

(ert-deftest test-readme/no-hardcoded-emacs-d-path ()
  "No elisp block contains hardcoded ~/.emacs.d/ path (use user-emacs-directory)."
  (with-temp-buffer
    (insert-file-contents test-readme-file)
    (org-mode)
    (org-element-map (org-element-parse-buffer) 'src-block
      (lambda (blk)
        (let ((lang (org-element-property :language blk))
              (value (org-element-property :value blk))
              (line (org-element-property :begin blk)))
          ;; Skip blocks under COMMENT headings
          (let ((parent (org-element-property :parent blk))
                (in-comment nil))
            (while parent
              (when (and (eq (org-element-type parent) 'headline)
                         (org-element-property :commentedp parent))
                (setq in-comment t))
              (setq parent (org-element-property :parent parent)))
            (unless in-comment
              (when (and (member lang '("emacs-lisp" "elisp"))
                         (string-match-p "\"~/\\.emacs\\.d/" value))
                (ert-fail
                 (format "Hardcoded ~/.emacs.d/ path in block at line %d; use user-emacs-directory"
                         line))))))))))

(ert-deftest test-readme/redmine-agenda-uses-custom-file ()
  "Redmine agenda configuration uses the module's customizable file path."
  (with-temp-buffer
    (insert-file-contents test-readme-file)
    (let ((content (buffer-string)))
      (should-not
       (string-match-p "(my/org-expand \"redmine\\.org\")" content))
      (should
       (string-match-p "(file-exists-p redmine-org-file)" content))
      (should
       (string-match-p "(list redmine-org-file)" content)))))

(ert-deftest test-readme/remote-development-avoids-local-only-settings ()
  "Remote development does not inherit costly or local-only settings."
  (with-temp-buffer
    (insert-file-contents test-readme-file)
    (let ((content (buffer-string)))
      (dolist (expected
               '("auto-revert-check-vc-info nil"
                 "auto-revert-remote-files nil"
                 "global-auto-revert-non-file-buffers nil"
                 "diff-hl-disable-on-remote t"
                 "tramp-own-remote-path"
                 "my/eglot-jdtls-contact"
                 "my/eglot-dart-contact"
                 "my/eglot-rust-analyzer-contact"))
        (should (string-match-p (regexp-quote expected) content)))
      (should-not
       (string-match-p (regexp-quote "(setq auto-revert-interval 1") content))
      (should-not
       (string-match-p (regexp-quote "tramp-remote-shell-executable") content)))))

(ert-deftest test-readme/eglot-contacts-select-the-project-host ()
  "Eglot contacts use command names remotely and local paths locally."
  (dolist (function '(my/eglot-remote-project-p
                      my/eglot-jdtls-contact
                      my/eglot-dart-contact
                      my/eglot-rust-analyzer-contact))
    (test-readme-eval-defun function))
  (cl-letf (((symbol-function 'project-root) #'identity)
            ((symbol-function 'file-remote-p)
             (lambda (file &rest _args)
               (and (string-prefix-p "/ssh:" file) "/ssh:host:")))
            ((symbol-function 'my/find-jdtls-installation)
             (lambda () '("java" "-jar" "/local/jdtls.jar")))
            ((symbol-function 'executable-find)
             (lambda (&rest _args) "/local/bin/rust-analyzer")))
    (let ((remote "/ssh:host:/repo/")
          (local "/local/repo/")
          (my/flutter-bin-dir "/local/flutter/bin"))
      (should (equal (my/eglot-jdtls-contact nil remote) '("jdtls")))
      (should (equal (my/eglot-jdtls-contact nil local)
                     '("java" "-jar" "/local/jdtls.jar")))
      (should (equal (car (my/eglot-dart-contact nil remote)) "dart"))
      (should (equal (car (my/eglot-dart-contact nil local))
                     "/local/flutter/bin/dart"))
      (should (equal (car (my/eglot-rust-analyzer-contact nil remote))
                     "rust-analyzer"))
      (should (equal (car (my/eglot-rust-analyzer-contact nil local))
                     "/local/bin/rust-analyzer")))))

(ert-deftest test-readme/compat-31-missing-installs-legacy-fallback ()
  "A missing compat-31 library leaves old Emacs time formatting usable."
  (dolist (function '(my/seconds-to-string-filter-legacy-args
                      my/ensure-compat-31))
    (test-readme-eval-defun function))
  (let (advice required)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (push feature required)
                 (not (eq feature 'compat-31))))
              ((symbol-function 'seconds-to-string)
               (lambda (_delay) "legacy"))
              ((symbol-function 'advice-add)
               (lambda (symbol where function &rest _properties)
                 (setq advice (list symbol where function)))))
      (my/ensure-compat-31))
    (should (equal (nreverse required) '(compat-31 time-date)))
    (should (equal advice
                   '(seconds-to-string
                     :filter-args
                     my/seconds-to-string-filter-legacy-args)))
    (should (equal
             (funcall (nth 2 advice) '(1 expanded abbrev))
             '(1)))))

(provide 'test-readme-structure)
;;; test-readme-structure.el ends here
