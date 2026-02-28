;;; test-readme-structure.el --- ERT tests for README.org structure -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Tests that README.org is well-formed and its elisp code blocks are
;; syntactically valid.  Does NOT evaluate the blocks (which would
;; require network access and a graphical display), only checks
;; structure and syntax.

;;; Code:

(require 'test-helper)

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

(provide 'test-readme-structure)
;;; test-readme-structure.el ends here
