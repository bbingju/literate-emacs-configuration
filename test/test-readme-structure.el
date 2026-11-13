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
(defvar markdown-preview-file-name nil)
(defvar markdown-css-paths nil)
(defvar markdown-xhtml-header-content nil)
(defvar my/markdown-styles nil)
(defvar my/markdown-style-directory nil)
(defvar my/markdown-style nil)
(defvar my/markdown-layout-css nil)
(defvar my/markdown-mermaid-css nil)
(defvar my/markdown-mermaid-js-url nil)
(defvar my/markdown-mermaid-js-integrity nil)
(defvar my/markdown-mermaid-js nil)
(defvar my/markdown-preview-uuid-js nil)
(defvar markdown-preview-stylesheets nil)
(defvar markdown-preview-script-oninit nil)
(defvar markdown-preview--preview-buffers nil)

(defun test-readme-eval-defun (name)
  "Find and evaluate the defun or defconst named NAME in README.org."
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
                                 (memq (car form) '(defun defconst))
                                 (eq (cadr form) name))
                        (eval form t)
                        (throw 'found t))))
                (end-of-file nil)))))))
    (ert-fail (format "Cannot find definition %S in README.org" name))))

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

(ert-deftest test-readme/source-blocks-macroexpand ()
  "Macroexpand every active elisp block, including use-package declarations."
  (require 'use-package)
  (require 'macroexp)
  (with-temp-buffer
    (insert-file-contents test-readme-file)
    (org-mode)
    (org-element-map (org-element-parse-buffer) 'src-block
      (lambda (blk)
        (when (member (org-element-property :language blk)
                      '("emacs-lisp" "elisp"))
          (let ((parent (org-element-property :parent blk))
                (in-comment nil)
                (line (line-number-at-pos
                       (org-element-property :begin blk))))
            (while parent
              (when (and (eq (org-element-type parent) 'headline)
                         (org-element-property :commentedp parent))
                (setq in-comment t))
              (setq parent (org-element-property :parent parent)))
            (unless in-comment
              (with-temp-buffer
                (insert (org-element-property :value blk))
                (goto-char (point-min))
                (condition-case err
                    (while t
                      (macroexpand-all (read (current-buffer))))
                  (end-of-file nil)
                  (error
                   (ert-fail
                    (format "Macroexpansion failed near README.org:%d: %s"
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

(ert-deftest test-readme/init-defines-warning-options-before-updating-them ()
  "init.el defines warning options and inhibits startup warning queuing."
  (let ((init-file (expand-file-name "init.el" test-project-root)))
    (with-temp-buffer
      (insert-file-contents init-file)
      (let ((require-pos (progn
                           (goto-char (point-min))
                           (search-forward "(require 'warnings)" nil t)))
            (update-pos (progn
                          (goto-char (point-min))
                          (search-forward "(add-to-list 'warning-suppress-log-types"
                                          nil t)))
            (inhibit-pos (progn
                           (goto-char (point-min))
                           (search-forward "(let ((warning-inhibit-types"
                                           nil t)))
            (org-pos (progn
                       (goto-char (point-min))
                       (search-forward "(require 'org)" nil t))))
        (should require-pos)
        (should update-pos)
        (should inhibit-pos)
        (should org-pos)
        (should (< require-pos update-pos inhibit-pos org-pos))))))

(ert-deftest test-readme/startup-warning-inhibition-prevents-delay ()
  "The startup inhibition rule prevents lexical warnings from being queued."
  (require 'warnings)
  (let ((after-init-time nil)
        (noninteractive nil)
        (delayed-warnings-list nil)
        (warning-inhibit-types '((files missing-lexbind-cookie))))
    (display-warning
     '(files missing-lexbind-cookie "/tmp/third-party.el")
     "Suppression probe")
    (should-not delayed-warnings-list)))

(ert-deftest test-readme/custom-file-preserves-warning-suppression ()
  "README.org restores the general warning rule after loading Custom."
  (with-temp-buffer
    (insert-file-contents test-readme-file)
    (let ((load-pos (progn
                      (goto-char (point-min))
                      (search-forward "(load custom-file)" nil t)))
          (update-pos (progn
                        (goto-char (point-min))
                        (search-forward
                         "(add-to-list 'warning-suppress-log-types '(files missing-lexbind-cookie))"
                         nil t))))
      (should load-pos)
      (should update-pos)
      (should (< load-pos update-pos)))))

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
  "Redmine agenda uses its custom file and hides completed planning items."
  (with-temp-buffer
    (insert-file-contents test-readme-file)
    (let ((content (buffer-string)))
      (should-not
       (string-match-p "(my/org-expand \"redmine\\.org\")" content))
      (should
       (string-match-p "(file-exists-p redmine-org-file)" content))
      (should
       (string-match-p "(list redmine-org-file)" content))
      (dolist (setting '("org-agenda-skip-deadline-if-done t"
                         "org-agenda-skip-scheduled-if-done t"
                         "org-agenda-skip-timestamp-if-done t"))
        (should (string-match-p setting content))))))

(ert-deftest test-readme/agenda-separates-work-and-personal-files ()
  "Work and personal agenda commands use separate file lists."
  (with-temp-buffer
    (insert-file-contents test-readme-file)
    (let ((content (buffer-string)))
      (dolist (expected
               '("my/org-work-agenda-files"
                 "my/org-personal-agenda-files"
                 "(my/org-expand \"personal-projects.org\")"
                 "(setq org-agenda-files (copy-sequence my/org-work-agenda-files))"
                 "(\"p\" \"Personal agenda\""
                 "(org-agenda-files my/org-personal-agenda-files)"))
        (should (string-match-p (regexp-quote expected) content))))))

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

(ert-deftest test-readme/rust-format-hook-waits-for-eglot ()
  "Rust save formatting is installed by eglot-managed-mode-hook."
  (test-readme-eval-defun 'my/rust-eglot-managed-setup)
  (with-temp-buffer
    (setq major-mode 'rust-ts-mode)
    (my/rust-eglot-managed-setup)
    (should (memq #'my/rust-format-buffer-if-managed before-save-hook))))

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

(defmacro test-readme-with-markdown-styles (&rest body)
  "Run BODY with two built-in Markdown styles and an empty style directory.
The directory is bound to `style-dir' and deleted afterwards."
  (declare (indent 0))
  `(let* ((style-dir (file-name-as-directory
                      (make-temp-file "test-markdown-styles-" t)))
          (my/markdown-style-directory style-dir)
          (my/markdown-style "github")
          (my/markdown-layout-css ".markdown-body{max-width:980px}")
          (my/markdown-mermaid-css ".mermaid-diagram{}")
          (my/markdown-styles
           '(("github" :css "https://cdn.example/github.css"
              :layout t :scheme auto :page-css "body{}")
             ("water" :css "https://cdn.example/water.css" :scheme light))))
     (dolist (name '(my/markdown-style-alist my/markdown-current-style
                     my/markdown-page-css my/markdown-scheme-js))
       (test-readme-eval-defun name))
     (unwind-protect (progn ,@body)
       (delete-directory style-dir t))))

(ert-deftest test-readme/markdown-local-css-extends-styles ()
  "Local CSS replaces the same-named built-in style and adds new ones."
  (test-readme-with-markdown-styles
    (should (equal (mapcar #'car (my/markdown-style-alist)) '("github" "water")))
    (dolist (name '("github" "paper"))
      (write-region "body{}" nil (expand-file-name (concat name ".css") style-dir)))
    (let ((styles (my/markdown-style-alist)))
      (should (equal (mapcar #'car styles) '("github" "water" "paper")))
      (should (equal (cdr (assoc "github" styles))
                     (list :css (expand-file-name "github.css" style-dir)
                           :layout t :scheme 'auto :page-css "body{}")))
      (should (equal (cdr (assoc "paper" styles))
                     (list :css (expand-file-name "paper.css" style-dir)
                           :layout t :scheme 'auto)))
      ;; The built-in definition itself is left untouched.
      (should (equal (plist-get (cdr (assoc "github" my/markdown-styles)) :css)
                     "https://cdn.example/github.css")))))

(ert-deftest test-readme/markdown-style-falls-back-to-first ()
  "An unknown saved style name falls back to the first style."
  (test-readme-with-markdown-styles
    (let ((my/markdown-style "water"))
      (should (equal (plist-get (my/markdown-current-style) :css)
                     "https://cdn.example/water.css")))
    (let ((my/markdown-style "removed-local-style"))
      (should (equal (plist-get (my/markdown-current-style) :css)
                     "https://cdn.example/github.css")))))

(ert-deftest test-readme/markdown-page-css-follows-style ()
  "Only layout styles get the GitHub layout; the scheme reaches Mermaid."
  (test-readme-with-markdown-styles
    (let ((github (cdr (assoc "github" my/markdown-styles)))
          (water (cdr (assoc "water" my/markdown-styles))))
      (should (equal (my/markdown-page-css github)
                     "<style>.markdown-body{max-width:980px}body{}.mermaid-diagram{}</style>"))
      (should (equal (my/markdown-page-css water)
                     "<style>.mermaid-diagram{}</style>"))
      (should (equal (my/markdown-scheme-js water)
                     "var mdColorScheme = 'light';")))))

(ert-deftest test-readme/markdown-apply-style-sets-export ()
  "Applying a style updates the stylesheet and head of exported HTML."
  (test-readme-with-markdown-styles
    (dolist (name '(my/markdown-mermaid-script-tag
                    my/markdown-xhtml-header-content my/markdown-apply-style))
      (test-readme-eval-defun name))
    (let ((my/markdown-style "water")
          (my/markdown-mermaid-js-url "https://cdn.example/mermaid.min.js")
          (my/markdown-mermaid-js-integrity "sha384-test")
          (my/markdown-mermaid-js "")
          (markdown-css-paths nil)
          (markdown-xhtml-header-content nil))
      (cl-letf (((symbol-function 'featurep)
                 (lambda (feature &rest _)
                   (not (eq feature 'markdown-preview-mode)))))
        (should-not (my/markdown-apply-style)))
      (should (equal markdown-css-paths '("https://cdn.example/water.css")))
      (should (string-prefix-p "<style>.mermaid-diagram{}</style>"
                               markdown-xhtml-header-content))
      (should (string-match-p (regexp-quote "var mdColorScheme = 'light';")
                              markdown-xhtml-header-content)))))

(ert-deftest test-readme/markdown-preview-inlines-local-css ()
  "Live preview links remote stylesheets but inlines local ones."
  (test-readme-eval-defun 'my/markdown-preview-stylesheet)
  (should (equal (my/markdown-preview-stylesheet '(:css "https://cdn.example/a.css"))
                 "https://cdn.example/a.css"))
  (let ((file (make-temp-file "test-markdown-style-" nil ".css" "h1{color:red}")))
    (unwind-protect
        (should (equal (my/markdown-preview-stylesheet (list :css file))
                       "<style>\nh1{color:red}\n</style>"))
      (delete-file file))))

(ert-deftest test-readme/markdown-preview-embeds-css-resources ()
  "Inlined local CSS carries its relative fonts, images and imports."
  (dolist (name '(my/markdown-css-resource-types my/markdown-css-reference-regexp
                  my/markdown-css-data-url my/markdown-css-embed))
    (test-readme-eval-defun name))
  (let ((dir (file-name-as-directory (make-temp-file "test-markdown-css-" t)))
        (png (unibyte-string #x89 ?P ?N ?G 0)))
    (unwind-protect
        (let ((coding-system-for-write 'utf-8-unix))
          (make-directory (expand-file-name "fonts" dir))
          (write-region "FONT" nil (expand-file-name "fonts/a.woff2" dir))
          (let ((coding-system-for-write 'no-conversion))
            (write-region png nil (expand-file-name "b.png" dir)))
          (write-region "/* 제목 */h2{background:url(b.png)}" nil
                        (expand-file-name "theme.css" dir))
          (write-region
           (concat "@import \"theme.css\";\n"
                   "@font-face{src:url( 'fonts/a.woff2?v=1#f' )}\n"
                   "a{background:url(https://cdn.example/x.png)}\n"
                   "b{mask:url(#m)}c{background:url(/abs.png)}\n"
                   "d{background:url(missing.png)}\n"
                   "e{background:url(data:image/png;base64,AA==)}\n"
                   ;; An import cycle is left alone.
                   "@import 'main.css';\n")
           nil (expand-file-name "main.css" dir))
          (should
           (equal
            (my/markdown-css-embed (expand-file-name "main.css" dir))
            (concat
             "@import \"data:text/css;charset=utf-8;base64,"
             (base64-encode-string
              (encode-coding-string
               (concat "/* 제목 */h2{background:url(data:image/png;base64,"
                       (base64-encode-string png t) ")}")
               'utf-8)
              t)
             "\";\n"
             "@font-face{src:url( 'data:font/woff2;base64,"
             (base64-encode-string "FONT" t) "#f' )}\n"
             "a{background:url(https://cdn.example/x.png)}\n"
             "b{mask:url(#m)}c{background:url(/abs.png)}\n"
             "d{background:url(missing.png)}\n"
             "e{background:url(data:image/png;base64,AA==)}\n"
             "@import 'main.css';\n"))))
      (delete-directory dir t))))

(ert-deftest test-readme/markdown-preview-keeps-css-escapes ()
  "CSS and JavaScript backslashes survive the preview template expansion."
  (test-readme-with-markdown-styles
    (dolist (name '(my/markdown-css-resource-types my/markdown-css-reference-regexp
                    my/markdown-css-data-url my/markdown-css-embed
                    my/markdown-preview-stylesheet my/markdown-preview-template-quote
                    my/markdown-preview-apply-style))
      (test-readme-eval-defun name))
    (let ((css ".hover\\:red{color:red}h1::before{content:\"\\2192\"}")
          (file (expand-file-name "paper.css" style-dir))
          (markdown-preview--preview-buffers (make-hash-table :test 'equal))
          (my/markdown-preview-uuid-js "")
          (my/markdown-mermaid-js "s.replace(/\\s+/g, ' ');")
          (markdown-preview-stylesheets nil)
          (markdown-preview-script-oninit nil))
      (write-region css nil file)
      (should-not (my/markdown-preview-apply-style (list :css file)))
      (cl-flet ((expand (value)
                  ;; `markdown-preview--read-preview-template' fills each
                  ;; placeholder with this non-literal `replace-match'.
                  (with-temp-buffer
                    (insert "${MD_STYLE}")
                    (goto-char (point-min))
                    (search-forward "${MD_STYLE}")
                    (replace-match value t)
                    (buffer-string))))
        (should (equal (expand (car markdown-preview-stylesheets))
                       (concat "<style>\n" css "\n</style>")))
        (should (string-suffix-p "s.replace(/\\s+/g, ' ');"
                                 (expand markdown-preview-script-oninit)))))))

(ert-deftest test-readme/markdown-preview-deletes-only-temp-page ()
  "The exit hook deletes an absolute preview page but not a relative one."
  (test-readme-eval-defun 'my/markdown-preview-delete-page)
  (let ((markdown-preview-file-name
         (make-temp-file "test-markdown-preview-" nil ".html")))
    (unwind-protect
        (progn
          (my/markdown-preview-delete-page)
          (should-not (file-exists-p markdown-preview-file-name)))
      (ignore-errors (delete-file markdown-preview-file-name))))
  (let ((default-directory
         (file-name-as-directory
          (make-temp-file "test-markdown-preview-dir-" t))))
    (unwind-protect
        (let ((markdown-preview-file-name ".markdown-preview.html"))
          (write-region "" nil markdown-preview-file-name)
          (my/markdown-preview-delete-page)
          (should (file-exists-p markdown-preview-file-name)))
      (delete-directory default-directory t))))

(ert-deftest test-readme/markdown-export-renders-mermaid ()
  "Exported HTML loads the pinned Mermaid build and renders on page load."
  (dolist (name '(my/markdown-page-css my/markdown-scheme-js
                  my/markdown-mermaid-script-tag my/markdown-xhtml-header-content))
    (test-readme-eval-defun name))
  (let* ((my/markdown-layout-css "")
         (my/markdown-mermaid-css "")
         (my/markdown-mermaid-js-url "https://cdn.example/mermaid.min.js")
         (my/markdown-mermaid-js-integrity "sha384-test")
         (my/markdown-mermaid-js "function mdMermaidRender(root) {}")
         (header (my/markdown-xhtml-header-content '(:scheme dark))))
    (should (string-prefix-p "<style></style>" header))
    (should (string-match-p
             (regexp-quote
              (concat "<script src=\"https://cdn.example/mermaid.min.js\""
                      " integrity=\"sha384-test\" crossorigin=\"anonymous\">"))
             header))
    (should (string-match-p
             (regexp-quote
              "var mdColorScheme = 'dark';function mdMermaidRender(root) {}")
             header))
    (should (string-match-p
             (regexp-quote "mdMermaidRender(document.body)") header))))

(ert-deftest test-readme/markdown-live-preview-renders-mermaid ()
  "The live browser preview loads Mermaid and renders after each update."
  (with-temp-buffer
    (insert-file-contents test-readme-file)
    (let ((content (buffer-string)))
      (dolist (expected
               '("mermaid@12.2.0/dist/mermaid.min.js"
                 "markdown-preview-javascript (list (my/markdown-mermaid-script-tag))"
                 "my/markdown-preview-uuid-js my/markdown-mermaid-js))"
                 "(my/markdown-preview-apply-style (my/markdown-current-style))"
                 "mdMermaidRender(document.getElementById('markdown-body'));"))
        (should (string-match-p (regexp-quote expected) content))))))

(ert-deftest test-readme/markdown-preview-tabs-register-own-uuid ()
  "Preview tabs register with the UUID in their URL, not the shared page's."
  (with-temp-buffer
    (insert-file-contents test-readme-file)
    (let ((content (buffer-string)))
      (dolist (expected
               '("new URLSearchParams(location.search).get('uuid')"
                 "var prefix = 'MDPM-Register-UUID: ';"))
        (should (string-match-p (regexp-quote expected) content))))))

(provide 'test-readme-structure)
;;; test-readme-structure.el ends here
