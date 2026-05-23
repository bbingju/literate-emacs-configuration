;;; my-c-ts-mode.el --- Custom C/C++ settings for c-ts-mode -*- lexical-binding: t; -*-
;;; Commentary:
;; Provides unified TAB handling and clang-format integration for C/C++ buffers
;; that use the Tree-Sitter based `c-ts-mode`.
;;
;; Features:
;; • Real TAB characters (indent-tabs-mode = t)
;; • 8-space visual width and indent offset
;; • Smart TAB: if a .clang-format is found in the project and clang-format is
;;   available ⇒ run clang-format-buffer, otherwise fallback to normal indent
;; • Optional on-save formatting via `my/c-ts-mode-format-on-save` (default: nil)
;;   Toggle with `M-x my/toggle-c-ts-format-on-save`
;;
;; NOTE:
;;   The `clang-format` ELPA package binds keys in `c-mode-base-map`, which
;;   lives in the classic CC-mode library.  When using only `c-ts-mode`, CC-mode
;;   isn’t loaded automatically, leading to the error
;;     "Symbol’s value as variable is void: c-mode-base-map".
;;   We pre-load CC-mode here to provide that map and silence the error.
;;
;;   Put `(require 'my-c-ts-mode)` in your init to enable.

;;; Code:

(require 'cc-mode)                      ;; provide c-mode-base-map for clang-format
(require 'subr-x)                       ;; for when-let etc.
(require 'cl-lib)                       ;; for cl-incf
(require 'clang-format nil t)           ;; soft-require, only if installed

(defgroup my/c-ts nil
  "Customisations for c-ts-mode."
  :group 'languages)

(defcustom my/c-ts-mode-format-on-save nil
  "If non-nil, automatically run clang-format on save when possible."
  :type 'boolean
  :group 'my/c-ts)

(defun my/clang-format-available-p ()
  "Return non-nil if clang-format can be used in this buffer."
  (and (featurep 'clang-format)
       (executable-find "clang-format")
       (locate-dominating-file default-directory ".clang-format")))

(defun my/clang-format-buffer-smart ()
  "Run clang-format if available, else fall back to `indent-for-tab-command'."
  (interactive)
  (if (my/clang-format-available-p)
      (let ((clang-format-style-option "file"))
        (clang-format-buffer))
    ;; If eglot is running, use its formatting instead
    (if (and (featurep 'eglot) (eglot-current-server))
        (eglot-format-buffer)
      (indent-for-tab-command))))

(defun my/clang-format-region-smart (start end)
  "Indent region using clang-format / eglot when possible.
Used as `indent-region-function' so that \\[indent-region]
respects `.clang-format' in eglot-managed C/C++ buffers."
  (cond
   ((my/clang-format-available-p)
    (let ((clang-format-style-option "file"))
      (clang-format-region start end)))
   ((and (featurep 'eglot) (eglot-current-server))
    (eglot-format start end))
   (t
    ;; Fall back to the default line-by-line indentation.
    (let ((indent-region-function nil))
      (indent-region start end)))))

(defun my/c-ts-mode-setup ()
  "Custom indentation and tooling for `c-ts-mode'."
  (setq-local indent-tabs-mode t
              tab-width 8
              c-ts-mode-indent-offset 8
              ;; Make C-M-\ (indent-region) honor .clang-format when available.
              indent-region-function #'my/clang-format-region-smart)
  ;; Enable electric-pair-mode as recommended for tree-sitter C/C++ modes
  (electric-pair-local-mode 1)
  ;; Bind TAB to smart clang-format/indent.
  (local-set-key (kbd "TAB") #'my/clang-format-buffer-smart)
  ;; Optional before-save hook.
  (when my/c-ts-mode-format-on-save
    (add-hook 'before-save-hook #'my/clang-format-buffer-smart nil t))
  ;; Additional eglot keybindings for C/C++ (set after eglot connects)
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (derived-mode-p 'c-ts-mode 'c++-ts-mode)
                (local-set-key (kbd "C-c C-r") #'eglot-rename)
                (local-set-key (kbd "C-c C-a") #'eglot-code-actions)
                (local-set-key (kbd "C-c C-d") #'eglot-find-declaration)
                (local-set-key (kbd "C-c C-i") #'eglot-find-implementation)))
            nil t))

(add-hook 'c-ts-mode-hook #'my/c-ts-mode-setup)
(add-hook 'c++-ts-mode-hook #'my/c-ts-mode-setup)

;;;###autoload
(defun my/toggle-c-ts-format-on-save ()
  "Toggle automatic clang-format on save for C/C++ buffers."
  (interactive)
  (setq my/c-ts-mode-format-on-save (not my/c-ts-mode-format-on-save))
  (message "clang-format on save %s"
           (if my/c-ts-mode-format-on-save "enabled" "disabled")))

;; ----------------------------------------------------------------------------
;; .clang-format  →  .dir-locals.el
;; ----------------------------------------------------------------------------

(defconst my/clang-format--dir-locals-mapping
  '((TabWidth    (tab-width)                              integer)
    (IndentWidth (c-basic-offset c-ts-mode-indent-offset) integer)
    (UseTab      (indent-tabs-mode)                       use-tab)
    (ColumnLimit (fill-column)                            integer))
  "Mapping from `.clang-format' keys to Emacs variables.
Each entry is (CF-KEY (EMACS-VAR...) CONVERTER).  CONVERTER is a
symbol naming the value transform applied by
`my/clang-format--convert-value'.")

(defconst my/clang-format--target-modes
  '(c-mode c-ts-mode c++-mode c++-ts-mode)
  "Modes that receive generated dir-local variables.")

(defun my/clang-format-parse-config (file)
  "Parse FILE (a .clang-format) and return an alist of recognised values.
Only top-level keys listed in `my/clang-format--dir-locals-mapping'
are captured; nested entries (lines starting with whitespace) are
ignored, so blocks like `BraceWrapping:' do not pollute the result.
Integer values are converted with `string-to-number'; `UseTab' is
returned as the trimmed string."
  (let ((result nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward
              "^\\(TabWidth\\|IndentWidth\\|UseTab\\|ColumnLimit\\)[[:space:]]*:[[:space:]]*\\([^#\n]+?\\)[[:space:]]*$"
              nil t)
        (let* ((key (intern (match-string 1)))
               (raw (match-string 2))
               (val (if (memq key '(TabWidth IndentWidth ColumnLimit))
                        (string-to-number raw)
                      raw)))
          (push (cons key val) result))))
    (nreverse result)))

(defun my/clang-format--convert-value (converter raw)
  "Apply CONVERTER tag to RAW value from a parsed .clang-format."
  (pcase converter
    ('integer raw)
    ('use-tab (not (equal raw "Never")))
    (_ raw)))

;;;###autoload
(defun my/clang-format-write-dir-locals ()
  "Generate or merge `.dir-locals.el' from the project's `.clang-format'.
Searches upward from `default-directory' for `.clang-format', maps
TabWidth/IndentWidth/UseTab/ColumnLimit to the corresponding Emacs
variables, and writes them under each of `c-mode', `c-ts-mode',
`c++-mode', `c++-ts-mode' using `add-dir-local-variable', which
preserves unrelated existing entries.  `BasedOnStyle' inheritance is
not resolved."
  (interactive)
  (let* ((root (locate-dominating-file default-directory ".clang-format"))
         (_    (unless root
                 (user-error "No .clang-format found in any parent directory")))
         (cf   (expand-file-name ".clang-format" root))
         (parsed (my/clang-format-parse-config cf))
         (dir-locals-file-path (expand-file-name ".dir-locals.el" root))
         (count 0))
    (let ((default-directory root))
      (dolist (entry my/clang-format--dir-locals-mapping)
        (let* ((cf-key    (nth 0 entry))
               (emacs-vars (nth 1 entry))
               (converter  (nth 2 entry))
               (cell       (assq cf-key parsed)))
          (when cell
            (let ((value (my/clang-format--convert-value converter (cdr cell))))
              (dolist (var emacs-vars)
                (dolist (mode my/clang-format--target-modes)
                  (add-dir-local-variable mode var value)
                  (cl-incf count))))))))
    (let ((buf (find-buffer-visiting dir-locals-file-path)))
      (when buf
        (with-current-buffer buf
          (save-buffer))))
    (message "Wrote %d entries to %s" count dir-locals-file-path)))

(provide 'my-c-ts-mode)
;;; my-c-ts-mode.el ends here
