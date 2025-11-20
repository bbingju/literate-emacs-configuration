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

(defun my/c-ts-mode-setup ()
  "Custom indentation and tooling for `c-ts-mode'."
  (setq-local indent-tabs-mode t
              tab-width 8
              c-ts-mode-indent-offset 8)
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

(provide 'my-c-ts-mode)
;;; my-c-ts-mode.el ends here
