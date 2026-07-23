;;; init.el --- Emacs init -*- lexical-binding: t; -*-
;;;
;;; Author: Phil Hwang <pjhwang@gmail.com>

;; Silence "missing lexical-binding cookie" warnings from third-party
;; .el files still loaded as source (ob-http, org-bullets, system mu4e, ...).
;; Must run before those files are loaded.
(setq warning-suppress-log-types '((files missing-lexbind-cookie)))

(require 'org)
(org-babel-load-file (concat user-emacs-directory "README.org"))
