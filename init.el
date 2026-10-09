;;; init.el --- Emacs init -*- lexical-binding: t; -*-
;;;
;;; Author: Phil Hwang <pjhwang@gmail.com>

;; Silence "missing lexical-binding cookie" warnings from third-party
;; .el files still loaded as source (ob-http, org-bullets, system mu4e, ...).
;; Keep the persistent rule for libraries loaded after startup.  During early
;; startup, `display-warning' queues warnings before consulting that rule, so
;; also inhibit them dynamically while loading the literate configuration.
(require 'warnings)
(add-to-list 'warning-suppress-log-types '(files missing-lexbind-cookie))

(let ((warning-inhibit-types
       (cons '(files missing-lexbind-cookie) warning-inhibit-types)))
  (require 'org)
  (org-babel-load-file (expand-file-name "README.org" user-emacs-directory)))
