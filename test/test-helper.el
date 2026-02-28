;;; test-helper.el --- Common test setup -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Shared setup for all ERT test files.
;; Provides path variables and common requires.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-element)

(defvar test-readme-file
  (expand-file-name "README.org"
                    (file-name-directory
                     (directory-file-name
                      (file-name-directory
                       (or load-file-name buffer-file-name default-directory)))))
  "Path to README.org (one level up from test/).")

(defvar test-project-root
  (file-name-directory test-readme-file)
  "Project root directory.")

(provide 'test-helper)
;;; test-helper.el ends here
