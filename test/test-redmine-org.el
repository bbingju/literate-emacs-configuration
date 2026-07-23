;;; test-redmine-org.el --- ERT tests for redmine-org.el -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Tests Redmine synchronization behavior without making network requests.

;;; Code:

(require 'test-helper)
(require 'cl-lib)

;; The package is not on the batch test load-path.  The module only needs its
;; feature at load time; individual API functions are mocked below.
(unless (featurep 'elmine)
  (provide 'elmine))

(load (expand-file-name "lisp/redmine-org.el" test-project-root) nil t)

(ert-deftest test-redmine-org/api-key-selects-configured-login ()
  "Auth lookup selects the documented API-key login for the Redmine host."
  (let ((redmine-org-auth-host "redmine.example.test")
        (redmine-org-auth-user "apikey")
        captured-args)
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest args)
                 (setq captured-args args)
                 (list (list :secret (lambda () "test-key"))))))
      (should (equal (redmine-org--api-key) "test-key"))
      (should (equal (plist-get captured-args :host)
                     "redmine.example.test"))
      (should (equal (plist-get captured-args :user) "apikey"))
      (should (equal (plist-get captured-args :require) '(:secret)))
      (should (= (plist-get captured-args :max) 1)))))

(ert-deftest test-redmine-org/fetches-all-assigned-issues ()
  "The default fetch asks elmine to paginate through every assigned issue."
  (let (captured-filters)
    (cl-letf (((symbol-function 'redmine-org--api-key)
               (lambda () "test-key"))
              ((symbol-function 'elmine/get-issues)
               (lambda (&rest filters)
                 (setq captured-filters filters)
                 nil)))
      (redmine-org--fetch-issues)
      (should (equal (plist-get captured-filters :assigned_to_id) "me"))
      (should (eq (plist-get captured-filters :limit) t)))))

(ert-deftest test-redmine-org/fallback-statuses-decode-utf-8 ()
  "Fallback status names are decoded like issue-specific allowed statuses."
  (let ((raw-name (encode-coding-string "신규" 'utf-8)))
    (should-not (multibyte-string-p raw-name))
    (cl-letf (((symbol-function 'redmine-org--api-key)
               (lambda () "test-key"))
              ((symbol-function 'elmine/get-issue)
               (lambda (&rest _args) '(:id 42)))
              ((symbol-function 'elmine/get-issue-statuses)
               (lambda () (list (list :id 1 :name raw-name)))))
      (let ((statuses (redmine-org--allowed-statuses 42)))
        (should (equal statuses '(("신규" . 1))))
        (should (multibyte-string-p (caar statuses)))))))

(ert-deftest test-redmine-org/unknown-status-keeps-keyword ()
  "An unmapped Redmine status updates STATUS but leaves the TODO keyword."
  (let ((redmine-org-status-todo-alist '(("Known" . "NEXT")))
        (redmine-org-default-todo "TODO")
        captured-status
        (todo-called nil))
    (cl-letf (((symbol-function 'org-entry-put)
               (lambda (_pom property value)
                 (when (equal property "STATUS")
                   (setq captured-status value))))
              ((symbol-function 'org-todo)
               (lambda (&rest _) (setq todo-called t))))
      (redmine-org--reflect-status "Custom Status")
      (should (equal captured-status "Custom Status"))
      (should-not todo-called))))

(ert-deftest test-redmine-org/known-status-sets-keyword-without-logging ()
  "A mapped Redmine status sets the org keyword with logging inhibited."
  (let ((redmine-org-status-todo-alist '(("Known" . "NEXT")))
        captured-todo
        logging-inhibited)
    (cl-letf (((symbol-function 'org-entry-put) (lambda (&rest _) nil))
              ((symbol-function 'org-todo)
               (lambda (keyword)
                 (setq captured-todo keyword
                       logging-inhibited org-inhibit-logging))))
      (redmine-org--reflect-status "Known")
      (should (equal captured-todo "NEXT"))
      (should logging-inhibited))))

(provide 'test-redmine-org)
;;; test-redmine-org.el ends here
