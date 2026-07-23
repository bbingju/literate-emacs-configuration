;;; redmine-org.el --- Sync Redmine issues assigned to me into an org file -*- lexical-binding: t; -*-

;; Author: bbingju
;; Keywords: redmine, org, tools
;; Package-Requires: ((emacs "27.1") (elmine "0"))

;;; Commentary:

;; Read-only, one-way sync from Redmine to an org file.  Fetches the
;; issues assigned to the current API user and renders them as an
;; org-mode TODO list so they can be tracked from Emacs (agenda, etc.).
;;
;; The API key is looked up through `auth-source' so it never ends up in
;; a committed file.  Add a line like the following to ~/.authinfo.gpg:
;;
;;     machine dev.roseaudio.kr login apikey password YOUR_REDMINE_API_KEY
;;
;; Then:  M-x redmine-org-sync
;;
;; The target file is fully regenerated on every sync, so do not hand-edit
;; it -- any local changes are overwritten (this is a read-only mirror).

;;; Code:

(require 'elmine)
(require 'org)
(require 'auth-source)
(require 'subr-x)

(defgroup redmine-org nil
  "Sync Redmine issues assigned to me into an org file."
  :group 'tools
  :prefix "redmine-org-")

(defcustom redmine-org-host "https://dev.roseaudio.kr/redmine"
  "Base URL of the Redmine instance (no trailing slash)."
  :type 'string)

(defcustom redmine-org-auth-host "dev.roseaudio.kr"
  "Host key used to look up the API key via `auth-source'.
Matches the `machine' field of the ~/.authinfo(.gpg) entry."
  :type 'string)

(defcustom redmine-org-file (expand-file-name "~/org/redmine.org")
  "Org file the fetched issues are written to."
  :type 'file)

(defcustom redmine-org-assigned-to "me"
  "Value for the Redmine `assigned_to_id' filter.
The special value \"me\" means the user owning the API key."
  :type 'string)

(defcustom redmine-org-extra-filters '(:status_id "open" :limit 100)
  "Extra plist filters passed to `elmine/get-issues'.
See the Redmine REST API for available keys."
  :type '(plist))

(defcustom redmine-org-status-todo-alist
  '(;; Redmine roseaudio.kr statuses (Korean)
    ("신규"         . "TODO")
    ("수정중"       . "NEXT")
    ("확인중"       . "NEXT")
    ("수정완료"     . "WAIT")   ; fixed, awaiting verification/close (not closed)
    ("현상태유지"   . "HOLD")
    ;; Common English / other Korean statuses (fallbacks)
    ("New"         . "TODO")
    ("In Progress" . "NEXT")
    ("진행"         . "NEXT")
    ("진행중"       . "NEXT")
    ("Feedback"    . "WAIT")
    ("피드백"       . "WAIT")
    ("Resolved"    . "WAIT")
    ("해결"         . "WAIT")
    ("Closed"      . "DONE")
    ("완료"         . "DONE")
    ("Rejected"    . "CANCELED")
    ("거부"         . "CANCELED"))
  "Map a Redmine status name to an org TODO keyword.
Unknown statuses fall back to `redmine-org-default-todo'."
  :type '(alist :key-type string :value-type string))

(defcustom redmine-org-default-todo "TODO"
  "Org TODO keyword used when a Redmine status is not in the alist."
  :type 'string)

(defcustom redmine-org-priority-alist
  '(("Immediate" . ?A)
    ("긴급"       . ?A)
    ("Urgent"    . ?A)
    ("Critical"  . ?A)
    ("High"      . ?B)
    ("높음"       . ?B)
    ("Major"     . ?B)
    ("Normal"    . ?C)
    ("보통"       . ?C)
    ("Minor"     . ?C)
    ("Low"       . nil)
    ("낮음"       . nil))
  "Map a Redmine priority name to an org priority char (or nil for none)."
  :type '(alist :key-type string :value-type (choice character (const nil))))

(defcustom redmine-org-add-to-agenda t
  "When non-nil, add `redmine-org-file' to `org-agenda-files' after sync."
  :type 'boolean)

(defcustom redmine-org-sync-interval 300
  "Minimum seconds between automatic syncs (see `redmine-org-maybe-sync').
Use 0 to sync on every call."
  :type 'integer)

(defvar redmine-org--last-sync nil
  "Time of the last successful sync, or nil if never synced this session.")

;;; Internal helpers

(defun redmine-org--api-key ()
  "Return the Redmine API key from `auth-source', or signal an error."
  (let* ((found (car (auth-source-search :host redmine-org-auth-host
                                         :max 1 :require '(:secret))))
         (secret (and found (plist-get found :secret))))
    (or (and secret (if (functionp secret) (funcall secret) secret))
        (user-error
         "No Redmine API key for host %S in auth-source.  Add: machine %s login apikey password <KEY>"
         redmine-org-auth-host redmine-org-auth-host))))

(defmacro redmine-org--with-connection (&rest body)
  "Evaluate BODY with elmine bound to this instance's host and API key."
  (declare (indent 0) (debug t))
  `(let ((elmine/host redmine-org-host)
         (elmine/api-key (redmine-org--api-key)))
     ,@body))

(defun redmine-org--decode-tree (obj)
  "Recursively decode raw UTF-8 byte strings in OBJ to multibyte strings.
elmine returns undecoded (unibyte) strings from the HTTP response, which
display as octal escapes and fail `equal' against multibyte literals (so
the status mapping would silently fall back).  Numbers and symbols pass
through unchanged."
  (cond
   ((stringp obj)
    (if (multibyte-string-p obj) obj (decode-coding-string obj 'utf-8)))
   ((consp obj)
    (cons (redmine-org--decode-tree (car obj))
          (redmine-org--decode-tree (cdr obj))))
   (t obj)))

(defun redmine-org--encode-ascii-json (object)
  "Return JSON for OBJECT with every non-ASCII char escaped as \\uXXXX.
The result is pure ASCII.  This matters because url.el (Emacs) rejects an
HTTP request whose *assembled* text is multibyte (Bug#23750): a raw UTF-8
body has bytes >=128 that get promoted to eight-bit characters when
concatenated with the other header strings, tripping that check.  Keeping
the body ASCII-only (Korean etc. as \\uXXXX, which Redmine parses back)
sidesteps the promotion entirely."
  (let* ((json-object-type 'plist)
         (json-array-type 'list)
         (raw (json-encode object)))
    (mapconcat
     (lambda (ch)
       (cond ((< ch 128) (char-to-string ch))
             ((< ch #x10000) (format "\\u%04x" ch))
             (t (let ((c (- ch #x10000)))          ; astral: surrogate pair
                  (format "\\u%04x\\u%04x"
                          (+ #xd800 (ash c -10))
                          (+ #xdc00 (logand c #x3ff)))))))
     raw "")))

(defun redmine-org--fetch-issues ()
  "Fetch the issues assigned to `redmine-org-assigned-to' from Redmine."
  (redmine-org--with-connection
    (redmine-org--decode-tree
     (apply #'elmine/get-issues
            :assigned_to_id redmine-org-assigned-to
            redmine-org-extra-filters))))

(defun redmine-org--update-issue (id fields)
  "PUT FIELDS (inner issue plist, e.g. (:status_id 7)) to issue ID.
Return t on any 2xx response; otherwise signal a `user-error' with the
server's message.  Redmine returns 204 on success, which elmine's own
`elmine/update-issue' misreports, so we inspect the status code directly."
  (redmine-org--with-connection
    (let* (;; Pure-ASCII JSON body so url.el never sees a multibyte request
           ;; (see `redmine-org--encode-ascii-json').
           (data (redmine-org--encode-ascii-json (list :issue fields)))
           (response (elmine/api-raw "PUT" (format "/issues/%s.json" id) data nil))
           (code (plist-get (plist-get response :status) :code))
           (body (plist-get response :body)))
      (if (and (integerp code) (<= 200 code 299))
          t
        (let ((errs (ignore-errors (plist-get (elmine/api-decode body) :errors))))
          (user-error "Redmine #%s update failed (HTTP %s): %s" id code
                      (cond ((and errs (listp errs)) (mapconcat #'identity errs "; "))
                            ((and body (> (length body) 0)) body)
                            (t "unknown error"))))))))

(defun redmine-org--assoc-name (plist)
  "Return the :name of a nested Redmine object PLIST, or nil."
  (and plist (plist-get plist :name)))

(defun redmine-org--todo-keyword (issue)
  "Return the org TODO keyword for ISSUE based on its Redmine status."
  (let ((status (redmine-org--assoc-name (plist-get issue :status))))
    (or (cdr (assoc status redmine-org-status-todo-alist))
        redmine-org-default-todo)))

(defun redmine-org--priority-cookie (issue)
  "Return an org priority cookie string (e.g. \"[#A] \") for ISSUE, or \"\"."
  (let* ((name (redmine-org--assoc-name (plist-get issue :priority)))
         (cell (assoc name redmine-org-priority-alist))
         (char (and cell (cdr cell))))
    (if char (format "[#%c] " char) "")))

(defun redmine-org--issue-url (issue)
  "Return the browser URL for ISSUE."
  (format "%s/issues/%s" redmine-org-host (plist-get issue :id)))

(defun redmine-org--format-issue (issue)
  "Render a single Redmine ISSUE plist as an org subtree string."
  (let* ((id       (plist-get issue :id))
         (subject  (or (plist-get issue :subject) "(no subject)"))
         (project  (redmine-org--assoc-name (plist-get issue :project)))
         (tracker  (redmine-org--assoc-name (plist-get issue :tracker)))
         (status   (redmine-org--assoc-name (plist-get issue :status)))
         (priority (redmine-org--assoc-name (plist-get issue :priority)))
         (done     (plist-get issue :done_ratio))
         (due      (plist-get issue :due_date))
         (updated  (plist-get issue :updated_on))
         (todo     (redmine-org--todo-keyword issue))
         (prio     (redmine-org--priority-cookie issue)))
    (concat
     (format "* %s %s[[%s][#%s]] %s%s\n"
             todo prio (redmine-org--issue-url issue) id
             (if tracker (format "[%s] " tracker) "")
             subject)
     (when (and due (not (string-empty-p due)))
       (format "  DEADLINE: <%s>\n" due))
     "  :PROPERTIES:\n"
     (format "  :REDMINE_ID: %s\n" id)
     (format "  :REDMINE_URL: %s\n" (redmine-org--issue-url issue))
     (when project  (format "  :PROJECT: %s\n" project))
     (when status   (format "  :STATUS: %s\n" status))
     (when priority (format "  :PRIORITY_NAME: %s\n" priority))
     (when (numberp done) (format "  :DONE_RATIO: %d%%\n" done))
     (when updated  (format "  :UPDATED: %s\n" updated))
     "  :END:\n")))

(defun redmine-org--render (issues)
  "Return the full org buffer contents for ISSUES."
  (concat
   "# -*- mode: org; coding: utf-8; -*-\n"
   "#+TITLE: Redmine Issues Assigned To Me\n"
   "#+FILETAGS: :redmine:\n"
   "#+STARTUP: overview\n"
   "# AUTO-GENERATED by redmine-org.el -- do not edit; run M-x redmine-org-sync.\n"
   (format "# Last synced from %s\n\n" redmine-org-host)
   (if issues
       (mapconcat #'redmine-org--format-issue issues "")
     "# No issues assigned.\n")))

;;;###autoload
(defun redmine-org-sync ()
  "Fetch Redmine issues assigned to me and write them to `redmine-org-file'."
  (interactive)
  (message "Redmine: fetching issues assigned to %S..." redmine-org-assigned-to)
  (let* ((issues (redmine-org--fetch-issues))
         (content (redmine-org--render issues))
         ;; Content contains Korean (statuses, subjects); force UTF-8 so
         ;; saving never prompts "Select coding system".
         (coding-system-for-write 'utf-8-unix))
    (with-temp-file redmine-org-file
      (insert content))
    ;; Refresh any buffer already visiting the file.
    (let ((buf (get-file-buffer redmine-org-file)))
      (when buf
        (with-current-buffer buf
          (revert-buffer :ignore-auto :noconfirm))))
    (when (and redmine-org-add-to-agenda
               (boundp 'org-agenda-files)
               (listp org-agenda-files))
      (add-to-list 'org-agenda-files redmine-org-file t))
    (setq redmine-org--last-sync (current-time))
    (message "Redmine: %d issue(s) written to %s"
             (length issues) redmine-org-file)
    issues))

;;;###autoload
(defun redmine-org-maybe-sync ()
  "Sync unless a sync ran within `redmine-org-sync-interval' seconds.
Any error is caught and reported, so callers (e.g. an agenda hook)
still proceed when Redmine is unreachable."
  (when (or (null redmine-org--last-sync)
            (>= (float-time (time-subtract (current-time) redmine-org--last-sync))
                redmine-org-sync-interval))
    (condition-case err
        (redmine-org-sync)
      (error
       ;; Avoid a repeated hammering loop when the server keeps failing.
       (setq redmine-org--last-sync (current-time))
       (message "Redmine auto-sync skipped: %s" (error-message-string err))))))

;;;###autoload
(defun redmine-org-open ()
  "Open `redmine-org-file', syncing first if it does not exist yet."
  (interactive)
  (unless (file-exists-p redmine-org-file)
    (redmine-org-sync))
  (find-file redmine-org-file))

;;; Write-back commands (operate on the issue at point)

(defun redmine-org--id-at-point ()
  "Return the REDMINE_ID of the org entry at point, or signal an error."
  (or (org-entry-get nil "REDMINE_ID")
      (user-error "No REDMINE_ID property on the entry at point")))

(defun redmine-org--allowed-statuses (id)
  "Return an alist of (NAME . ID) statuses the workflow allows for issue ID.
Falls back to the full status list if the server omits allowed_statuses."
  (redmine-org--with-connection
    (let* ((issue (redmine-org--decode-tree
                   (elmine/get-issue id :include "allowed_statuses")))
           (allowed (plist-get issue :allowed_statuses)))
      (mapcar (lambda (s) (cons (plist-get s :name) (plist-get s :id)))
              (or allowed (elmine/get-issue-statuses))))))

(defun redmine-org--reflect-status (name)
  "Update the entry at point to reflect the new Redmine status NAME.
Sets the STATUS property and the org TODO keyword without logging."
  (org-entry-put nil "STATUS" name)
  (let ((kw (cdr (assoc name redmine-org-status-todo-alist)))
        (org-inhibit-logging t))          ; don't prompt for a state-change note
    (when kw (org-todo kw))))

;;;###autoload
(defun redmine-org-set-status (&optional add-note)
  "Change the Redmine status of the issue at point.
Prompt among the workflow-allowed statuses and push the change.
With a prefix argument, also prompt for a note to attach to the change."
  (interactive "P")
  (let* ((id (redmine-org--id-at-point))
         (choices (redmine-org--allowed-statuses id))
         (name (completing-read (format "New status for #%s: " id)
                                (mapcar #'car choices) nil t))
         (status-id (cdr (assoc name choices)))
         (note (and add-note (read-string "Note (optional): ")))
         (fields (append (list :status_id status-id)
                         (and note (not (string-empty-p note)) (list :notes note)))))
    (redmine-org--update-issue id fields)
    (redmine-org--reflect-status name)
    (message "Redmine #%s -> %s" id name)))

;;;###autoload
(defun redmine-org-add-note (note)
  "Add NOTE as a comment on the Redmine issue at point."
  (interactive "sNote: ")
  (when (string-empty-p (string-trim note))
    (user-error "Empty note"))
  (let ((id (redmine-org--id-at-point)))
    (redmine-org--update-issue id (list :notes note))
    (message "Redmine #%s: note added" id)))

;;;###autoload
(defun redmine-org-set-done-ratio (pct)
  "Set the done ratio (0-100) of the Redmine issue at point to PCT."
  (interactive "nDone %%: ")
  (setq pct (max 0 (min 100 pct)))
  (let ((id (redmine-org--id-at-point)))
    (redmine-org--update-issue id (list :done_ratio pct))
    (org-entry-put nil "DONE_RATIO" (format "%d%%" pct))
    (message "Redmine #%s: done %d%%" id pct)))

(provide 'redmine-org)
;;; redmine-org.el ends here
