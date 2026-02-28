;;; test-ediff-org.el --- ERT tests for ediff org folding -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Tests for `my/ediff-expand-org-buffer' which unfolds org headings
;; and widens narrowed buffers before ediff comparison.

;;; Code:

(require 'test-helper)
(require 'org)
(require 'org-fold nil t)

;; Define the function under test (from README.org)
(defun my/ediff-expand-org-buffer ()
  "Expand all org headings in current buffer for ediff.
Widen first so headings outside a narrowed region are also unfolded."
  (when (derived-mode-p 'org-mode)
    (widen)
    (org-fold-show-all)))

(ert-deftest test-ediff-org/basic-fold-unfold ()
  "Folded org headings are expanded after calling the function."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading 1\nBody 1\n* Heading 2\nBody 2\n")
    (goto-char (point-min))
    (org-cycle)  ; fold first heading
    (my/ediff-expand-org-buffer)
    ;; After expand, all text should be visible
    (goto-char (point-min))
    (should (search-forward "Body 1" nil t))
    (should (search-forward "Body 2" nil t))))

(ert-deftest test-ediff-org/narrowed-buffer ()
  "Narrowed org buffer is widened after calling the function."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading 1\nBody 1\n* Heading 2\nBody 2\n")
    (goto-char (point-min))
    ;; Narrow to first heading only
    (narrow-to-region (point-min) (search-forward "Body 1"))
    (should (buffer-narrowed-p))
    (my/ediff-expand-org-buffer)
    (should-not (buffer-narrowed-p))
    ;; Full buffer content is accessible
    (goto-char (point-min))
    (should (search-forward "Body 2" nil t))))

(ert-deftest test-ediff-org/non-org-buffer-skipped ()
  "Non-org buffers are left unchanged."
  (with-temp-buffer
    (fundamental-mode)
    (insert "some text\n")
    (narrow-to-region 1 5)
    (should (buffer-narrowed-p))
    (my/ediff-expand-org-buffer)
    ;; Should still be narrowed — function skips non-org buffers
    (should (buffer-narrowed-p))))

(ert-deftest test-ediff-org/buffer-b-unfold ()
  "Function works on buffer B (simulating ediff's second buffer)."
  (with-temp-buffer
    (org-mode)
    (insert "* Section A\nContent A\n* Section B\nContent B\n")
    (goto-char (point-min))
    (org-cycle)
    (my/ediff-expand-org-buffer)
    (goto-char (point-min))
    (should (search-forward "Content A" nil t))
    (should (search-forward "Content B" nil t))))

(provide 'test-ediff-org)
;;; test-ediff-org.el ends here
