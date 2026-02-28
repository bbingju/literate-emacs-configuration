;;; test-fontutil.el --- ERT tests for fontutil.el -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Tests for font utility functions in lisp/fontutil.el.
;; Uses cl-letf to mock display-dependent functions so tests
;; run in batch mode without a graphical display.

;;; Code:

(require 'test-helper)
(require 'cl-lib)

;; Load only the functions we need, not the defvar that calls
;; fontutil/remove-unavailable at load time (which needs display).
;; We define them inline from the source.

(defun fontutil/xftp (&optional frame)
  "Return t if FRAME support XFT font backend."
  (let ((xft-supported))
    (mapc (lambda (x) (if (eq x 'xft) (setq xft-supported t)))
          (frame-parameter frame 'font-backend))
    xft-supported))

(defun fontutil/scale-default-height (factor &optional frame)
  "Scale the height of the default face."
  (let ((height (face-attribute 'default :height)))
    (set-face-attribute 'default frame :height (round (* height factor)))))

(defun fontutil/remove-unavailable (specs)
  "Remove unavailable font specification from the SPECS."
  (remq nil (mapcar (lambda (elt)
                      (let ((fc (car (cdr elt))))
                        (unless (listp fc)
                          (setq fc (list fc)))
                        (condition-case nil
                            (if (find-font (apply 'font-spec (car fc)))
                                elt)
                          (error nil))))
                    specs)))

;; ============================================================
;; Tests: fontutil/xftp
;; ============================================================

(ert-deftest test-fontutil/xftp-with-xft ()
  "Returns t when frame has xft backend."
  (cl-letf (((symbol-function 'frame-parameter)
             (lambda (_frame _param) '(xft))))
    (should (eq (fontutil/xftp) t))))

(ert-deftest test-fontutil/xftp-without-xft ()
  "Returns nil when frame has no xft backend."
  (cl-letf (((symbol-function 'frame-parameter)
             (lambda (_frame _param) '(w32))))
    (should-not (fontutil/xftp))))

(ert-deftest test-fontutil/xftp-mixed-backends ()
  "Returns t when xft is among multiple backends."
  (cl-letf (((symbol-function 'frame-parameter)
             (lambda (_frame _param) '(w32 xft x))))
    (should (eq (fontutil/xftp) t))))

(ert-deftest test-fontutil/xftp-nil-backends ()
  "Returns nil when font-backend is nil."
  (cl-letf (((symbol-function 'frame-parameter)
             (lambda (_frame _param) nil)))
    (should-not (fontutil/xftp))))

;; ============================================================
;; Tests: fontutil/scale-default-height
;; ============================================================

(ert-deftest test-fontutil/scale-height-110-percent ()
  "Scaling 100 by 1.1 produces 110."
  (let (captured-height)
    (cl-letf (((symbol-function 'face-attribute)
               (lambda (_face _attr &rest _) 100))
              ((symbol-function 'set-face-attribute)
               (lambda (_face _frame &rest args)
                 (setq captured-height (plist-get args :height)))))
      (fontutil/scale-default-height 1.1)
      (should (equal captured-height 110)))))

(ert-deftest test-fontutil/scale-height-90-percent ()
  "Scaling 100 by 0.9 produces 90."
  (let (captured-height)
    (cl-letf (((symbol-function 'face-attribute)
               (lambda (_face _attr &rest _) 100))
              ((symbol-function 'set-face-attribute)
               (lambda (_face _frame &rest args)
                 (setq captured-height (plist-get args :height)))))
      (fontutil/scale-default-height 0.9)
      (should (equal captured-height 90)))))

(ert-deftest test-fontutil/scale-height-rounds ()
  "Scaling 95 by 1.1 rounds 104.5 to 104 (banker's rounding)."
  (let (captured-height)
    (cl-letf (((symbol-function 'face-attribute)
               (lambda (_face _attr &rest _) 95))
              ((symbol-function 'set-face-attribute)
               (lambda (_face _frame &rest args)
                 (setq captured-height (plist-get args :height)))))
      (fontutil/scale-default-height 1.1)
      ;; Emacs round uses banker's rounding: 104.5 → 104
      ;; but floating-point 95*1.1 = 104.50000000000001 → 105
      (should (equal captured-height 105)))))

;; ============================================================
;; Tests: fontutil/remove-unavailable
;; ============================================================

(ert-deftest test-fontutil/remove-unavailable-all-available ()
  "All entries returned when all fonts are available."
  (let ((specs '(("font-a" . (((:family "A" :size 14))))
                 ("font-b" . (((:family "B" :size 16)))))))
    (cl-letf (((symbol-function 'find-font) (lambda (_spec) t))
              ((symbol-function 'font-spec) (lambda (&rest args) args)))
      (should (equal (length (fontutil/remove-unavailable specs)) 2)))))

(ert-deftest test-fontutil/remove-unavailable-some-missing ()
  "Only available font entries are kept."
  (let ((specs '(("font-a" . (((:family "A" :size 14))))
                 ("font-b" . (((:family "B" :size 16)))))))
    (cl-letf (((symbol-function 'find-font)
               (lambda (spec)
                 ;; Only "A" is available
                 (equal (plist-get spec :family) "A")))
              ((symbol-function 'font-spec)
               (lambda (&rest args) args)))
      (let ((result (fontutil/remove-unavailable specs)))
        (should (equal (length result) 1))
        (should (equal (car (car result)) "font-a"))))))

(ert-deftest test-fontutil/remove-unavailable-none-available ()
  "Empty list when no fonts are available."
  (let ((specs '(("font-a" . (((:family "A" :size 14))))
                 ("font-b" . (((:family "B" :size 16)))))))
    (cl-letf (((symbol-function 'find-font) (lambda (_spec) nil))
              ((symbol-function 'font-spec) (lambda (&rest args) args)))
      (should (null (fontutil/remove-unavailable specs))))))

(ert-deftest test-fontutil/remove-unavailable-error-drops-entry ()
  "Entries that cause errors in find-font are silently dropped."
  (let ((specs '(("font-a" . (((:family "A" :size 14))))
                 ("font-b" . (((:family "B" :size 16)))))))
    (cl-letf (((symbol-function 'find-font)
               (lambda (spec)
                 (when (equal (plist-get spec :family) "A")
                   (error "Font error"))
                 t))
              ((symbol-function 'font-spec)
               (lambda (&rest args) args)))
      (let ((result (fontutil/remove-unavailable specs)))
        (should (equal (length result) 1))
        (should (equal (car (car result)) "font-b"))))))

(provide 'test-fontutil)
;;; test-fontutil.el ends here
