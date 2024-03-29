;; -*- mode: emacs-lisp -*-

;;;
;;; Font-related configuration
;;;

;; Note that the default font for Emacs should not be handled here.
;;

(defun fontutil/xftp (&optional frame)
  "Return t if FRAME support XFT font backend."
  (let ((xft-supported))
    (mapc (lambda (x) (if (eq x 'xft) (setq xft-supported t)))
          (frame-parameter frame 'font-backend))
    xft-supported))

(defun fontutil/scale-default-height (factor &optional frame)
  "Scale the height of the default face
New height will be calculated by (* FACTOR old-face-height)"
  (let ((height (face-attribute 'default :height)))
    (set-face-attribute 'default frame :height (round (* height factor)))))


(defun fontutil/increase-default-height ()
  (interactive)
  (fontutil/scale-default-height 1.1
                             (selected-frame))
  (message "New face height: %d"
           (face-attribute 'default :height)))

(defun fontutil/decrease-default-height ()
  (interactive)
  (fontutil/scale-default-height 0.9
                             (selected-frame))
  (message "New face height: %d"
           (face-attribute 'default :height)))

(defun fontutil/install-mouse-wheel ()
  (when (display-graphic-p)
    ;; When Emacs uses graphic display,"control + mouse wheel up"
    ;; increases the default font size whereas "control + mouse wheel
    ;; down " decreases the size.

    ;; Note that if you call `mwheel-install' after this configuration,
    ;; both [C-mouse-4] and [C-mouse-5] bindings are cleared.
    ;;
    ;; It seems that mwheel package is automatically loaded in Emacs 22
    ;; or higher.  Thus, I do not need to call `mwheel-install' any longer.

    ;; In darwin, the wheel-up and wheel-down events are [C-wheel-up]
    ;; and [C-wheel-down] respectively.
    (let ((incr [C-mouse-4]) (decr [C-mouse-5]))
      (and (eq system-type 'darwin)
           (setq incr [C-wheel-up]
                 decr [C-wheel-down]))

      (global-set-key incr 'fontutil/increase-default-height)
      (global-set-key decr 'fontutil/decrease-default-height))))

(defun fontutil/remove-unavailable (specs)
  "Remove unavailable font specification from the SPECS.

See `fontutil/fontconfig' for the definition of SPECS."
  (remq nil (mapcar (lambda (elt)
                      (let ((fc (car (cdr elt))))
                        (unless (listp fc)
                          (setq fc (list fc)))
                        (if (find-font (apply 'font-spec (car fc)))
                            elt)))
                    specs)))

(defvar fontutil/fontconfig
  (fontutil/remove-unavailable
   '(("scodepro-14" . (((:family "Source Code Pro" :size 14)
                        (hangul :family "D2Coding" :size 16)
                        (symbol :family "Symbola" :size 15))
                       ((line-spacing . 1))))
     ("scodepro-15" . (((:family "Source Code Pro" :size 15)
                        (hangul :family "D2Coding" :size 18)
                        (symbol :family "Symbola" :size 15))))
     ("scodepro-16" . (((:family "Source Code Pro" :size 16)
                        (hangul :family "D2Coding" :size 20)
                        (symbol :family "Symbola" :size 15))))
     ("scodepro-18" . (((:family "Source Code Pro" :size 18)
                        (hangul :family "D2Coding" :size 22)
                        (symbol :family "Symbola" :size 24))))
     ("scodepro-20" . (((:family "Source Code Pro" :size 20)
                        (hangul :family "D2Coding" :size 24)
                        (symbol :family "Symbola" :size 20))))
     ("scodepro-22" . (((:family "Source Code Pro" :size 22)
                        (hangul :family "D2Coding" :size 22)
                        (symbol :family "Symbola" :size 22))))
     ("scodepro-24" . (((:family "Source Code Pro" :size 24)
                        (hangul :family "D2Coding" :size 24)
                        (symbol :family "Symbola" :size 24))))
     ("scodepro-26" . (((:family "Source Code Pro" :size 26)
                        (hangul :family "D2Coding" :size 26)
                        (symbol :family "Symbola" :size 26))))
     ("scodepro-28" . (((:family "Source Code Pro" :size 28)
                        (hangul :family "D2Coding" :size 28)
                        (symbol :family "Symbola" :size 28))))
     ("scodepro-30" . (((:family "Source Code Pro" :size 30)
                        (hangul :family "D2Coding" :size 30)
                        (symbol :family "Symbola" :size 30))))
     ("scodepro-32" . (((:family "Source Code Pro" :size 32)
                        (hangul :family "D2Coding" :size 32)
                        (symbol :family "Symbola" :size 32))))

     ("ubuntu-14" . (((:family "Ubuntu Mono" :size 14)
                        (hangul :family "D2Coding" :size 14)
                        (symbol :family "Symbola" :size 15))
                     ((line-spacing . 3))))
     ("ubuntu-15" . (((:family "Ubuntu Mono" :size 15)
                        (hangul :family "D2Coding" :size 15)
                        (symbol :family "Symbola" :size 16))
                     ((line-spacing . 4))))
     ("ubuntu-16" . (((:family "Ubuntu Mono" :size 16)
                        (hangul :family "Noto Sans Mono CJK KR" :size 16)
                        (symbol :family "Noto Color Emoji" :size 17))
                     ((line-spacing . 4))))
     ("ubuntu-18" . (((:family "Ubuntu Mono" :size 18)
                        (hangul :family "D2Coding" :size 18)
                        (symbol :family "Symbola" :size 20))
                     ((line-spacing . 4))))

     ("ubuntu-20" . (((:family "Ubuntu Mono" :size 20)
                        (hangul :family "D2Coding" :size 20)
                        (symbol :family "Symbola" :size 22))
                     ((line-spacing . 4))))

     ("cascadia-14" . (((:family "Cascadia Code PL" :size 14)
                        (hangul :family "Noto Sans Mono CJK KR" :size 16)
                        (symbol :family "Cascadia Code PL" :size 14))
                     ((line-spacing . 4))))
     ("cascadia-16" . (((:family "Cascadia Code PL" :size 16)
                        (hangul :family "D2Coding" :size 18)
                        (symbol :family "Cascadia Code PL" :size 16))
                     ((line-spacing . 4))))

     ("noto-15" . (((:family "Noto Mono" :size 15)
                    (hangul :family "Noto Sans Mono CJK KR" :size 15)
                    (symbol :family "Noto Color Emoji" :size 17))
                   ((line-spacing . 4))))

     ("noto-16" . (((:family "Noto Mono" :size 16)
                    (hangul :family "Noto Sans Mono CJK KR" :size 16)
                    (symbol :family "Noto Color Emoji" :size 18))
                   ((line-spacing . 4))))

     ("dejavu-28" . (((:family "Dejavu Sans Mono" :size 28)
                      (hangul :family "Noto Sans Mono CJK KR" :size 28)
                      (symbol :family "Noto Color Emoji" :size 28))
                     ((line-spacing . 5))))

     ("dejavu-32" . (((:family "Dejavu Sans Mono" :size 32)
                      (hangul :family "Noto Sans Mono CJK KR" :size 32)
                      (symbol :family "Noto Color Emoji" :size 32))
                     ((line-spacing . 5))))

     ("inconsolata-14" . (((:family "Inconsolata" :size 14)
                           (hangul :family "D2Coding" :size 14)
                           (symbol :family "Symbola" :size 15))
                          ((line-spacing . 2))))
     ("inconsolata-15" . (((:family "Inconsolata" :size 15)
                           (hangul :family "D2Coding" :size 16)
                           (symbol :family "Symbola" :size 17))
                          ((line-spacing . 2))))
     ("inconsolata-16" . (((:family "Inconsolata" :size 16)
                           (hangul :family "D2Coding" :size 16)
                           (symbol :family "Symbola" :size 17))
                          ((line-spacing . 3))))
     ("inconsolata-20" . (((:family "Inconsolata" :size 20)
                           (hangul :family "D2Coding" :size 20)
                           (symbol :family "Symbola" :size 22))
                          ((line-spacing . 3))))

     ("consolas-14" . (((:family "Consolas" :size 14)
                           (hangul :family "D2Coding" :size 16)
                           (symbol :family "Symbola" :size 16))
                          ((line-spacing . 2))))
     ("consolas-15" . (((:family "Consolas" :size 15)
                           (hangul :family "D2Coding" :size 16)
                           (symbol :family "Symbola" :size 16))
                          ((line-spacing . 2))))
     ("consolas-16" . (((:family "Consolas" :size 16)
                           (hangul :family "D2Coding" :size 18)
                           (symbol :family "Symbola" :size 17))
                          ((line-spacing . 3))))
     ("consolas-20" . (((:family "Consolas" :size 20)
                           (hangul :family "D2Coding" :size 22)
                           (symbol :family "Symbola" :size 25))
                          ((line-spacing . 3))))

     ("monaco-14" . (((:family "Monaco" :size 14)
                      (hangul :family "D2Coding" :size 16)
                      (symbol :family "Symbola" :size 17))))
     ("monaco-15" . (((:family "Monaco" :size 15)
                      (hangul :family "D2Coding" :size 18)
                      (symbol :family "Symbola" :size 20))))
     ("monaco-16" . (((:family "Monaco" :size 16)
                      (hangul :family "D2Coding" :size 22)
                      (symbol :family "Symbola" :size 22))))

     ("menlo-14" . (((:family "Menlo" :size 14)
                      (hangul :family "D2Coding" :size 16)
                      (symbol :family "Symbola" :size 20))))
     ("menlo-16" . (((:family "Menlo" :size 16)
                      (hangul :family "D2Coding" :size 20)
                      (symbol :family "Symbola" :size 20))))
     ("menlo-18" . (((:family "Menlo" :size 18)
                      (hangul :family "D2Coding" :size 22)
                      (symbol :family "Symbola" :size 20))))

     ("pt-14" . (((:family "PT Mono" :size 14)
                      (hangul :family "D2Coding" :size 16)
                      (symbol :family "Symbola" :size 20))
                 ((line-spacing . 4))))
     ("pt-16" . (((:family "PT Mono" :size 16)
                      (hangul :family "D2Coding" :size 20)
                      (symbol :family "Symbola" :size 20))))
     ("pt-18" . (((:family "PT Mono" :size 18)
                      (hangul :family "D2Coding" :size 22)
                      (symbol :family "Symbola" :size 20))
                 ((line-spacing . 3))))

     ("hack-14" . (((:family "Hack" :size 14)
                      (hangul :family "D2Coding" :size 16)
                      (symbol :family "Symbola" :size 20))))
     ("hack-16" . (((:family "Hack" :size 16)
                      (hangul :family "D2Coding" :size 20)
                      (symbol :family "Symbola" :size 20))))
     ("hack-18" . (((:family "Hack" :size 18)
                      (hangul :family "D2Coding" :size 22)
                      (symbol :family "Symbola" :size 20))))

     ("firacode-13" . (((:family "Fira Code" :size 13)
                    (hangul :family "D2Coding" :size 15)
                    (symbol :family "Symbola" :size 18))))
     ("firacode-14" . (((:family "Fira Code" :size 14)
                    (hangul :family "D2Coding" :size 16)
                    (symbol :family "Symbola" :size 20))))
     ("firacode-15" . (((:family "Fira Code" :size 15)
                    (hangul :family "D2Coding" :size 18)
                    (symbol :family "Symbola" :size 20))))
     ("firacode-16" . (((:family "Fira Code" :size 16)
                        (hangul :family "D2Coding" :size 20)
                        (symbol :family "Symbola" :size 15))))
     ("firacode-18" . (((:family "Fira Code" :size 18)
                        (hangul :family "D2Coding" :size 22)
                        (symbol :family "Symbola" :size 24))))
     ("firacode-22" . (((:family "Fira Code" :size 22)
                        (hangul :family "D2Coding" :size 22)
                        (symbol :family "Fira Code" :size 22))))

     ("d2coding-14" . (((:family "D2Coding" :size 14)
                        (hangul :family "D2Coding" :size 14)
                        (symbol :family "D2Coding" :size 14))))
     ("d2coding-15" . (((:family "D2Coding" :size 15)
                        (hangul :family "D2Coding" :size 15)
                        (symbol :family "D2Coding" :size 15))))
     ("d2coding-16" . (((:family "D2Coding" :size 16)
                        (hangul :family "D2Coding" :size 16)
                        (symbol :family "D2Coding" :size 16))))
     ("d2coding-18" . (((:family "D2Coding" :size 18)
                        (hangul :family "D2Coding" :size 18)
                        (symbol :family "D2Coding" :size 18))))
     ("d2coding-22" . (((:family "D2Coding" :size 22)
                        (hangul :family "D2Coding" :size 22)
                        (symbol :family "D2Coding" :size 22))))
     ("d2coding-24" . (((:family "D2Coding" :size 24)
                        (hangul :family "D2Coding" :size 24)
                        (symbol :family "D2Coding" :size 24))))
     ("d2coding-32" . (((:family "D2Coding" :size 32)
                        (hangul :family "D2Coding" :size 32)
                        (symbol :family "D2Coding" :size 32))))

     ("cascadia-14" . (((:family "Cascadia Code PL" :size 14)
                        (hangul :family "D2Coding" :size 14)
                        (symbol :family "Cascadia Code PL" :size 14))))
     ("cascadia-16" . (((:family "Cascadia Code PL" :size 16)
                        (hangul :family "D2Coding" :size 16)
                        (symbol :family "Cascadia Code PL" :size 16))))
     ("cascadia-18" . (((:family "Cascadia Code PL" :size 18)
                        (hangul :family "D2Coding" :size 18)
                        (symbol :family "Cascadia Code PL" :size 18))))
     ("cascadia-20" . (((:family "Cascadia Code PL" :size 20)
                        (hangul :family "D2Coding" :size 20)
                        (symbol :family "Cascadia Code PL" :size 20))))
     ("cascadia-24" . (((:family "Cascadia Code PL" :size 24)
                        (hangul :family "D2Coding" :size 24)
                        (symbol :family "Cascadia Code PL" :size 24))))
     ("cascadia-32" . (((:family "Cascadia Code PL" :size 32)
                        (hangul :family "D2Coding" :size 32)
                        (symbol :family "Cascadia Code PL" :size 32))))
     ("cascadia-36" . (((:family "Cascadia Code PL" :size 36)
                        (hangul :family "D2Coding" :size 36)
                        (symbol :family "Cascadia Code PL" :size 36))))
     ("cascadia-40" . (((:family "Cascadia Code PL" :size 40)
                        (hangul :family "D2Coding" :size 40)
                        (symbol :family "Cascadia Code PL" :size 40))))
     ))
  "Font and Frame configuration list

Each element has a form of (FONT-SPEC FRAME-SPEC), where
FONT-SPEC is a list of font properties, and FRAME-SPEC is a list
of frame parameters.

See `font-spec' for the list of font properties.

You can specify multiple font in FONT-SPEC; In this case, FONT-SPEC
is a list of font specs, where the first element is font properties
for the default font, and the rest elements are for the additional
font properties.  Except the first element, all elements have
TARGET as a prefix element.  See `set-fontset-font' for the possible
TARGET.  For example, if you want to use font XXX as the default font,
and you want to use YYY for Korean script, hangul, and use ZZZ for
symbolic glyphs, then the FONT-SPEC might be

  ((:family \"XXX\" :size 14)
   (hangul :family \"YYY\" :size 16)
   (symbol :family \"ZZZ\") :size 15)

As you can see in above, each font can have different size.  (This
is useful for CJK fonts)")

(defun fontutil/apply (name)
  "Apply font specification, NAME.

See also `fontutil/fontconfig' for the possible candidates"
  (let ((fonts (car (cdr (assoc name fontutil/fontconfig))))
        (params (cadr (cdr (assoc name fontutil/fontconfig)))))
    (unless (listp (car fonts))
      (setq fonts (list fonts)))
    (when (car fonts)
      (set-face-attribute 'default nil :font (apply 'font-spec (car fonts))))
    (dolist (aux (cdr fonts))
      (set-fontset-font t (car aux) (apply 'font-spec (cdr aux))))
    (when params
      (setq default-frame-alist params)
      (dolist (f (frame-list))
        (modify-frame-parameters f params)))))

(defun fontutil/set-font (name)
  "Apply font specification, NAME from `fontutil/fontconfig'"
  (interactive (list (completing-read "font: " fontutil/fontconfig)))
                                      ;; (lambda (f)
                                      ;;   (list-fonts (apply #'font-spec
                                      ;;                      (car (cdr f)))))
                                      ;; t nil)))
  (fontutil/apply name))

(when nil ;; (display-graphic-p)
  ;; See https://github.com/tonsky/FiraCode/wiki/Setting-up-Emacs

  ;; Magit has also problem with this
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                 (36 . ".\\(?:>\\)")
                 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                 ;; comment below entry if youn got 'error in process
                 ;; filter: attempt to shape unibyte text
                 ;; (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (48 . ".\\(?:x[a-zA-Z]\\)")
                 (58 . ".\\(?:::\\|[:=]\\)")
                 (59 . ".\\(?:;;\\|;\\)")
                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                 (91 . ".\\(?:]\\)")
                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                 (94 . ".\\(?:=\\)")
                 (119 . ".\\(?:ww\\)")
                 (123 . ".\\(?:-\\)")
                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
                 )
               ))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring])))))

(provide 'fontutil)
