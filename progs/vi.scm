;; vi
;; TODO: combine two maps colon and single char cmds
;; TODO: parse cmds with numbers
;; There are many commonds such as C-u/d and "g g" are recoganized as invalid commands

(texmacs-module (vi)
                ;; (:use (generic generic-kbd)
                )

(import-from (normal))
(define-public cmd-buffer '())

(define-public vim-active #t)
(define-public normal-active #f)
(define-public visual-active #f)

(tm-define (vim-mode?) vim-active)
(tm-define (normal-mode?) normal-active)
(tm-define (visual-mode?) visual-active) ;TODO


(tm-define (toggle-vim) 
           (:check-mark "v" vim-mode?)
           (set! vim-active (not vim-active))
           (set-message "toggle vim" "")
           )

(tm-define (toggle-normal) 
           (set! normal-active (not normal-active))
           )

(tm-define (exit-normal)
           (set! normal-active #f)
           (set-message "Insert" "")
           (set! cmd-buffer '()))

(tm-define (enter-normal)
           (set! normal-active #t)
           (set-message "Normal" "")
           )

(texmacs-modes
 (in-vim% (vim-mode?)) 
 (in-normal% (normal-mode?) in-vim%)
 ) 


;; (tm-define (keyboard-press key time)
;;            (:require (not normal-active))
;;            (cond 
;;              ((== key "escape") (enter-normal))
;;              (else (former key time))))

(tm-define (message-from-key key)
           (string-append "Sorry! " key " is not supported."))

(tm-define (keyboard-press key time)
           (:require normal-active)
           (:require vim-active)
           (vi-dispatch key))

;; :w
(tm-define (write-to-tm file-name)
           (let* ((this (current-buffer))
                  (to-file-name
                   (if (== "" file-name)
                       this
                       (url-relative this file-name))))
             (if (or (url-scratch? this)
                     (== "" file-name)
                     (== file-name this))
                 (save-buffer this)
                 (save-buffer-as to-file-name (list)))))

;; wq
(tm-define (write-and-quit file-name)
           (write-to-tm file-name)
           (quit-TeXmacs))

(tm-define (set-cmd-buffer lst)
           (set! cmd-buffer lst))

(tm-define (clear-cmd-buffer)
           (set-cmd-buffer '()))

(tm-define (join l)
           (if (null? l) ""
               (let ((c (car l)))
                 (string-append (if (== c "space") " " c) (join (cdr l))))))

(tm-define (skip-colon-space l)
           (if (null? l) '("")
               (let ((c (car l)))
                 (if (or (== c ":") (== c "space"))
                     (skip-colon-space (cdr l))
                     l))))

;; a recursive version of parsing
(tm-define (-parse l cmd)
           (let ((c (car l)))
             (if (== c "space")
                 (if (null? (cdr l)) (list cmd "")
                     (list cmd (join (skip-colon-space (cdr l)))))
                 (if (null? (cdr l)) (list (string-append cmd c) "")
                     (-parse (cdr l) (string-append cmd c))))))

(tm-define (vi-parse-colon l)
           (-parse (skip-colon-space l) ""))


(tm-define (vi-dispatch key)
           (set-cmd-buffer (append cmd-buffer (list key)))
  
           (let* ((fc (car cmd-buffer))
                  (lc (cAr cmd-buffer)))
             (if (== key "escape")
                 (begin (clear-cmd-buffer) (set-message "clearing" ""))
                 (cond

                   ((and (== fc ":")
                         (== lc "return"))
                    (let* ((cmd-opt (vi-parse-colon (cDr cmd-buffer)))
                           (cmd (car cmd-opt))
                           (opt (cadr cmd-opt))
                           (cmd-fn (ahash-ref vi-colon-map cmd)))
                      (if cmd-fn
                          (begin (cmd-fn opt) (clear-cmd-buffer))
                          (begin (clear-cmd-buffer) (set-message "invalide cmd as well" "")))))

                   ((and (== fc ":")
                         (not (== lc "return"))
                         (not (== lc "backspace")))
                    (set-message (test-join cmd-buffer) ""))

                   ;; only :, backspace bug happens
                   ((and (== fc ":")
                         (== lc "backspace"))
                    (set-cmd-buffer (cDr (cDr cmd-buffer)))
                    (set-message (join cmd-buffer) ""))

                   (else ; normal cmd
                    (if (invalid? (parse-all cmd-buffer))
                        (begin
                          (clear-cmd-buffer)
                          (set-message "invalid" ""))

                        (let* ((parse-res (parse-all cmd-buffer))
                               (cmd-type (cadr parse-res))
                               (cmd-char (caddr parse-res)))

                          (cond

                            ((valid-mark? parse-res)

                             (let ((m (fourth parse-res))
                                   (c cmd-char))
                               (jump-or-mark c m)
                               )
                             (clear-cmd-buffer))

                            ((valid-search? parse-res)
                             (set-message (string-append "Success: " (join cmd-buffer)) "")
                             (clear-cmd-buffer))

                            ((valid-hjkl? parse-res)
                             (when (in? cmd-char '("h" "j" "k" "l" "C-d" "C-u" "left" "right" "up"
                                                       "down" "return" "backspace" "i" "L"
                                                       "G" "g g" "a" "A" "I" "#" "[" "]" "o" "O" "x"
                                                       "$" "0" "b" "w" "e" "u" "D" "p" "R"))
                               (let ((n (-repeat-times parse-res))
                                     (fn (vi-get-cmd cmd-char)))
                                 (repeat n (when fn (fn)))))
                             (clear-cmd-buffer))

                            ((valid-cmpd? parse-res)
                             (set-message (string-append "Success: " (join cmd-buffer)) "")
                             (clear-cmd-buffer))
                            (else
                             (set-message
                              (string-append "not complete: " (join cmd-buffer)) ""))))))))))

(define-public single-char-cmds (make-ahash-table))

(tm-define (vi-map-set! key fn)
           (ahash-set! single-char-cmds key fn))

(tm-define (vi-get-cmd key)
           (ahash-ref single-char-cmds key))

(tm-define (vi-map-one l)
           (if (and (pair? l) (string? (car l)) (pair? (cdr l)))
               (with (key action) l
                     `(vi-map-set! ,key ,action))
               (set-message "error" "error")))

(tm-define (vi-map-body l)
           (map (lambda (x) (vi-map-one x)) l))

(tm-define-macro (vi-kbd-map . l)
                 `(begin ,@(vi-map-body l)))


(kbd-map
 (:mode in-vim?)
 ("j k" (enter-normal))
 ;; for easy movement without switching to normal mode
 ("A-j" (kbd-down))
 ("A-k" (kbd-up))
 ("A-h" (kbd-left))
 ("A-l" (kbd-right))
 )

;; ;; current hack: should be put on vi-kbd-map
;; (kbd-map
;;   (:mode in-normal?)
;;   ("d d" (begin
;;            (kbd-start-line)
;;            (kbd-select kbd-end-line)
;;            (kbd-cut)
;;            (kbd-backspace)))
;;   ("d" (kbd-delete)))



(vi-kbd-map
 ("$" kbd-end-line)
 ("0" kbd-start-line)
 ("^" kbd-start-line)

;; kbd-page-up invalid key
 ("C-d" (lambda () (begin (set-message "C-d" "") (kbd-page-down))))
 ("C-u" (lambda () (begin (set-message "C-u" "") (kbd-page-up))))
 ("return" (lambda () (begin (kbd-down) (kbd-start-line))))

 ("h" kbd-left)
 ("left" kbd-left)
 ("backspace" kbd-left)

 ("l" kbd-right)
 ("right" kbd-right)

 ("j" kbd-down)
 ("down" kbd-down)

 ("k" kbd-up)
 ("up" kbd-up)

 ;; researve < and > for promoting sections

 ("[" traverse-next)
 ("]" traverse-previous)

 ("o" (lambda ()
        (kbd-end-line)
        (exit-normal)
        (kbd-return)))

 ("O" (lambda ()
        (kbd-up)
        (kbd-end-line)
        (exit-normal)
        (kbd-return)))

 ("/" interactive-search)

 ("p" (lambda ()
        (clipboard-paste "primary")))

 ("D" (lambda ()
        (kbd-select go-end-line)
        (clipboard-cut "primary"))) ;; it should be cut

 ("x" (lambda ()
        (kbd-select kbd-right)
        (clipboard-cut "primary")))

 ("G" go-end)
 ;; ("g" (noop)); HACK: avoid shoing "g" when using "gg"
 ("g g" go-start) ; Bug: invalid cmds


 ("R" (lambda ()(update-document "all")))

 ("b" go-to-previous-word)

 ;; The go-to-next-word function in TeXmacs goes to the end of the current word
 ("e" go-to-next-word)
 ("w"
  (lambda ()
    (go-to-next-word)
    (kbd-right)))

 ("A"
  (lambda ()
    (kbd-end-line)
    (exit-normal)))

 ("I"
  (lambda ()
    (kbd-start-line)
    (exit-normal)))

 ("i" exit-normal)

 ("a"
  (lambda ()
    (kbd-right)
    (exit-normal)))

 ("#" (lambda () (numbered-toggle (focus-tree))))

 ("u" (lambda ()
        (undo 0)))
 ("C-r" (lambda ()
          (redo 0))))

;; open a scheme session
;; :scm
(tm-define (scheme s)
           (make-session "scheme" "default"))

;;(export-buffer-main (current-buffer) s fm opts)
;; current-buffer-url
;; file-format
;; url-head path
;; url->string
;; url-tail 文件名
;; url-glue
;; url-exists?

; :E
(tm-define (export-to-latex file-name)
           (let* ((this (current-buffer))
                  (to-file-name
                   (if (== "" file-name) 
                       (url-append (url-head this)
                                   (string-append (url-basename this) ".tex"))
                       (url-relative this file-name))))
             (if (url-scratch? this)
                 (choose-file (buffer-exporter "latex") "Export as LaTex" "latex")
                 (export-buffer-main
                  this
                  to-file-name
                  "latex" (list)))))


(tm-define vi-colon-map (make-ahash-table))
;;(ahash-set! vi-colon-map "t" test-msg)
(ahash-set! vi-colon-map "w" write-to-tm)
(ahash-set! vi-colon-map "wq" write-and-quit)
(ahash-set! vi-colon-map "e" load-document)
(ahash-set! vi-colon-map "scm" scheme)
(ahash-set! vi-colon-map "q" (lambda (x) (safely-quit-TeXmacs)))
(ahash-set! vi-colon-map "E" export-to-latex)
(ahash-set! vi-colon-map "ua" (lambda (x) (update-document "all")))
(ahash-set! vi-colon-map "q!" (lambda (x) (kill-current-window-and-buffer)))

(tm-define (get-colon-cmd key)
           (ahash-ref vi-colon-map key))

(tm-define (set!-colon-cmd k v)
           (ahash-set! vi-colon-map k v))



