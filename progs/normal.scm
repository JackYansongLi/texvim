(texmacs-module (normal))

(define-public buf '())

(tm-define digits '("1" "2" "3" "4" "5" "6" "7" "8" "9" ))
(tm-define natural-digits '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))

;; The function parse-prefix and parse-posfix returns the digits in the prefix of vim commands, which represents numbers of lines to move, number of word to delete, etc.
;; Examples:
;    (tm-define l1 '("0" "1" "2"))
;    (tm-define l2 '("2" "a" "2"))
;    (tm-define l3 '("2" "3" "2"))
;    Scheme] (parse-prefix l1) 
;      (("0", "1", "2") ())
;    Scheme] (parse-prefix l2) 
;      (("a" "2") ("2")) 
;    Scheme] (parse-prefix l3) 
;      (() ("2" "3" "2")) 

(tm-define (parse-number l prefix-repeat)
           (if (null? l)
               (list l prefix-repeat)
               (let ((fc (car l)))
                 (if (and (equal? fc "0") (null? prefix-repeat))
                     (list l '())
                     (if (member fc natural-digits)
                         (parse-number (cdr l) (append prefix-repeat (list fc)))
                         (list l prefix-repeat))))))
(tm-define (parse-prefix l) (parse-number l '()))
(tm-define (parse-posfix l) (parse-number l '()))

; TODO: move-cmds contains redundent cmds
(tm-define move-cmds '(
                       "h" "j" "k" "l" "b" "B" "%" "w"
                           "e" "0" "D" "$" "f" "F" "/" "i" 
                           "s" "a" "Y" "S" ";" "{" "}" "u"
                           "." "x" "o" "O" "P" "A" "#" "*"
                           "p" "n" "N" "I" "G" "W" "B" "&"
                           "R" "L" "(" ")" "[" "]"
                           "return" "backspace"))
(tm-define compound-cmds '("d" "c" "y"))
(tm-define search-cmds '("f" "t" "T" "F"))
(tm-define mark-cmds '("m" "'" "`"))
(tm-define normal-cmd-states
           (append natural-digits move-cmds compound-cmds search-cmds mark-cmds))

; (parse-normal-cmd '("d"))
;     Scheme] (parse-normal-cmd '("d" "2" "w"))
;       (("2" "w") (:cmpd "d"))

(tm-define (parse-normal-cmd l)
           (if (null? l)
               (list l '(#:none #f))
               (let ((fc (car l))) ; fc represents first character
                 (cond
                   ((member fc compound-cmds)
                    (list (cdr l) (list #:cmpd fc)))
                   ((member fc search-cmds)
                    (list (cdr l) (list #:search fc)))
                   ((member fc move-cmds)
                    (list (cdr l) (list #:hjkl fc)))
                   ((member fc mark-cmds)
                    (list (cdr l) (list #:mark fc)))
                   (else (list (cdr l) (list #:invalid fc)))))))



;; The search and mark command can only be used as the first character of a vim command. Thus, the parse-search-char and parse-mark-char simply return the first character.
(tm-define (parse-search-char l)
           (if (null? l) 
               #f
               (car l)))
(tm-define parse-mark-char parse-search-char)

;; now posfix-number

(tm-define directions-simple '("0" "h" "d" "l" "j" "k" "l" "$" "e" "b" "B" "w" "W" "*" "#" "n" "G" "g g"))
(tm-define directions-cmpd search-cmds)

; Examples: 
;   Scheme]  (parse-cmpd-direction '("j" "2"))
;     (("2") :direction-simple "j")
;   Scheme]  (parse-cmpd-direction '("2" "j"))
;     (("j") :direction-invalid "2")
;   Scheme] (parse-cmpd-direction '("z" "f"))
;     (("f") :direction-invalid "z")
;   Scheme] (parse-cmpd-direction '("f" "f"))
;     (("f") :direction-by-search "f")
(tm-define (parse-cmpd-direction l)
           (if (null? l)
               (list l #:no-direction #f)
               (let ((fc (car l))) ; fc represents the first character of the input list
                 (cond
                   ((member fc directions-simple) (list (cdr l) #:direction-simple fc))
                   ((member fc directions-cmpd) (list (cdr l) #:direction-by-search fc))
                   (else (list (cdr l) #:direction-invalid fc))))))

(tm-define (parse-all l)
           (let* ((posfix-with-prefix-number (parse-prefix l))
                  (posfix-string (car posfix-with-prefix-number))
                  (prefix-no (cadr posfix-with-prefix-number))
                  (l2-cmd (parse-normal-cmd posfix-string))
                  (l2 (car l2-cmd))
                  (cmd-type (caadr l2-cmd))
                  (cmd-char (cadr (cadr l2-cmd)))
                  (l3-no (parse-posfix l2))
                  (l3 (car l3-no))
                  (posfix-no (cadr l3-no))
                  (l4 (parse-cmpd-direction l3))
                  (direction-search-char (parse-search-char (car l4))))
             (cond
               ((eqv? cmd-type #:search) (list prefix-no #:search cmd-char
                                               (parse-search-char l2)))
               ((eqv? cmd-type #:mark) (list prefix-no #:mark cmd-char
                                             (parse-mark-char l2)))
               ((eqv? cmd-type #:cmpd)
                (if (equal? #:direction-invalid (cadr l4))
                    #:invalid
                    (append (list prefix-no cmd-type cmd-char posfix-no
                                  (if (equal?  #:direction-by-search (cadr l4))
                                      (list #:direction-by-search (caddr l4)
                                            (parse-search-char (car l4)))
                                      (list (cadr l4) (caddr l4))))
                            )))

               ((eqv? cmd-type #:hjkl) (list prefix-no cmd-type cmd-char))

               ((eqv? cmd-type #:none) (list prefix-no cmd-type cmd-char))
               (else #:invalid))))


; ;; no cmd
; (equal? (parse-all '("2" "3")) (list '("2" "3") #:none #f))
; (equal? (parse-all '("2" "0")) (list '("2" "0") #:none #f))
; (equal? (parse-all '("@")) #:invalid)

; ;; hjkl without no.
; (equal? (parse-all '("0")) (list '() #:hjkl "0"))
; (equal? (parse-all '("j")) (list '() #:hjkl "j"))

; ;; hjkl with no
; (equal? (parse-all '("2" "j")) (list '("2" ) #:hjkl "j"))
; (equal? (parse-all '("2" "3" "j")) (list '("2" "3") #:hjkl "j"))

; ;; search without no
; (equal? (parse-all '("f")) (list '() #:search "f" #f))
; (equal? (parse-all '("F" "c")) (list '() #:search "F" "c"))

; ;; search without no
; (equal? (parse-all '("2" "f")) (list '("2") #:search "f" #f))
; (equal? (parse-all '("2" "F" "c")) (list '("2") #:search "F" "c"))
; (equal? (parse-all '("2" "3" "f")) (list '("2" "3") #:search "f" #f))
; (equal? (parse-all '("2" "3" "F" "c")) (list '("2" "3") #:search "F" "c"))

; ;; cmpd cmd
; (equal? (list '() #:cmpd "d" '() (list #:no-direction #f))
; 	(parse-all '("d")))
; (equal? #:invalid
; 	(parse-all '("d" "@")))

; ;; cmpd cmd with prefix
; (equal? (parse-all '("2" "d"))
; 	(list '("2") #:cmpd "d" '() (list #:no-direction #f)) )
; (equal? (parse-all '("2" "d" "@"))
; 	(list '("2") #:cmpd "d" '() (list #:direction-invalid "@")))

; (equal? (parse-all '("2" "d" "e"))
; 	(list '("2") #:cmpd "d" '() (list #:direction-simple "e")) )

; (equal? (parse-all '("2" "d" "2" "e"))
; 	(list '("2") #:cmpd "d" '("2") (list #:direction-simple "e")) )

; (equal? (parse-all '("2" "d" "0" "e"))
; 	(list '("2") #:cmpd "d" '() (list #:direction-simple "0")) )


; (parse-all '("2" "d" "2" "3" "f"))
; (parse-all '("2" "d" "2" "3" "f" "c"))
; (parse-all '("2" "c" "F" "o"))

(tm-define (invalid? l)
           (equal? #:invalid l))

(tm-define (valid-hjkl? l)
           (and
            (equal? (cadr l) #:hjkl )
            (caddr l)))

(tm-define (hjkl-test l)
           (valid-hjkl? (parse-all l)))

(tm-define (valid-search? l)
           (and
            (equal? (cadr l) #:search )
            (cadddr l)))

(tm-define (valid-mark? l)
           (and
            (equal? (cadr l) #:mark)
            (cadddr l)))

(tm-define hjkl-test (lambda (l) (valid-hjkl? (parse-all l))))
(tm-define search-test (lambda (l) (valid-search? (parse-all l))))


(tm-define (valid-cmpd? l)
           (if (equal? (cadr l) #:cmpd)
               (cAr (cAr l))
               #f))

(tm-define (cmpd-test l)
           (valid-cmpd? (parse-all l)))

(tm-define (valid-cmd? l)
           (or (valid-hjkl? l)
               (valid-search? l)
               (valid-cmpd? l)
               (valid-mark? l)))

(tm-define (valid-test l)
           (valid-cmd? (parse-all l)))

(tm-define (lst->number l)
           (if (null? l) 1
               (string->number (join l))))

(tm-define (prefix-number l)
           (lst->number (car l)))

(tm-define (posfix-number l)
           (if (or (<= (length l) 4) (null? (fourth l))) 1
               (lst->number  (fourth l))))

(tm-define (--repeat-times l)
           (* (prefix-number l) (posfix-number l)))

(tm-define (-repeat-times l)
           (let ((cmd-type (caddr l)))
             (if (or (equal? l #:invalid)
                     (equal? cmd-type #:mark))
                 1
                 (--repeat-times l))))

(tm-define (repeat-times l)
           (-repeat-times (parse-all l)))

;; mark implementation

(tm-define mark-hist '())
;; a list of marks, without pos data
;; car is most recent
(tm-define jump-hist '()) 

(tm-define (-get-mark-pos m l)
           (if (null? l)
               #f
               (if (equal? m (caar l))
                   (cdar l)
                   (-get-mark-pos m (cdr l)))))

(tm-define (get-mark-pos m)
           (-get-mark-pos m mark-hist))

(tm-define (-exist-mark? m l)
           (map (lambda (x) (equal? m (car x))) l))

(tm-define (or-list l)
           (if (null? l) #f
               (or (car l) (or-list (cdr l)))))

(tm-define (exist-mark? m)
           (or-list
            (-exist-mark? m mark-hist)))

(define (-update-mark-hist m pos l)
  (if (exist-mark? m)
      (map
       (lambda (x)
         (if (equal? m (car x))
             (cons m pos)
             x)) l)
      (rcons l (cons m pos) )))

(tm-define (update-mark-hist m pos)
           (set! mark-hist
                 (-update-mark-hist m pos mark-hist)))

(tm-define set-mark-pos update-mark-hist)

(tm-define (mark-quote-jump m)
           (if (exist-mark? m)
               (go-to-path (get-mark-pos m))
               (set-message (string-append "Sorry, no marks named " m) "")))

(tm-define (mark-backquote-jump m)
           (begin
             (mark-quote-jump m)
             (kbd-start-line)))

(tm-define (update-jump-hist m)
           (when (exist-mark? m)
             (set! jump-hist
                   (cons m jump-hist))))

(tm-define (update-insertion-pos)
           (set-mark-pos "^" (cursor-path)))

(tm-define (jump-to-last-insertion)
           (mark-quote-jump "^"))

(tm-define (jump-or-mark c m)
           (cond
             ((equal? c "m") (set-mark-pos m (cursor-path)))
             ((equal? c "'") (mark-quote-jump m))
             ((equal? c "`") (mark-backquote-jump m))))
