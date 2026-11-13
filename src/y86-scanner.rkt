#lang racket
(require plai/datatype)
(require plai/test-harness)

(print-only-errors)

(define-type Token
  [IDENTIFIER (i symbol?)]        ;; [A-Za-z]\(A-Za-z|_|[0-9])*
  [LPAREN]                        ;; (
  [RPAREN]                        ;; )
  [PERCENT]                       ;; %
  [DOLLAR]                        ;; $
  [INTEGER (n integer?)]          ;; [0-9]+
  [HEX]                           ;; 0x
  )                      

#;
(define (fn-for-token t)
  (type-case Token t
    [IDENTIFIER (i) #f]
    [LPAREN () #f]
    [RPAREN () #f]
    [PERCENT () #f]
    [DOLLAR () #f]
    [INTEGER (n integer?)]
    [HEX]))

;; Token -> String
;; given token, produce the string it represents
(define (token->string t)
  (type-case Token t
    [IDENTIFIER (i) (symbol->string i)]
    [LPAREN () "("]
    [RPAREN () ")"]
    [PERCENT () "%"]
    [DOLLAR () "$"]
    [INTEGER (n) (number->string n)]
    [HEX () "0x"]))

;; TODO: labels ex. loop-start:

;; (listof Token) -> String
;; given listof tokens, produce string it represents
(define (tokens->string lot)
  (string-append " " (foldr (λ (v l) (string-append v " " l)) "" (map token->string lot))))

;; (listof Char) -> (listof Token)
;; produce the listof tokens corresponding to given list of characters
;; EFFECTS: signal error if fails to read syntax
(define (scan loc0)
  ;; Accumulators:
  ;; loc is (listof Char)
  ;; Invariant: remaining chars to be scanned
  ;; curr is (listof Char) or false
  ;; Invariant: sequential seen chars 
  (local [(define (start loc curr)
            (cond
              [(empty? loc) empty]
              [else
               (match (first loc)
                 [#\( (cons (LPAREN) (start (rest loc) false))]
                 [#\) (cons (RPAREN) (start (rest loc) false))]
                 [#\% (cons (PERCENT) (start (rest loc) false))]
                 [#\$ (cons (DOLLAR) (start (rest loc) false))]
                 [#\# (line-comment (rest loc) (string (first loc)))] ;; # indicates comment
                 [#\. (line-comment (rest loc) (string (first loc)))] ;; . indicates assembler directive for memory setup
                 [#\, (start (rest loc) false)]
                 [else
                  (cond
                    [(char-numeric? (first loc))
                     (in-numeric (rest loc) (string (first loc)))]
                    [(char-alphabetic? (first loc))
                     (in-ident (rest loc) (string (first loc)))]
                    [(char-whitespace? (first loc))
                     (start (rest loc) false)]
                    [else (error "Bad character" (first loc))])])]))

          ;; (listof Char) String -> (listof Token)
          ;; parse int or hex token and continue recursing
          (define (in-numeric loc curr)
            (cond
              [(empty? loc) (cons (INTEGER (string->number curr))
                                  (start empty false))]
              [(string=? curr "0x") (cons (HEX) (start loc false))]
              [(or (char-numeric? (first loc))
                   (and (char=? #\x (first loc))
                        (string=? curr "0")))
               (in-numeric (rest loc)
                           (string-append curr (string (first loc))))]
              [else
               (cons (INTEGER (string->number curr))
                     (start loc false))]))
          
          ;; (listof Char) String -> (listof Token)
          ;; parse identifier token and continue recursing
          (define (in-ident loc curr)
            (cond
              [(empty? loc) (cons (IDENTIFIER (string->symbol curr))
                                  (start empty false))]
              [(or (char-alphabetic? (first loc))
                   (char-numeric? (first loc))
                   (char=? (first loc) #\_))
               (in-ident (rest loc)
                         (string-append curr (string (first loc))))]
              [else
               (cons (IDENTIFIER (string->symbol curr))
                     (start loc false))]))
          
          ;; (listof Char) String -> (listof Token)
          ;; handle a line comment
          (define (line-comment loc curr)
            (cond
              [(empty? loc) (start empty false)]
              [(char=? (first loc) #\newline) (start (rest loc) false)]
              [else (line-comment (rest loc)
                                  (string-append curr (string (first loc))))]))
          ]
    (start loc0 false)))

;; String -> String
;; produce string from the given file
(define (file->string fname)
  (port->string (open-input-file fname)))

;; String -> (listof Char)
;; produce a listof characters from the given file
(define (file->list fname)
  (string->list (file->string fname)))

;; String -> (listof Token)
;; scan a given string
(define (scan-string str)
  (scan (string->list str)))

;; String -> (listof Token)
;; scan a given file
(define (scan-file fname)
  (scan-string (file->string fname)))

(test (scan-string "") empty)
(test (scan-string "(") (list (LPAREN)))
(test (scan-string "()") (list (LPAREN) (RPAREN)))
(test (scan-string "(77)22d65") (list (LPAREN) (INTEGER 77) (RPAREN)
                                      (INTEGER 22) (IDENTIFIER 'd65)))
(test (scan-string "44#fee fi fo fum\ndid")
      (list (INTEGER 44) (IDENTIFIER 'did)))
(test (scan-string "irmovq $123, %r16 # load r16 with 123")
      (list (IDENTIFIER 'irmovq) (DOLLAR) (INTEGER 123) (PERCENT) (IDENTIFIER 'r16)))