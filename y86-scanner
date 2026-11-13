#lang plai

(define-type Token
  [IDENTIFIER (i symbol?)] ;; (A-Za-z|_|[0-9])*
  [LPAREN]                        ;; (
  [RPAREN]                        ;; )
  [COMMA]                         ;; ,
  [PERCENT])                      ;; %

#;
(define (fn-for-token t)
  (type-case Token t
    [IDENTIFIER (i) #f]
    [LPAREN () #f]
    [RPAREN () #f]
    [COMMA () #f]
    [PERCENT () #f]))

;; Token -> String
;; given token, produce the string it represents
(define (token->string t)
  (type-case Token t
    [IDENTIFIER (i) (symbol->string i)]
    [LPAREN () "("]
    [RPAREN () ")"]
    [COMMA () ","]
    [PERCENT () "%"]))

;; (listof Token) -> String
;; given listof tokens, produce string it represents
(define (tokens->string lot)
  (string-append " " (foldr (λ (v l) (string-append v " " l)) "" (map token->string lot))))

;; String -> (listof Char)
;; produce a listof characters from the given file
(define (file->list fname)
  (string->list (file->string fname)))

;; (listof Char) -> (listof Token)
