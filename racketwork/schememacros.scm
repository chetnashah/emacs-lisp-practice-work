
;; -*- geiser-scheme-implementation: guile -*-

;; http://www.shido.info/lisp/scheme_syntax_e.html
;; define-syntax + syntax-rules

;; At any places () can be replaced by [] as you wish
;; e.g. [[lambda [x] [* x x]] 10] will give answer 100

(define-syntax nil!
  (syntax-rules ()
    [(_ x) (set! x '())]))

;; the general syntax can be shown as
;; (define-syntax id
;;   (syntax-rules (literal-id ...)
;;     [pattern template]
;;     [pattern2 template2]
;;     ...))

(define-syntax incf
  (syntax-rules ()
    [(_ x) (begin (set! x (+ x 1)) x)]
    [(_ x i) (begin (set! x (+ x i)) x)]
    ))

(define bb 1)
(incf bb)
(display bb)
(incf bb 11)
(display bb)

;; lets play some more

(define-syntax kwote
  (syntax-rules ()
    [(kwote exp1) (quote exp1)]))

;; (kwote (foo bar)) will not evaluate foo


;; see recursive definition of macros





