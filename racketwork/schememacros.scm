
;; -*- geiser-scheme-implementation: guile -*-

;; http://www.shido.info/lisp/scheme_syntax_e.html
;; define-syntax + syntax-rules

;; At any places () can be replaced by [] as you wish
;; e.g. [[lambda [x] [* x x]] 10] will give answer 100

(define-syntax nil!
  (syntax-rules ()
    [(_ x) (set! x '())]))




