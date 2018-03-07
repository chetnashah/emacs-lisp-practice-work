#lang racket

;; memoization
;; useful caching for pure functions, since same input gives same output then why compute again and again

;; KeyIdea: keep a table of seen inputs with corresponding results, the state i.e. table is part of closure of original fn

(define (fibonacci1 x)
  (if (or (= x 1) (= x 2))
      1
      (+ (fibonacci1 (- x 1))
         (fibonacci1 (- x 2)))))

(define (memo fn)
  (let ([tbl (make-hash)])
    (lambda (x)
      (if (hash-has-key? tbl x)
          (hash-ref tbl x)
          (begin
            (hash-set! tbl x (fn x))
            (hash-ref tbl x))))))

;; does not do recursive memoization, only toplevel memoization
(define memofib (memo fibonacci1))

;;TODO do recursive memoization