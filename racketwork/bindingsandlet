#lang racket

;; let expressions and bindings

;; (let ([x1 e1]
;;       [x2 e2]
;;       ...
;;       [xn en])
;;   body)
;; body evaluates with the environment enriched by bindigs of x1, x2

(define (max-of-list xs)
  (cond [(null? xs) (error "max-of-list of empty list not possible")]
        [(null? (cdr xs)) (car xs)] ;; here we check if list has only single element
        [#t (let ([tlans (max-of-list (cdr xs))])
              (if (> tlans (car xs))
                  tlans
                  (car xs)))]))

;; racket has 4 ways to define local variables :
;; let - different from ML's let
;; let* - same as ML's let
;; letrec
;; define

;; let binding section having variables are always fetched from environment before let expression
;; e.g.

(define (silly-double x)
  (let ([x (+ x 3)] ;; uses outside x value in the expr part
        [y (+ x 2)]) ;; still uses outside x value in expr part
    (+ x y -5)))

;; let* binding section expr are evaluated in environment produced from previous bindings
;; can repeat bindings later ones shadow, similar behaviour could be acheived with nesting let
(define (silly-double2 x)
  (let* ([x (+ x 3)] ;; uses outside x 
         [y (+ x 2)]) ;; uses inner x
    (+ x y -8)))

;; top level bindings and referencing variables

(define (f x) (+ x (* x b))) ;; forward reference okay here as function body is not evaluated here
(define b 3)
(define c (+ b 4)) ;; backward reference okay
;; (define d (+ e 5)) ;; not okay, to define binding of d , the expr (+ e 5) needs to evaluated, which is not possible for fwd
(define e 5)
;; (define f 4) ;; not allowed, duplicate definition not allowed