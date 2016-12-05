#lang racket

;; lists in racket

(define ll (list 1 2 3)) ;; list is useful for making long lists without using cons

(car ll)

(cdr ll)

;; sum elements of list xs recursively
(define (sum xs)
  (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))))

;; do append recursively
(define (my-append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (my-append (cdr xs) ys))))

;; defining map (a higher order function) recursively
(define (my-map f xs)
  (if (null? xs)
      null
      (cons (f (car xs)) (my-map f (cdr xs)))))

;; lambda notation (function values) is
;; (lambda (x) (+ x x))



