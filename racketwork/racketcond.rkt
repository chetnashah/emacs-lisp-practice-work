#lang racket

;; sum version using cond instead of if else
(define (sum3 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum3 (cdr xs)))]
        [#t (+ (sum3 (car xs)) (sum3 (cdr xs)))]))

;; usually a good style to put #t as last in cond like a default in switch case other wise
;; cond might return something weird

;; anything other than #f is considered as truthy
;; even 0,null are truthy in racket

(if 0 1 2) ;; prints 1

(if null 1 2) ;; prints 1

(if #f 1 2) ;; prints 2