#lang racket

;;Dynamic typing nature

;; dynamic typing helps us be clever
(define xs (list 4 5 6))

(define ys (list (list 3 4) 5 6 (list 4 (list 3 2 3))))

;; making list of arbitrary types : no restriction!!
(define zs (list "hi" #f 2))

;; sum arbitrarily nested lists
;; restraint every element should either be list of numbers or a number
(define (sum1 xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (sum1 (cdr xs)))
          (+ (sum1 (car xs)) (sum1 (cdr xs))))))

;; sum arbitrarily nested lists
;; no constraints
;; not foolproof thought , e.g. (sum2 "hi")
(define (sum2 xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (sum2 (cdr xs)))
          (if (list? (car xs))
              (+ (sum2 (car xs)) (sum2 (cdr xs)))
              (sum2 (cdr xs))))))

(sum2 (list (list "hi" (list 3)) 3 5)) ;; "hi" is ignored by sum2, because we only process types confirmed by number? and list?

