#lang racket

(define x 3)

(define y (+ x 2))

;; binding variable cube1 to be bound to a function
(define cube1
  (lambda (x)
    (* x x x)))

;; no need to write lambda
;; exactly same as above definition
(define (cube2 x)
  (* x x x))

;; conditianl if takes three arguments
;; first is condition, second is iftrue part, third is iffalse part
(define (pow1 x y)
  (if (= y 0)
      1
      (* x (pow1 x (- y 1)))))

