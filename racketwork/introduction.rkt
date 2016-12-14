#lang racket



(define s "hello") ;; like val def in ml, defun in lisp

(define x 3)

(define y (+ x 2)) ;;lisp like prefix notation for calling funs

(define cube1
  (lambda (x) (* x (* x x)))) ;; variable cube1 bound to a lambda

(define cube2
  (lambda (x) (* x x x)));; no currying, * takes any no of args

(define (cube3 x y z)
  (* x x x)) ;; define without a lambda being bound to variable

;; recursive function like lisp with an if else branch
(define (pow1 x y)
  (if (= y 0)
      1
      (* x (pow1 x (- y 1)))))

;; multi argument currying lambda style
;; should be called as
;; > ((pow2 2) 3)
(define pow2
  (lambda (x)
    (lambda (y)
      (pow1 x y))))

;; Racket lists
;; Empty List : null
;; Cons constructor : cons
;; Access head of list : car
;; Access tail : cdr
;; Check for empty null?
