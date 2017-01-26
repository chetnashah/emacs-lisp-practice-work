#lang racket

(define (sum xs)
  (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))))

;; lambdas are nameless functions
;; or expressions that return procedures
;; standard notation is (lambda (arg1 ...) body)
(lambda (x)
  (+ x x x))

;; lambda application
((lambda (x)
   (* x x)) 9)

;; give a name/id my-cuber to the nameless function value
(define my-cuber (lambda (y)
                   (* y y y)))

(my-cuber 5)

;; similar is following
(define (my-other-cuber z)
  (* z z z))

(my-other-cuber 6)

;; alternate lambda notation is following (for var-arg proc.)
;; (lambda rest-id body)
;; this kind of lambda notation can only have single rest-id
;; resulting function can take any number of arguments
;; all put into list named rest-id

((lambda xs
   (sum xs))
 1 1 1 1 1 1)

;; use of apply
;; one would always like functions that take
;; varArgs to take input list of items and vice versa


;; e.g. (+ (list 2 3 4 5)) breaks where + is varArg
;; but (sum (list 2 3 4 5)) works 

;; similarly (+ 2 3 4 5) works
;; but (sum 2 3 4 5 6) doesnt work

;; apply takes a function and a list
;; and calls function with items in the list
(apply + (list 1 1 1 1 1 1 1))
;; btw javascript apply is same

;; similarly for sum, lambda notation with rest-id,
;; where all are arguments passed are collected in list ys
(define msum
  (lambda ys
    (sum ys)))

(msum 1 1 1)

;; so apply and lambda with rest-id in one case and use
;; other for vice versa