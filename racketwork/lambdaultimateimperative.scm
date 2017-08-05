;; 1. do iteration desugares into TCO recursive calls.
(do ((i 0 (+ 1 i)))
  ((eq? 10 i) 'done)
  (display i))


;; 2. compound statements can be emulated as later
;; functions nested
(define (s1)
  (display "s1 executing\n"))

(define (s2)
  (display "s2 executing\n"))

(define (s3)
  (display "s3 executing\n"))

(define (s4)
  (display "s4 executing\n"))


;; an imperative compound statement is a statement
;; with many statements in sequential order of execution
;; e.g.
;; begin
;;   statement1;
;;   statement2;
;; end

;; we convert it to imperative like sequential execution
;; by nesting later statements inside lambdas
(
 (lambda (dummy)
   (s2))
 (s1)
)

(display "-------------\n")

((lambda (d1)
   ((lambda (d2)
      (s3))
    (s2)))
 (s1))

(display "------------\n")
;; executing four statements in sequential order.
;; lambda with d2 is continuation of s1
;; lambda with d3 is continuation of s2
((lambda (d1)
   ((lambda (d2)
      ((lambda (d3)
         (s4))
       (s3)))
    (s2)))
 (s1))

;; a key aspect to notice syntactially is
;; ((lambda will usually evaluate the contents of lambda
;; where as (lambda just returns procedure
(lambda (x) (+ x 1)) ;; just returns <procedure>
((lambda (x) (+ x 1)) 98) ;; returns 99


;; Continuation: a function to call with the answer,
;; rather than return a value, imagine you can never return
;; you only have a function given to you to call the
;; answer with

;; recursive function with continuation passing style
(define (fact N C)
  (if (= N 0)
      (C 1)
      (fact (- N 1)
            (lambda (a) (C (* N a))))))

;; see here how I use C instead of return
(define (fact2 N C)
  (if (= N 0)
      (C 1)
      (C (* (fact (- N 1) C) N))))

;; check by passing identity function as continuation
;; which ten works like regular return.

(fact 4 (lambda (u) u))  ;; 24
(fact2 5 (lambda (r) r))  ;; 120

