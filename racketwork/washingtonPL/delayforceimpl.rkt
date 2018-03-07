
#lang racket
;; delay and force help us do lazy evaluations.
;; give thunk to my-delay and that thunk will only be evaluated once ever,
;; subsequent requests to promise return the stored value that was the result of the thunk
;; implementing delay and force with mcons

;; thunk -> promise
(define (my-delay th)
  (mcons #f th)) ;; first item in cons tells if it is evaluated, second is think evaluation result

;; the mcons acts like a promise whose first value is evaluation status
;; and second value is actual evaluation result of thunk

;; promise -> value
(define (my-force p)
  (if (mcar p)
      (mcdr p)
      (begin
        (set-mcar! p #t)
        (set-mcdr! p ((mcdr p)))
        (mcdr p))))

;; check with below
;; (define pp (my-delay (lambda x 999)))
;; (define ab (my-force pp))