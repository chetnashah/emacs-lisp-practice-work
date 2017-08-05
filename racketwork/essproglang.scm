

;; inductive specification of sets
;; base case + building function closed over set

;; (in-S n)  ::: Nat -> Bool
;; in-S if n = 0 or n - 3 belongs to S
;; its a top-down specification
(define (in-S? n)
  (cond ((zero? n) #t)
        ((>= n 3) (in-S? (- n 3)))
        (#t #f)))

