(define (logical-not s)
  (cond [(= s 0) 1]
        [(= s 1) 0]
        [#t (error "logical-and error")]))


(define (logical-and x y)
  (cond [(and (= x 1) (= y 1)) 1]
        [(and (= x 1) (= y 0)) 0]
        [(and (= x 0) (= y 1)) 0]
        [(and (= x 0) (= y 0)) 0]
        [#t (error "logical-not error")]))

;; wires are made using make-wire

;; (get-signal <wire>) returns signal on the given wire

;; (set-signal <wire> <new val>) sets new val on the wire

;; (add-action <wire> <procedure without args>)
;; says whenever signal on wire changes, run the procedure

;; (after-delay <delay> <procedure>)
;; run the given procedure after given delay amount of time passes


