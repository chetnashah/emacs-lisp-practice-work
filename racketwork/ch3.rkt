
#lang racket

(define balance 100)

(define (withdraw amount)
  (if (> balance amount)
      (begin
        (set! balance (- balance amount))
        balance)
      "Insufficient funds"))


;; functions with closures can act like objects with state variables!!
(define new-withdraw
  (let ([new-balance 200])
    (lambda (amount) ;; this function closes new-balance as a private var
      (if (> new-balance amount)
          (begin
            (set! new-balance (- new-balance amount))
            new-balance)
          "Insufficient funds with new-balance"))
    ))


;; full fledged account object with private var balance
;; and methods deposit and withdraw. Note dispatch method is returned

(define (make-account balance)
  (define (withdraw amount) ;; start getting comfortable defining functions
    (if (> balance amount)  ;; in functions that close over free vars(balance)
        (begin
          (set! balance (- balance amount))
          balance)
        "In sufficient funds"))
  (define (deposit amount)
    (begin
      (set! balance (+ balance amount))
      balance))
  (define (getBalance)
    balance)
  (define (dispatch m) ;; dispatch returns procedure matching the name passed
    (cond [(eq? m 'withdraw) withdraw]
          [(eq? m 'deposit) deposit]
          [(eq? m 'getBalance) getBalance]
          [#t (error "Unknown method requested")]))
  dispatch) ;; dispatch method represents account object 

;; try
;; (define ac1 (make-account 999))
;; ((ac1 'withdraw) 333)
;; ((ac1 'getBalance))

;; Ex 3.1
(define (make-accumulator initial)
  (define (adder num)
    (begin
      (set! initial (+ initial num))
      initial))
  adder)

(define A (make-accumulator 5))

(A 10)
(A 19)


;; Ex 3.2
;; Write a procedure make-monitored that takes as input a procedure f
;; that itself takes one input.
;; The result returned by make-monitored is third procedure say mf
;; that keeps track number of times f was called(some internal counter)

;; if input to mf is special symbol how-many-calls? then return ctr value
;; if input to mf is special symbol reset-count then mf resets counter to 0
;; if input is anything else, apply input to f and return

(define (make-monitored f)
  (let ([calls 0])
    (define (how-many-calls)
      calls)
    (define (reset-count)
      (set! calls 0))
    (define (incr-count-and-call-f x)
      (begin
        (set! calls (+ 1 calls))
        (f x)))
    (define (mf x)
      (cond [(eq? x 'how-many-calls?) (how-many-calls)]
            [(eq? x 'reset-count) (reset-count)]
            [#t (incr-count-and-call-f x)]))
    mf))

;; account that checks against a password
(define (make-account-with-p balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (cond [(not (eq? p password) (lambda (x) "incorrect password"))]
          [(eq? m 'withdraw) withdraw]
          [(eq? m 'deposit) deposit]
          [#t (error "unknown request")]))
  dispatch)


;;; required reading : environment model of evaluation
;;1. a procedure is creaated by evaluating a lambda expression,
;;  relative to a given environment. The resulting procedure object
;;  is a pair consisting of text of lambda expression and pointer to
;;  environment in which procedure was created.
;;
;;2. A procedure object is applied to a set of arguments by constructing
;;   a frame, binding the formal parameters of the procedure to the
;;   arguments of the call, and then evaluating the body of the procedure
;;   in the context of new environment constructed. The new frame has its
;;   enclosing environment, the environment part of procedure object being
;;   applied.





;; Equvialence between let and lambda
;; the let syntax
;; (let ([<v1> <e1>]) <body>)
;; is syntactic sugar for
;; ((lambda (<v1>) <body>) <e1>)

;; an example use of internal definitions for defining my-sqrt


;; see the env model diagram in the book
(define (my-sqrt x)
  (define (square x)
    (* x x))
  (define (average . numbers)
    (/ (apply + numbers) (length numbers)))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;; Two key points for internal definitions

;; 1. The names of local procedures do not interfere with names external
;; to the enclosing procedure because the local procedure names will be bound
;; in the frame that main procedure creates when it is run and hence shadow
;; external procedures with the same name

;; 2. The local procedures can access the arguments of the enclosing
;; procedure simply by using parameter names as free variables.
;; This is because the body of the local procedure is evaluated in an
;; environment that is subordinate to the environment for the enclosing
;; procedure.

;; In terms of set! stuff, racket has different approach
;; http://blog.racket-lang.org/2007/11/getting-rid-of-set-car-and-set-cdr.html
;; To use mutation operations like set-car! and set-cdr! read following:
;; (require scheme/mpair)
;; Those procedures were moved to a different module and renamed to mcons, mcar, mcdr, set-mcar!, set-mcdr!, mlist to emphasize that they operate on mutable data, unlike their immutable counterparts.

(require scheme/mpair)

;; ex 3.12

;; functional non-mutating append
(define (append x y)
  (if (null? x)
      y
      (mcons (mcar x) (append (mcdr x) y))))

;; get last-pair of a list
(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))

;; mutating append
(define (append! x y)
  (set-mcdr! (last-pair x) y)
  x)

(define x (mlist 'a 'b))
(define y (mlist 'c 'd))
(define z (append x y))
z
(mcdr x)
(define w (append! x y))
w
(mcdr x)

;; ex 3.13
(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)

(define lp (make-cycle (mlist 'a 'b 'c)))

