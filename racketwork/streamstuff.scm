;; -*- geiser-scheme-implementation: guile -*-

;;
;; As a data abstraction, streams are same as lists.
;; With ordinary lists, both car and cdr are evaluated
;; at construction.
;; With steams, cdr is evaluated at selection time


;; implementation of streams will be based
;; on special form called 'delay'

;; Evaluating (delay <exp>) returns delayed object,
;; like a promise of exp evaluation result in future.
;; (delay <expr>) => promise

;; Companion of delay is force,
;; force takes a delayed object/promise and performs
;; evaluation, in effect forcing delay to fulfill the promise
;; (force <promise>) => value



;; (cons-stream a b) is equivalent to (cons a (delay b))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

;; stream-cdr selects cdr of the pair and evaluates
;; delayed expression found there to obtain rest of stream

;; DELAY and FORCE SEMANTICS
;; delay must be a macro to prevent eager eval of exp
;; delay puts expression in lambda with no-args to delay eval
;; (define (delay exp)
;;   (lambda () exp))
;;
;; force implementation will also usually memoize result
;; in addition to below
;; force calls promise with no arguments
;; (define (force promise)
;;   (promise))

;; delay and force are already present in scheme R5RS
;; implementation which are correct as desired and
;; hence we will use them.

;; e.g. try out
;; (define heyer (cons 1 (delay (display 'hey))))
;; (force (cdr heyer))
;; output => 'hey

;; cons-stream needs to be a macro;
;; Why ?
;; (define (cons-stream a b)
;;   (cons a (delay b)))
;; because while calling itself
;; b is evaluated

;; e.g. (cons-stream 1 (display 'hi))
;; will display hi instead of delaying its evaluation




(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))






