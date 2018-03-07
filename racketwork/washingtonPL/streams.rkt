#lang racket

;; a stream is an infinite sequence of values.
;; key idea: use thunk/promise to delay creating rest of sequence.

;; stream is a thunk/pair, where first cell is value second cell is remaining thunk/stream,
;; which on evaluation makes a similar cell, with this behaviour repeated.

;; lets create
;; 1 2 4 8 16 32 64 128... powers of 2
;; (1 . thunk)
;; (1 . ( 2 . thunk ))
;; ...

;; our stream is a thunk, which on calling returns pair.

(define ones (lambda () (cons 1 ones))) ;; ones is a procedure

;; natural numbers that start at x

;; alternated definition of f -> (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))]))
(define (f x) (cons x (lambda () (f (+ x 1)))))
(define nats (lambda () (f 1)))

;; all streams have recursive formulation like above

(define powers-of-2
  (letrec ([f (lambda (x) (cons (expt 2 x) (lambda () (f (+ 1 x)))))])
    (lambda () (f 1))))

;; implement stream-map and stream-filter
;; stream -> fun -> stream
(define (stream-map ff s)
  (let ([pr (s)])
    (lambda () (cons (ff (car pr)) (stream-map ff (cdr pr))))))

(define (add1 x) (+ 1 x))
(define twos (stream-map add1 ones))

(define (print-stream-pred s test)
  (let ([pr (s)])
    (if (test (car pr))
        '()
        (cons (car pr) (print-stream-pred (cdr pr) test)))))

(print-stream-pred powers-of-2 (lambda (y) (> y 50000)))

(define (stream-filter pred s)
  (let ([pr (s)])
    (if (pred (car pr))
        pr
        (stream-filter pred (cdr pr)))))


(stream-filter (lambda (y) (> y 100)) powers-of-2)


;; sieve of erastosthenes is a great example that results in a stream of prime numbers
;; TODO

