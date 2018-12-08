#lang racket

(require test-engine/racket-tests)

(struct pr [fst snd] #:transparent) ;; pair is a keyword
;; A ConsOrEmpty is one of:
;; '()
;; (make-pair Any ConsOrEmpty)

;; our-cons:: Any Any -> ConsOrEmpty
;; cons is a checked constructor that produces pairs
(define (our-cons a-value a-list)
  (cond
    [(empty? a-list) (pr a-value a-list)]
    [(pair? a-list) (pr a-value a-list)]
    [#t (error "second argument to our-cons has to be list")]))

(check-expect (our-cons 'a '()) '(a))
(check-expect (our-cons 'a '(1 2)) '(a 1 2))
(check-error (our-cons 1 2))

;; similarily selectors of cons i.e. car and cdr have to be
;; checked
(define (our-first a-list)
  (cond
    [(empty? a-list) (error "cannot get first of an empty list")]
    [(pr? a-list) (pr-fst a-list)]))

(check-error (our-first '()))
(check-expect (our-first (our-cons 'a '())) 'a)

(define (our-rest a-list)
  (cond
    [(empty? a-list) (error "cannot get rest of empty list")]
    [(pr? a-list) (pr-snd a-list)]))

(check-error (our-rest '()))
(check-expect (our-rest (our-cons 'a '())) '())
(check-expect (our-rest (our-cons 'a (our-cons 'b '()))) (our-cons 'b '()))

