#lang racket

(require test-engine/racket-tests)

;; add-to-end :: List-of-Item, Item -> List-of-Item
;; adds given item to the end of the list
(define (add-to-end xs x)
  (cond
    [(empty? xs) (cons x '())]
    [#t (cons (first xs) (add-to-end (rest xs) x))]))

(check-expect (add-to-end '() 2) (cons 2 '()))
(check-expect (add-to-end '(1 2 3) 4) '(1 2 3 4))

;; rev :: List-of-Item -> List-of-Item
;; a non tail recursive version of reverse list,
;; makes use of add-to-end

(define (rev xs)
  (cond
    [(empty? xs) '()]
    [#t (add-to-end (rev (rest xs)) (first xs))]))

(check-expect (rev '()) '())
(check-expect (rev '(1)) '(1))
(check-expect (rev '(1 2 3 4)) '(4 3 2 1))

(test)