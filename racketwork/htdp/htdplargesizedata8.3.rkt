#lang racket

(require test-engine/racket-tests)

;; function that checks if a-list-of-names contains "Flatt"
;; List-of-names -> Boolean
(define (contains-flatt? a-list-of-names)
  (cond
    [(empty? a-list-of-names) #f]
    [(not (pair? a-list-of-names)) (error "expected a list:")]
    [(equal? (car a-list-of-names) "Flatt") #t]
    [#t (contains-flatt? (cdr a-list-of-names))]))

(check-expect (contains-flatt? '()) #f)
(check-expect (contains-flatt? '("hi" "hello")) #f)
(check-expect (contains-flatt? '("hi" "Flatt")) #t)
(check-error (contains-flatt? 2))

(test)