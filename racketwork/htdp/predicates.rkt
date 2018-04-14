#lang racket

;; although we cannot define sum types in racket,
;; it is useful to create predicates that can check for them

;; a light is either
;; "red"
;; "green"
;; "yellow"

;; light? :: Any -> Boolean
(define (light? x)
  (cond [(string? x) (or (string=? "red" x)
                         (string=? "green" x)
                         (string=? "yellow" x))]
        [#t #false]))

;; although light is represented as string
;; not all strings are lights, use light? to write checked programs
(light? "oo")
(light? "green")

;; the above predicate can be used in larger constructs
(define (light=? aval bval)
  (if (and (light? aval) (light? bval))
      (string=? aval bval)
      (error "one of the arguments is not light")))

(light=? "red" "blue")
