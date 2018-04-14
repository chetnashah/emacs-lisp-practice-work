#lang racket

;; racket struct help generate tagged bag of named-fields (as a product of values)
;; help you introduce a cartesian product of values (of fields), with a tagname

(struct posn (x y) #:transparent) ;; posn is struct-name and x, y are its fields
(struct posn2 [x y] #:transparent) ;; also a valid syntax

;; structure types by default are opaque
;; using #:transparent with struct is useful for implicit implementation of toString and equals?
;; an instance of transparent structure prints like  a call to constructor

;; defining struct introduces following functions
;; 1. struct constructor to make new values of struct
(define point1 (posn 2 3))

;; 2. struct predicate, to check if given value confirms to a struct
(posn? point1)
(posn? 11)

;; 3. field accessors for each field in struct, structname-fieldname
(posn-x point1)
(posn-y point1)

(struct opaquept [x y])
(equal? (opaquept 2 5) (opaquept 2 5));; #false, cannot do structural equality with opaque structs, it only does eq?
(equal? (posn 6 7) (posn 6 7));; #true, structural equality only works with transparent structs


;; since struct fields are just values, struct values can be nested inside other struct values
;; e.g.
(struct contactinfo [phonenum emailid address])
(struct person [name age cinfo])

;; see contactinfo struct value is nested inside person struct value
(define p1 (person "chet" 50 (contactinfo "80980943" "chet@abc.dom" "oieuwr,sdfo")))





