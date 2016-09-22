
(+)  ;; evaluates to 0
(+ 2) ;; evaluates to 2
(+ 2 3) ;; evaluates to 5

;; expressions can be nested
(/ (- 7 1) (- 4 2))

;; evaluation semantics

;; 1. firrst arguments are evaluated , from left to right
;; 2. the values evaluated in these arguments are thne passed to the function

( / (-7 1) (- 4 2))

;; quote is a special operator : meaning distinct evaluation rule of its own -> do nothing
;; protecting expressions from evaluation
(quote (+ 3 5))
'( / 5 6)


;;-------------------------------------data types --------------------------------------

;; numbers evaluate to themselves
34

;; strings evaluate to themselves
"hi hello"

;; symbols are words, they do not usually evaluate to themselves, if you refer it quote it
'Arch

;; List is represented as one or more elements enclosed in parentheses
'(1 2 3) ;; quoting it since 1 is not a function if I try to evaluate it

;; quote protects the whole expression, including child expressions, so no need to quote child expressions
'(the list has (A B C) expression inside of it)

;; you can build lists by calling list as a function and arguments are packed into parenthesesized list
(list 1 2 3) ;; evaluates to give (1 2 3)
(list 'my (+ 2 3) "sons") ;; evaluates to (my 5 "sons")


;; secret : Lisp programs are expressed as lists -> which means lisp programs can generate lisp code
;; code is data; data is code!!!

;; empty lists
() ;; nil
nil ;; nil
'() ;; nil
'nil ;; nil
;; nil evaluates to itself





