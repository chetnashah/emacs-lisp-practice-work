
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



;; ways to make lists -

;; construct list with "cons"
;; cons takes an atom and a list and puts atom as head of list

(cons 'a '(b c d))      ;; (a b c d)
(cons 'a (cons 'b nil)) ;; (a b)
(cons 'a nil)           ;; (a)

;; alternatively you can use the list function
(list 'a 'b 'c 'd) ;; (a b c d)
(list 'a '(b c d)) ;; (a (b c d))

;; car and cdr
;; car returns the head of list as an atom
;; cdr returns the tail of list as list

(car '(a b c d)) ;; a
(cdr '(a b c d)) ;; (b c d)

;; see list predicates

;; if special operator
;; (if predicate b1 b2) -> if predicate is true, follow b1 else follow b2

(if (listp 27)
    (+ 1 2)
  (* 5 2))  ;; 10

;; everything except nil is truthy, here 27 is truthy

(if 27 1 2) ;; 1

(and nil 5 5 5)       ;; returns nil if any one is false and stop checking immediatel
(and 3 t 4 (+ 33 33)) ;; if all are t, returns last evaluated
(or 8 t 3)            ;; does not evaluate rest when finds t, returns 8
(or 7 9 nil)          ;; returns 7

;; 'and' and 'or' are macros, like special operators, macros can circumvent evaluation rule


;;-----------------------------defining functions ------------------------------

;; use defun to define functions
;; (defun fnName argList procedure)
;; not fnName and argList won't be evaluated so you don't need to quote them
(defun printtoscreen (m)
  (message m))

(printtoscreen "jayshah")

;; you can say the above function binds symbol printtoscreen to a lambda with arg m and body message m

;; a list has to be quoted othewise it will be treated as code

;; structural recursion with car and cdr
;; write a function our-member that takes an atom and a list and returns t if atom is member of list



;; ------------------------------------------ variables and assignment ---------------------------------

;; let helps you create local variables
;; bind variables according to varlist, then eval body
;; varlist is like ((Symbol Valueform) (Symbol Valueform) ... )
;; (let varlist body..)
(let ((x 1) (y 2))
  (+ x y))               ;; 3

;;Question : can valueform be an expression -> let's try
(let ((a (+ 88 88)) (b 9))
  (+ a b))
;; yes valueform can itself be an expression

;; return value of let is evaluation of body

(defun ask-number ()
  (message "Please enter a number")
  (let ((val (read)))
    (if (numberp val)
	val
      (ask-number))))

(ask-number)

;; let variable names can cause shadowing so it is a good practice
;; to keep global names like *globaname* etc.

;;-------------------assignment------------------------------

;; main idea is set, setq, setf
;; set is set
;; setq is setquote
;; setf is setField -> a generalized version of setq
(set ss '(1 2 3 4))         ;; error ss has no value
(set (quote ss) '(1 2 3 4)) ;; OK
(setq ss '(1 2 3 4))        ;; OK same as above
(setf ss '(1 2 3 4))        ;; OK, similar as above

;; we will come back to setf
