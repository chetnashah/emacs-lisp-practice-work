
;; taken from http://www.cs.cmu.edu/~ggordon/lisp-hints.txt

;; symbols and bindings
;; symbols can be bound to values
;; symbols can globally be bound to values using setq


(setq a 5) ; 5

a ; 5

;; symbols can be locall bound to values using let, shadowing global value of same symbol locally inside
(let ((a 99)) a) ;; 99

a ; 5

;; when symbol needs to be evaluated, it will evaluate to its bound value
(+ 4 a) ;; 9

b ;; if you try to evaluate a symbol that is not bound to a value, it will result in error



;;;;;; keywords
;; any symbol that starts with a colon is a "keyword" and keyword evaluate to themselvs

:this-is-keyword ;; :this-is-keyword
:me-too ;; :me-too



;;;;;;;;; Conses
;; a cons is nothing but a pair, or a two field tuple, like <K,V>

(cons 4 5) ;;(4.5)

(cons (cons (cons 4 5) 8) 99) ;; (((4.5).8).99)

;; cons has a car and cdr, giving out K and V respectively

(car (cons 4 5)) ;; 4
(cdr (cons 4 5)) ;; 5
;; You can build many data structures using conses.
;; Simplest would be a linked list where, car points to elment, cdr points to other cons or nil


;;;;;;;;;;;;;;;;;;Lists are lisp's linked lists
(list 2 5 6) ;; (2 5 6)

;; Lisp prints linked lists in a special way: it omits periods and parens
(cons 4 nil) ;; (4)
(cons 4 (cons 5 6)) ;; (4 5.6)
(cons 4 (cons 5 (cons 6 nil))) ;; (4 5 6)
;; it seems like if cdr of a cons cell is another cons cell then (.) printing is omitted

;;car and cdr of nil are nil
(car nil) ;;nil
(cdr nil) ;;nil

;; If you store your list in a variable, you can make it act like a stack
(setq stck nil)

(push 4 stck)
(push 99 stck)
(push 0 stck) ;; (0 99 4)

stck ;; (0 99 4)

(pop stck) ;; 0
(pop stck) ;; 99
(pop stck) ;; 4
(pop stck) ;; nil


;;;;;;;;;;;;;;;;;;;;;;;;;;;;Functions;;;;;;;;;;;;;;;;;;;;;;;;

(+ 4 5 6) ;; 15 - prefix notation, first item in list is function

(+ (+ 3 4) (+ (+ 3 4) 5)) ;; 19 - strict eval order, eval arguments first

(defun foo (x y) (+ x y 5)) ;; defun is a function defining macro whose name or argument list is not evaluated when defining

(defun fact (x)
  (if (> x 0)
      (* x (fact (- x 1)))
    1)) ;; a recursive function in lisp

(fact 4) ;; 24

;;a lisp function with multiple statements in its defun body
(defun bar (x)
  (setq x (* x 3))
  (setq x (/ x 2))
  (+ x 4))

;; every statement in body above is evaluated and value of last one
;; is returned
(bar 5) ;; 11

;; binding - the process of assigning a symbol a value for
;; duration of some lexical scope is called binding

;;optional arguments in function
;; any argument after symbol &optional is optional
;; you can also provide default values for optional args

(defun bar (x &optional y) (if y x 0))

(bar 4) ;; 0
(bar 5 t) ;; 5

;; figure out how to specify default values for arguments


;; For var arg list for e.g. like printf where the arity of function is not fixed
;; you use &rest. Lisp will collect all arguments not otherwise
;; accounted for into a list and bind the &rest parameter to taht list
;; e.g.
(defun foo (x &rest y) y)

(foo 3) ;; nil
(foo 4 5 6 7) ;; (5 6 7)



;;Finally you can give your function other kind of optional argument
;; known as "keyword argument"
;;The caller can give these arguments in any order since they are
;; labeled with keywords, much like ocamls labeled arguments

;;emacs lisp doesn't support key arguments it seems :(
(defun foo2 (&key x y)
  (cons x y))

(foo2 :y 4 :x 9) ;;doesn't work




;;;;;;-----------------Printing and Formatting------------------

;; format is name of the function to be used
;; CommonLisp and EmacsLisp format tend to be different it seems

(format "An atom: ~S~%and a list: ~S~%and an integer: ~D~%" nil (list 5) 6)


;;;;;Forms and REPL
;;Forms/Expressions are the things which we type into REPL Loop
;;In general, a form is either an atom e.g. symbol, integer, string
;;or it will be a list.
;;

;;If form is atom lisp evaluates it immedeately.
;;Symbol evaluate to their value, int and strings evaluate to themselves
;;If form is a list, first thing is function, rest are arguments

;;Repl saves recent results in *, ** and *** (not in Elisp)

5 ;; 5

;;;;;;;;;;;;;;;;;;;;;;;Special Forms;;;;;;;;;;;;;;;;;;;;;;;
;;Unlike regular form/expressions, special forms have
;; different rules of evaluation.
;;There are number of special forms that look like function
;; but aren't e.g. if

;;Mainly divided into
;;1. assignments -> setq, setf, push, pop
(setq aaa 3) ;; aaa is not evaluated bcoz setq is special form
(quote a) ;; a is not evaluated bcoz quote is a special form

;;2. function -> like quote but preferred for objects which are functions
;; useful for passing functions to other functions, higher order functions
(function +)
#'+

;;3. definitions like defun, defstruct are special form
(defun adders (x y z) (+ x y z)) ;; (x y z) is not evaluated bcoz special form


;;4. let bindings are special form
;; let binding body rules are same as defun - see multi statement body below
(let ((a 2)
      (b 5)
      (c 2))
  (setq c (+ a b))
  c)

;; In let bindings, the variables being bound cannot refer to each other
(let ((i 2)
      (er (+ i 1)))
  er) ;; err cannot refer to binding variable i in binding construct

;;To do as desired in above construct we use "let*"
;;e.g.
(setq x 7) ;; x = 7
(let* ((x 1)
       (y (+ x 1)))
  y) ;; 2
;; let* form is equivalent to second let inside first let and so on..



;;;;-------------------------------defvar--------------------------
;; defvar special form is similar to setq in that it sets value
;; of a global variable. It is unlike setq in two ways :
;; 1. defvar only sets value of variable if variable already does not
;;    have a value
;; 2. If variable already has a value, defvar does not override value

(defvar meaning-of-universe 42
  "The answer to everything")

meaning-of-universe ;; 42

;; below defvar execution has no effect if it finds variable with val
(defvar meaning-of-universe 88
  "Thou shall not pass")

meaning-of-universe ;; still 42

