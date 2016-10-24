
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

;; let's define a function
(defun plus10 (x)
  (+ x 10))

;; apply function to succcessive cars of the list -> 1 2 3 4 5
(mapcar 'plus10 '(1 2 3 4 5)) ;; (11 12 13 14 15)


;; apply function to successive cdrs of the list -> (2 3 4 5) (3 4 5) (4 5) (5)
(maplist 'car '(1 2 3 4 5))

;; think of conses as trees also : left subtree is car and right subtree is cdr. they are unbalanced but binary trees nonetheless
;; recursive program to copy expressions

;; define our-member which takes an obj and list and tells if obj is member of list

;; built-in fn member returns nil if element not present in list
;; comparision done with equal fn and return value is tail of list whose car is elt
(member '5 '(1 2 3 4)) ;; nil
(member '3 '(1 2 3 4)) ;; (3 4)
(member '(1 1) '(9 8 (1 1) 99)) ;;((1 1) 99) 

;; built in fn equal
(equal 3 3)    ;; t
(equal 4 "4")  ;; nil
(equal () nil) ;; t
(equal () '()) ;; t
(equal nil 0)  ;; nil
(equal '(3 3 2) '(3 3 2)) ;; t
;; this contrasts against eql as -> equal is value equality/structural equality
;; eql is reference/atomic/numerical equality
(eql '(3 3 2) '(3 3 2)) ;; nil

;; adjoin : return item consed onto front of list only if it is not in list, else return list unmodified
(adjoin 'c '(a b c)) ;; (a b c)
(adjoin 'z '(a b c)) ;; (z a b c)

(defun my-adjoin (obj lst)
  (if (member obj lst)
      lst
    (cons obj lst)))

(my-adjoin 'b '(a b c))
(my-adjoin 'z '(a b c))

;; union takes two lists and joins them using set union operation
;; also since they are sets there is no notion of ordering
(union '(a b c) '(a b s)) ;; (s a b c)
(intersection '(a b c) '(s b c)) ;; (c b)
(set-difference '(a b c d e) '(b e)) ;; (d c a)

;; if you care about order in a set you are thinking of something
;; akin an array or a vector which in lisp is known as a 'sequence'

;; lets see some common functions
(length '(a b c)) ;; 3

;; subseq takes a list and start index, optional end index and slices list to return subseq
(subseq '(a b c d) 1 2) ;; (b)
(subseq '(a b c d) 1) ;; (b c d)

;; you can check whether optional parameters were passed or not
;; using null predicate
(defun my-subseq (lst st &optional nd)
  (if (and (eql st 0) (null nd))
      lst
    (my-subseq (cdr lst) (1- st))))

(my-subseq '(a b c d e) 2) ;; (c d e)
;; TODO now make one that uses nd

;; reverse takes a list and reverses it copying
(reverse '(a b c)) ;; (c b a)


(defun my-reverse (lst)
  (if (null lst)
      lst
    (append (my-reverse (cdr lst)) (list (car lst)))))

(my-reverse '(1 2 3 4))

;; works like concat takes a list of lists and appends them
(append '(2) '(4))

;; (push x lst) pushes x onto front of lst where lst should be a variable
(setf ls '(1 1))
(push 2 ls) ;; (2 1 1)

;; (push obj lst) is equivalent to (setf lst (cons obj lst))
(defun my-push (obj lst)
  (setf lst (cons obj lst)))

(my-push 3 ls) ;; (3 2 1 1)

;; (pop lst) returns front most element from listvar, if nil returns nil.
(pop ls) ;; 2, yes it is changing the listvar ls
(pop ls) ;; 1
(pop ls) ;; 1
(pop ls) ;; nill

(defun my-pop (lst)
  (let ((x (car lst)))
    (setf lst (cdr lst))
    x))

(my-push '3 '(2 2 2)) ;; these are unlike modifying listvars
(my-pop '(9 39 3)) ;; these are unlike modifying listvars

;;;;;;;;;;interesting info on lists : DOTTED LISTS ;;;;;;;;;;;;;;;;;;
(setf pp (cons 'a 'b))  ;; (a.b)
;; in a dotted list car and cdr are seperated by a dot.
(cons 'a nil) ;; (a) ;;what sorcery is this?
(cons 'a (cons 'b (cons 'c nil))) ;; (a b c)
(cons 'a (cons 'b (cons 'c 'd))) ;; (a b c . d)

(cons 'a (cons 'b nil)) ;; (a b)
(cons 'a '(b)) ;; (a b)

;; other great things about conses
;; are they are substitute for pairs
;; and could be used for something like hashmaps
;; a list of consesis called assoc-list or alist

(setf trans '((+ . "add") (- . "substract")));;((+ . "add") (- . "substract"))
;; assoc function is used to retrieve pair from hashmap(assoclist)
;; assoc takes a key and a alist and returns the pair
(assoc '+ trans) ;; (+ . "add")


;;;;;;;;;;;;;;;;;;;;;;; ch -3 exercises ;;;;;;;;;;;;;;;;;;;;;;;;;

;; append obj to list if does not exist
(defun adj-exist (lst obj)
  (if (or (member obj lst) (null obj))
      lst
    (append lst (list obj))))

(adj-exist nil nil)

;; new-union is a union that preserves order
;;How did I do this?
(defun new-union (l1 l2)
  (if (null l2)
      l1
    (new-union (adj-exist l1 (car l2)) (cdr l2))))
	    
(new-union '(1 2 3 4) '(1 2 9 8 0))
(set-difference '(1 2 3 4) '(1 2))

(member '(a) '((a) (b)))


;; learning about funcall and apply
;; what is the most basic way to call a function ? put it as first mem of list)

(concat "a" "b") ;; "ab"

;; above is when you know which function to call in advance
;; to mix in some meta programming, you might need to compute at run time which function to call, to do that you use funcall, when you also need to determine how many values to pass, use apply

;; funcall calls functions with arguments, and returns whaterver function returns

(setq f 'list) ;; assign name f to functio list
(funcall f 'x 'y 'z) ;; (x y z)

;;NOTE : all arguments to funcall are evaluated and so x, y, z are quoted

(funcall 'and t nil) ;;ERROR: invalid function and, because and is a special form.
(funcall 'list 'x 'y 'z) ;; OK, lisp functions/built-in functions allowed, special forms and macros not allowed in funcall, list is a built-in function

;; apply takes two arguments : 1) function to be called. 2) list of arguments to call fun wit

(apply 'list '(x y z)) ;; (x y z) - subtle huh ?
(apply '+ '(1 2 3 5)) ;; 10 - u see varags here?

;; while we are taking look at funcall and apply, lets also have a brief look at lambda
;; it is different from defun in the way that defun takes name, args, body and attaches the function you made using args and body resulting fun to the given name
;; lambda on the other hand just takes args and body and is a fn with no name. you can call it immedeately only or use it somewhere a fn is expected

;; the anonymous-equivalent(lambda) of multiply-by-seven is
(lambda (number) (* 7 number))

;; call it with 3
((lambda (number) (* 7 number)) 3) ;; 21

;; let's try passing a lambda to a funcall - funcall dont care abt name
(funcall (lambda (number) (* 7 number))
	 '3) ;; 21


