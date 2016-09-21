;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(funcall 'cons 1 2)

(funcall (lambda (a b c) (+ a b c)) 2 4 5) ;; funcall fn arg1 arg2 arg3...

(apply (lambda (a b c) (+ a b c)) '(2 4 5));; apply fn '(arg1 arg2 arg3 ...)

;; classical lambda abstraction + application : (\ args body) 18
((lambda (x) (+ x x)) 18)


;; useful list predicates
;; listp obj -> returns t if object is some sort of list empty or otherwise
(listp '(4)) ;; true
(listp '())  ;; true

(listp 3)    ;;false

(listp nil) ;; wierd that this is true ?? -> nil is a list

(consp 3)       ;; nil
(consp nil)     ;;f, nil is not a cons cell, althought it is a list
(consp '(23 4)) ;; t
(consp '(5))    ;; t
;; the only difference between consp and listp is nil

(atom 6)      ;;t
(atom nil)    ;;t --> wtf ? nil is an atom and also a list but not a cons cell
(atom '(3 4)) ;;nil

(null nil)  ;;t
(null 3)    ;;nil
(null ())   ;;t
(null '())  ;;t
(null '(4)) ;;nil

(if (null nil) (print 2) (print 3))

(+ 54 (* 34 9) 34) ;; try out barfing and slurping in paredit with C- -> and C- <-

(+ 32
   (+ 23 23)
   (* 23 23
      (+ 23 12 11))) ;; strucutural navigation between siblings with C-M-f and C-m-b and
                     ;; strucutural navigation child side C-m-d down and parent side C-m-u



