
;; when a symbol is evaluated, it is treated as a variable
;; the result is the variable's value if it has one.

(last '(a b c d))

(if (> 4 5)
    (message "then part")
  (message "else part"))

;; nil is falsy
(if nil (message "nil is truthy") (message "nil is falsy"))

;; () is falsy
(if () (message "() is truthy") (message "() is falsy"))

;; t is truthy
(if t (message "t is truthy") (message "t is falsy"))

;; all primitive objects are truthy
(if 3 (message "3 is truthy") (message "3 is falsy"))

;; any lisp expression that can be evaluated is a form and results in a value
(car '(1 2))

;; a macro is a trasnformation between forms

;; exercising function definitions with optional and rest

(defun foof (i1 i2 i3)
        (print (+ i1 i2 i3)))

(foof 4 5 6)

(null nil)

;; returns tail of passed list
(rest '(9 8 7 5))

(car '(7 8 8 9))

(defun mynth (lst n)
  (if (eql n 0)
      (car lst)
    (mynth (cdr lst) (n-1))))

(defun mylength (ll)
  (if (null ll)
      0
    (+ 1 (mylength (cdr ll)))
      )
  )

mynth '(1 2 3)  0

(mylength '(1 2 3 4 5 6 7 8 9 0))


