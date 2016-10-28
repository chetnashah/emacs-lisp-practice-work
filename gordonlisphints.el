
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

(cons (cons 4 5) 8) ;; ((4.5).8)

;; cons has a car and cdr, giving out K and V respectively

(car (cons 4 5)) ;; 4
(cdr (cons 4 5)) ;; 5









