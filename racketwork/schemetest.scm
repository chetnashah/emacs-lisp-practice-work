

;; need to install Guile repl for eval-buffer for scheme, racket
;; repl does not allow set-car! etc.
(define pr (cons 'a 'b))

(set-car! pr 'c)


(let ([v 2])
  (+ (* v 4) 5))

((lambda (v)
   (+ (* v 4) 5))
 3)
