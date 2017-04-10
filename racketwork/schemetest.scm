

;; need to install Guile repl for eval-buffer for scheme, racket
;; repl does not allow set-car! etc.
(define pr (cons 'a 'b))

(set-car! pr 'c)

