;; we call the continuation in expr
(+ 4 (call-with-current-continuation
      (lambda (cont) (cont (+ 1 2)))))

;; we do not call the continuation, just arbitrary value
;; 99 is returned
(+ 4 (call-with-current-continuation
      (lambda (cont) 99)))