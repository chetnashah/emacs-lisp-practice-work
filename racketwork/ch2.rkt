
#!/usr/bin/racket
#lang racket

(define nil '())

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ 1 low) high))))

(define (foldrr op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (foldrr op initial (cdr sequence)))))


(define (filterr pred sequence)
  (cond [(null? sequence) nil]
        [(pred (car sequence))
         (cons (car sequence) (filterr pred (cdr sequence)))]
        [#t (filterr pred (cdr sequence))]))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence)
)


(define tree (cons (list 1 2) (list 3 4)))

;; flattens a tree in left to right order
(define (enumerate-tree tre)
  (cond [(null? tre) '()]
        [(not (pair? tre)) (list tre)]
        [#t (append (enumerate-tree (car tre)) (enumerate-tree (cdr tre)))])
  )

;; representing count-leaves using foldrr
(define (count-leaves tr)
  (foldrr + 0 (map (lambda (x) 1) (enumerate-tree tr))
))
