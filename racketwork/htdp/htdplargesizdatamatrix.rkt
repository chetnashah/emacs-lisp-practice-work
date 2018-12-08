#lang racket

(require test-engine/racket-tests)

;;transpose matrix along diagonal
;; transpose :: Matrix -> Matrix

(define wor1 (cons 11 (cons 21 '())))
(define wor2 (cons 12 (cons 22 '())))

(define tam1 (cons wor1 (cons wor2 '())))

;; A Matrix is one of :
;; - (cons Row '())  ;; last row
;; - (cons Row Matrix) ;; intermediate rows
;; also known as lln - list of list of numbers

;; A Row is one of
;; '()
;; (cons Number Row)

(define row1 (cons 11 (cons 12 '())))
(define row2 (cons 21 (cons 22 '())))
(define mat1 (cons row1 (cons row2 '())))

;; transpose :: lln -> lln
(define (transpose lln)
  (cond
    [(empty? lln) '()]
    [(empty? (first lln)) '()]
    [#t (cons (first* lln) (transpose (rest* lln)))]))

;; first* : lln -> first column
(define (first* lln)
  (cond
    [(empty? lln) '()]
    [(empty? (first lln)) '()]
    [#t (cons (first (first lln)) (first* (rest lln)))]))

(check-expect (first* mat1) '(11 21))

;; rest* :: lln -> lln with first col removed
(define (rest* lln)
  (cond
    [(empty? lln) '()]
    [(empty? (first lln)) '()]
    [#t (cons (rest (first lln)) (rest* (rest lln)))]))

(check-expect (rest* mat1) '((12) (22)))


(check-expect (transpose mat1) tam1)

(test)