#lang racket

(require 2htdp/image)

;; col takes a number and an Image
;; and repeats Image vertically for n times
;; Number, Image -> Image
(define (col n img)
  (cond [(eq? n 0) img]
        [#t (above img (col (- n 1) img))]))


(col 4 (text "hola" 25 "red"))

;; row takes a number and an Image
;; and repeats image horizontally
;; Number, Image -> Image
(define (row n img)
  (cond [(eq? 0 n) img]
        [#t (beside img (row (- n 1) img))]))

(row 3 (text "hey" 24 "blue"))

(define sq (square 10 "outline" "black"))
;; we have a grid
(col 18 (row 8 sq))



;; recursive struct for russian doll
(struct layer [color doll])

;; A RD is one of
;; - String
;; - (layer String RD)

(layer "red" (layer "green" (layer "yello" "blue")))


