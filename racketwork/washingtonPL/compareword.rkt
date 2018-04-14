#lang racket

;; A 1string is a String of length 1
;; three letterword consists of three 1String
(struct threeletterword [fst snd thd] #:transparent)


(define (compare-letter l1 l2)
  (if (equal? l1 l2)
      l1
      #false))

;; compare-word consumes two threeletterword.
;; produces a word that indicates where the given ones agree and disagree.
;; e.g. (compare-word (tlw c a t) (tlw b a t)) will result in (tlw #f a t)
;; threeletterword -> threeletterword -> threeletterword
(define (compare-word w1 w2)
  (if (equal? w1 w2)
      w1
      (threeletterword
       (compare-letter (threeletterword-fst w1) (threeletterword-fst w2))
       (compare-letter (threeletterword-snd w1) (threeletterword-snd w2))
       (compare-letter (threeletterword-thd w1) (threeletterword-thd w2)))))


;; test case
(compare-word (threeletterword 'c 'a 't) (threeletterword 'b 'a 't))