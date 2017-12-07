#lang racket

;; looks like imperative code doesn't it?
;; sequential flow like first declaring three variables one after other
(let ([k 1])
  (let ([j 2])
    (let ([u 2])
      (+ j k u))))


