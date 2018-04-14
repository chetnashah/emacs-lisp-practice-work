#lang racket
(require 2htdp/image)
(require 2htdp/universe)
;; implementing finite state machines using big-bang
;; program a recognizer for a(b|c)*d
;; initial color white
;; once a is seen, color yellow
;; red color for error state
;; green color for satisfied state

;; State is one of
;; AA, BB, ER, DD

(define (s-render s)
  (cond [(string=? s "AA") (rectangle 100 100 "solid" "WhiteSmoke")]
        [(string=? s "BB") (rectangle 100 100 "solid" "yellow")]
        [(string=? s "ER") (rectangle 100 100 "solid" "red")]
        [(string=? s "DD") (rectangle 100 100 "solid" "green")]
        [#t (rectangle 100 100 "solid" "red")]))

(define (key-handler s ke)
  (cond
    [(equal? s "DD") s] 
    [(and (equal? s "AA") (key=? ke "a")) "BB"]
        [(and (equal? s "BB") (or (key=? ke "b")
                                  (key=? ke "c"))) "BB"]
        [(and (equal? s "BB") (key=? ke "d")) "DD"]
        
        [#t "ER"]))

(define (run)
  (big-bang "AA"
            (to-draw s-render)
            (on-key key-handler)))
