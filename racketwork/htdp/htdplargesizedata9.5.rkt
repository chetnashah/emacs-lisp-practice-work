#lang racket
(require test-engine/racket-tests)
(require 2htdp/universe)
(require 2htdp/image)
(define HEIGHT 80)
(define WIDTH 100)
(define XSHOTS (/ WIDTH 2))

;; graphical constants
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define SHOT (triangle 3 "solid" "red"))

;; a List-of-Shots is one of
;; - '()
;; (cons Shot List-of-shots)
;; interpretation: collection of shots fired

;; A Shot is a Number
;; interpretation: represents shot's y-coordinate

;; A ShotWorld is a List-of-numbers
;; interpretation: each number on list represents y-coord

;; rendering
;; ShotWorld -> Image
(define (to-image w)
  (cond [(empty? w) BACKGROUND]
        [#t (place-image SHOT XSHOTS (first w) (to-image (rest w)))])) 

;; dealing with tick action
;; ShotWorld -> ShotWorld
(define (tock w)
  (cond [(empty? w) '()]
        [#t (if (< (first w) 0)
                (tock (rest w))
                (cons (- (first w) 1) (tock (rest w)))
                )]))

;; dealing with key events
;; add a shot to the world if player presses space bar
;; ShotWorld, KeyEvent -> ShotWorld
(define (keyh w ke)
  (if (key=? ke " ") (cons HEIGHT w) w))

(check-expect (to-image (cons 9 '()))
              (place-image SHOT XSHOTS 9 BACKGROUND))

(check-expect (tock (cons 9 (cons 99 '())))
              (cons 8 (cons 98 '())))

(test)

(define (main w0)
  (big-bang w0
            [on-tick tock]
            [on-key keyh]
            [to-draw to-image]))
