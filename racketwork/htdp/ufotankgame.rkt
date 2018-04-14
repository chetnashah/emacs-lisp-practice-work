#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(define WIDTH 200)
(define HEIGHT 200)
(define TANK-HEIGHT 10)
(define TANK-WIDTH 25)
(define UFO-HEIGHT 8)
(define UFO-WIDTH 15)

(define MTSCN (empty-scene WIDTH HEIGHT "Beige"))

(define TANK (rectangle TANK-WIDTH TANK-HEIGHT "solid" "blue"))
(define UFO (rectangle UFO-WIDTH UFO-HEIGHT "solid" "red"))
(define MISSILE (triangle 13 "solid" "yellowgreen"))
;; ufo tank game

;; a UFO is a posn
;; tank is [loc vel]
;; a missile is a posn

(struct posn [x y] #:transparent)

;; loc in x-axis, vel in x-axis
(struct tank [loc vel])

(struct aim [ufo tank])     ;; state when there are two entities in game
(struct fired [ufo tank missile]);; state when there are three entities in game


;; a SIGS is one of:
;; - (aim ufo tank)
;; - (fired ufo tank missile)
;; in short it is the complete state of space invader game

;; tank, bg -> Image
(define (tank-render t bg)
  (place-image TANK (tank-loc t) (- HEIGHT TANK-HEIGHT) bg))

;; ufo, bg -> Image
(define (ufo-render u bg)
  (place-image UFO (posn-x u) (posn-y u) bg))

;; missile, bg -> Image
(define (missile-render m bg)
  (place-image MISSILE (posn-x m) (posn-y m) bg))

;; si-render: SIGS -> Image
(define (si-render s)
  (cond [(aim? s) (tank-render (aim-tank s) (ufo-render (aim-ufo s) MTSCN))]
        [(fired? s) (tank-render (fired-tank s)
                                 (ufo-render (fired-ufo s)
                                            (missile-render (fired-missile s) MTSCN)))]))

;; computes distance between two posn
(define (pdistance p1 p2)
  (sqrt (+ (* (- (posn-x p1) (posn-x p2)) (- (posn-x p1) (posn-x p2)))
           (* (- (posn-y p1) (posn-y p2)) (- (posn-y p1) (posn-y p2))))))

;; checks if two points are close
;; p1, p2 -> boolean
(define (arepointsclose p1 p2)
  (< (pdistance p1 p2) 5))

;; si-game-over? : SIGS -> boolean
(define (si-game-over? s)
  (cond [(and (aim? s) (> (posn-y (aim-ufo s)) HEIGHT)) #t]
        [(fired? s) (or (< (posn-y (fired-ufo s)) 0)
                        (arepointsclose (fired-missile s)
                                        (fired-ufo s)))]
        [#t #f]))


;; TODO on-tick calls a function called si-move
;; si-move SIGS -> SIGS
(define (si-move s)
  (cond [(aim? s) (aim (posn
                        (+ (posn-x (aim-ufo s)) (random 2))
                        (+ (posn-y (aim-ufo s)) (random 2)))
                       (tank
                        (+ (tank-vel (aim-tank s)) (tank-loc (aim-tank s)))
                        (tank-vel (aim-tank s))))]
        [(and (fired? s) (> (posn-y (fired-missile s)) 0))
         (fired (posn
                            (+ (posn-x (fired-ufo s)) (random 2))
                            (+ (posn-y (fired-ufo s)) (random 2)))
                           (tank
                            (+ (tank-vel (fired-tank s)) (tank-loc (fired-tank s)))
                            (tank-vel (fired-tank s)))
                           (posn
                            (posn-x (fired-missile s))
                            (- (posn-y (fired-missile s)) 4))
                           )]
        [#t s] ;; handle case when missile leaves viewport, return aim struct
        ))

(define (si-render-final s)
  (text "game over" 20 "black"))

;; si-control , which is key-event handler
;; si-control:: SIGS, KeyEvent -> SIGS
;; left key for left movement
;; right key for right movement
;; space key for launch missile
;;TODO fire missile
(define (si-control s ke)
  (cond [(and (aim? s) (key=? ke "left")) (aim (aim-ufo s) (move-tank (aim-tank s) -5))]
        [(and (aim? s) (key=? ke "right")) (aim (aim-ufo s) (move-tank (aim-tank s) 5))]
        [(and (aim? s) (key=? ke " ")) (fired (aim-ufo s) (aim-tank s) (posn (tank-loc (aim-tank s)) (- HEIGHT TANK-HEIGHT)))]
        [(and (fired? s) (key=? ke "left")) (fired (fired-ufo s) (move-tank (fired-tank s) -5) (fired-missile s))]
        [(and (fired? s) (key=? ke "right")) (fired (fired-ufo s) (move-tank (fired-tank s) 5) (fired-missile s))]
        [(and (fired? s) (key=? ke " ")) s]
        ))

(define (move-tank t delta)
  (tank (+ delta (tank-loc t)) (tank-vel t)))

;; TODO si-main launches big-bang

(define aim1 (aim (posn 30 30) (tank 28 0)))
(define fired1 (fired (posn 49 49) (tank 50 9) (posn 50 50)))
;; (si-render aim1)
;; (si-render fired1)

(define (run s)
  (big-bang s
            (on-tick si-move)
            (to-draw si-render)
            (stop-when si-game-over? si-render-final)
            (on-key si-control)))

            


