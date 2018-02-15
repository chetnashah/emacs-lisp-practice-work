#lang racket

(+ 3 5)

(abs -4)

(add1 99)

(ceiling 22.4)

(gcd 60 80)

(println "logs")
(log 100)
(log 2)

(println "utilities")
;; max is variadic
(max 12 324 3456 23094)

;; generate a random no less than given
(random 10000)

;; exact and inexact numbers
(/ 4 6);; racket shows 2/3

(define (distance x y)
  (sqrt (+ (* x x) (* y y))))

(distance 3 4)

;; strings...
(string-append "what a" "lovely " "day" " 4 BSL")
(string-length "Good Monday Morning") ;; 19
;;(string-ith "Hello world" 1)
(number->string 64)

(define str "helloworld")
(define i 5)

(define mmm
  (string-append (substring str 0 i)
                 "_"
                 (substring str i)))

;; images are values just like strings and numbers
(require 2htdp/image)

;; circle, ellipse, line, rectangle, star all produce images
(circle 10 "solid" "green")
(rectangle 10 20 "solid" "yellow")
(star 12 "solid" "gray")

;; text produces images
(text "hello" 25 "olive")
(text "Goodbye" 30 "indigo")

;; composing images with overlay, overlay/xy and overlay/align

;; other useful primitives are empty-scene, place-image, scene+line


;; Boolean arithmetic
#true ;; #t
#false ;; #f
#t ;; #t
#f ;; #f
