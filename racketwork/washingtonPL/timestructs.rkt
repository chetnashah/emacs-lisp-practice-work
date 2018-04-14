#lang racket

;; time represented in hours, mins and seconds
(struct hmstime [hours mins secs])

(define t (hmstime 12 30 2)) ;; 45002 seconds

;; converts hms time to total time in seconds
;; hmstime -> Number
(define (hms->secs hms)
  (+ (* 3600 (hmstime-hours hms))
     (* 60 (hmstime-mins hms))
     (hmstime-secs hms)))

(hms->secs t)


