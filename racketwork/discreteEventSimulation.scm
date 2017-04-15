
;; make sure guiser repl for scheme is installed
(define (logical-not s)
  (cond [(= s 0) 1]
        [(= s 1) 0]
        [#t (error "logical-and error")]))


(define (logical-and x y)
  (cond [(and (= x 1) (= y 1)) 1]
        [(and (= x 1) (= y 0)) 0]
        [(and (= x 0) (= y 1)) 0]
        [(and (= x 0) (= y 0)) 0]
        [#t (error "logical-not error")]))

(define (logical-or x y)
  (cond [(and (= x 1) (= y 1)) 1]
        [(and (= x 1) (= y 0)) 1]
        [(and (= x 0) (= y 1)) 1]
        [(and (= x 0) (= y 0)) 0]
        [#t (error "logical-not error")]))

;; wires are made using make-wire

;; (get-signal <wire>) returns signal on the given wire

;; (set-signal <wire> <new val>) sets new val on the wire

;; (add-action <wire> <procedure without args>)
;; says whenever signal on wire changes, run the procedure

;; (after-delay <delay> <procedure>)
;; run the given procedure after given delay amount of time passes

(define (inverter in-wire out-wire)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal in-wire))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! out-wire new-value)))))
  (add-action! in-wire invert-input)
  'ok)

;; similarly and gate
(define (and-gate a1-wire a2-wire out-wire)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1-wire) (get-signal a2-wire))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! out-wire new-value)))))
  (add-action! a1-wire add-action-procedure)
  (add-action! a2-wire add-action-procedure)
  'ok)

;; similarly or gate
(define (or-gate a1-wire a2-wire out-wire)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1-wire) (get-signal a2-wire))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! out-wire new-value)))))
  (add-action! a1-wire or-action-procedure)
  (add-action! a2-wire or-action-procedure)
  'ok)

;; A wire in our simulation will be an object
;; with two local state variables:
;; signal-value and action-procedures to be run on change of signal-value

;; we implement wire with message-passing style, return dispatch function
;; dispatch has 3 cases set-signal!, get-signal and add-action!

;; An interesting thought experiment from the SICP book
;; Why msg passing style (wire 'get-signal) // wire procedure, get-signal data
;; functional style (get-signal wire) // get-signal is procedure, wire is data
;; The truth is in languages which we can deal with procedures as objects
;; there is no fundamenta difference between procedures and data

;; a standard template for an object would be
;; initialize protected state with let.
;; define all inner functions which close over state
;; return dispatch method with cases containing all inner definitions
(define (make-wire)
  (let ((signal-value 0) (action-procedures '())) ;; init state
    ;; inside let body we have all inner definitions
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin
            (set! signal-value new-value)
            (call-each action-procedures))
          'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond [(eq? m 'get-signal) signal-value]
            [(eq? m 'set-signal!) set-my-signal!]
            [(eq? m 'add-action!) accept-action-procedure!]
            [#t (error "Unknown operation on wire")]))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

;; now the object-function correspondence for wire

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire value)
  ((wire 'set-signal!) value))

(define (add-action! wire proc)
  ((wire 'add-action!) proc))


