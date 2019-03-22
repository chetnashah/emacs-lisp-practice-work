#lang racket

;; By using define-syntax we are making a transformer binding.
;; Effectively telling Racket compiler, whenever encounterirng a chunk of syntax starting with foo
;; give it to transformer function specified and replace it with syntax returned from the transformer
(define-syntax foo
  (lambda (stx)
    (syntax "doo doo")))

;; similar to define and lambda, one can use shorthand at binding
(define-syntax (foo2 stx) ;; notice syntax passed in does not get eagerly evaluated
  (syntax "woo hoo"))

;; one of the shorthand for syntax keyword is "#'"
(define-syntax (foo3 stx)
  #'"boom box")

;; processing input to macro
;; The "stx" that we refer above is what is given to our transformer: a source syntax object
(define-syntax (foo4 stx)
  (print stx)
  (syntax (void)))

;; In fact: The input and output of a macro transformer (i.e., source and replacement forms) are represented as syntax objects
;; One can create literal syntax objects using "syntax" keyword or "#'"
(syntax (+ 1 2)) ;; the source inside syntax object is never evaluated, it acts as if quoted

;; know if given object is a syntax object using (syntax? stx)
(syntax? (+ 1 2)) ;;#f
(syntax? (syntax (+ 1 2))) ;;#t

;; unwrapping a syntax object using (syntax-e stx)
(syntax-e (syntax (+ 1 2))) ;; '(#<syntax +> #<syntax 1> #<syntax 2>)
(syntax-e (syntax a)) ;; 'a
(syntax-e #'(x . y)) ;; '(#<syntax x> . #<syntax y>)

;; stripping syntax object using (syntax->datum stx)
;; strips lexical info, source-loc info and tamper status and just returns datum
(syntax->datum (syntax a))       ;; 'a
(syntax->datum (syntax (+ 1 2))) ;; '(+ 1 2)
(syntax->datum #'(+ 1 2))        ;; '(+ 1 2)
(syntax->datum #'(x . y))        ;; '(x . y)


