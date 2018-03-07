#lang racket

;; a macro definition describes how to transform syntax to different syntax
;; helps extend syntax, domain specific language as syntax etc


;; a macro expansion is the process of rewriting according to macro definition
;; by rewriting before evaluation, the lazy execution and stuff is possible 

;; if you define a macro m in racket, m becomes a new special form
;; i.e. (m ...) gets expanded according to definition of m
;; so macros can be uses for defining special forms

;; parenthesis in macros ->
;; (m ...) expands to ( something else ) i.e. parenthesis remain there.

;; scope in macros ->
;; variable with same name as macro will shadow the macro.

(define-syntax my-if      ;; macro name
  (syntax-rules
      (then else)         ;; new keywords other than macro name
    [(my-if e1 then e2 else e3) (if e1 e2 e3)])) ;; specifications

;; try (my-if (> 2 4) then (+ 4 1) else (+ 100 100))

;; skipping evaluation of a parameter, only special forms can do
(define-syntax comment-out
  (syntax-rules
      ()
    [(comment-out ignore instead) instead]))

;; one doesn't see error
;; since we manipulate code (and thus execution) on rewriting
(comment-out (error "wowza") (+ 1 1))

;; other good use is to skip
;; evaluation of expression by thunking it
(define-syntax my-delay
  (syntax-rules
      ()
    [(my-delay e1) (lambda () e1)]))


;; hygeine of macros
;; problem case 1
;; soln: secretly renames local variables in macros with fresh names
;; problem case 2,
;; soln: looks up variables used in macros where macro is defined
