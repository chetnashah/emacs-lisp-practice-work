#lang racket

;; writing an interpreter

;; variable, in order to evaluate a variable, look it up in environment

;; an environemnt is just a mapping of strings to values
;; (eval-under-env e env) an inner helper function

;; (eval-exp e) is the interpreter which calls helper with empty environment.



;; technically a function is not a value but function+env i.e. a closure is a value.