#lang racket

;; a macro definition describes how to transform some new syntax
;; into different syntax in the source language sort of like a transpiler


;; macro expansion is process of rewriting syntax for each macro use

;; macro expansion happens before any of compiling running evaluation typechecking happens

;; If you define a macro m in Racket,
;; then m becomes a new special form.


;;For e.g. my own inventex syntax macro my-if
;; where
;; Expand (my-if e1 then e2 else e3)
;; to (if e1 e2 e3)


;; macros vs functions ?
;; when in doubt, resist defining a macro


;; 1) Macro Tokenization
;; Macro systems generally work at the level of tokens and not sequences of charachters

;; 2) Macro parenthesization

;; 3) Macro scoping

;; define macros with define-syntax

