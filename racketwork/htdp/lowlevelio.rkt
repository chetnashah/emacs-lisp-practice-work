#lang racket

;; a racket port represents a source or sink of data
;; like a file, a terminal, a tcp connection

;; ports provide sequential acess

;; functions for generating ports
;; - open-output-file :: FileName -> Output-Port
;; - open-input-file :: FileName -> Input-Port

;; functions for writing/reading to ports
;; - display:: String, Port -> Nothing
;; - read-line :: Port -> String | #<eof>
;; - read-char :: Port -> char | eof
;; - read-byte :: Port -> byte | eof

;; close-output-port and close-input-port for closing
;; file-stream-port? recognizes file-stream ports

(define out (open-output-file "abc.txt" #:exists 'replace))

(displayln "goo.gle" out)
(display "amazon" out)
(display "Netflix" out)
(newline out)
(newline out)
(display "Whatsapp" out)
(display "Mocha" out)
(close-output-port out)

;; read-all-lines :: input-port, '() -> list-of-line/string
(define (read-all-lines in ans)
  (let ([r (read-line in)])
    (cond [(eof-object? r) (reverse ans)]
          [(string? r)
           (read-all-lines in (cons r ans))])))

;; read-words :: input-port, '() -> list of words

;; read-words/line :: input-port -> list of list of words

;; read-1strings :: input-port -> list of 1Strings

;; making a list of lines from input port
(define in (open-input-file "abc.txt"))

 (read-all-lines in '())

;; read-line returns a string if present or eof
 (close-input-port in)

(define (just-a-function)
  (println "A sentence")
  (println "Another sentence")
  2)
