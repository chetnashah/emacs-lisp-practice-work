#lang racket

(require 2htdp/batch-io)
(require test-engine/racket-tests)

;; built-in functions
;; read-file
;; read-lines
;; read-words

;; words-on-line :: LLS -> List-of-Numbers
;; determines number of words on each line
(define (words-on-line lls)
  (cond
    [(empty? lls) '()]
    [else (cons (length (first lls))
                (words-on-line (rest lls)))]))


(define line0 (cons "hello" (cons "world" '())))
(define line1 '())

(define lls0 '())
(define lls1 (cons line0 (cons line1 '())))

(check-expect (words-on-line lls0) '())
(check-expect (words-on-line lls1) (cons 2 (cons 0 '())))

;; collapse :: LLS -> String
;; converts a list of lines into a string,
;; with strings seperated by space and lines seperated by "\n"
(define (collapse lls)
  (cond [(empty? lls) ""]
        [(empty? (rest lls)) (line-collapse (first lls))]
        [#t (string-append
             (line-collapse (first lls))
             "\n"
             (collapse (rest lls)))]))

(define (line-collapse line)
  (cond
    [(empty? line) ""]
    [(empty? (rest line)) (first line)]
        [#t (string-append (first line) " " (line-collapse (rest line)))]))

(check-expect (collapse lls0) "")
(check-expect (collapse lls1) "hello world\n")


(define (is-not-article? x)
  (not (or (string=? "a" x) (string=? "an" x) (string=? "the" x))))

(define (line-remove-article line)
  (filter is-not-article?
          line))

;; article-remover :: nameOfFile -> void
;; input is the name of the file n, the file is read,
;; all the articles are removed and writes result out to
;; file whose name is "no-articles-nameOfFile"
(define (article-remover nameOfFile)
  (let ([lls (read-words/line nameOfFile)])
    (write-file (string-append "no-articles-" nameOfFile)
                (collapse (map line-remove-article lls)))))

(article-remover "ttt.txt")
;;(test)

