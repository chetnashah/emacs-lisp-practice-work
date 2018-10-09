

When using scheme stuff like (set-car!), (set-cdr!) etc,
racket does not support it, even with #lang scheme,

So such code is written in .scm files.

Also for rkt code, racket repl is used.

For mutable code, .scm files are used, with scheme guile repl.
Geiser tries to guess the scheme(which will be shown in minor mode)
To explicitly set scheme set buffer local variable in comments
e.g. for guile
;; -*- geiser-scheme-implementation: guile -*-

Scheme repls are opened by issuing M-x run-geiser,
When it will ask which repl you want(chicken, mit, guile, chibi, chez)
And it should be same as one shown in your minor mode

More on http://geiser.nongnu.org/geiser_4.html

Trace recursive function calls in guile with ,trace
gives a nice execution stack like so:

e.g. non tail recursive evaluation
scheme@(guile-user)> (define (fact1 n) 
                       (if (zero? n) 1
                           (* n (fact1 (1- n)))))
scheme@(guile-user)> ,trace (fact1 4)
trace: (fact1 4)
trace: |  (fact1 3)
trace: |  |  (fact1 2)
trace: |  |  |  (fact1 1)
trace: |  |  |  |  (fact1 0)
trace: |  |  |  |  1
trace: |  |  |  1
trace: |  |  2
trace: |  6
trace: 24

And tail recursive:
scheme@(guile-user)> (define (facti acc n)
                       (if (zero? n) acc
                           (facti (* n acc) (1- n))))
scheme@(guile-user)> (define (fact2 n) (facti 1 n))
scheme@(guile-user)> ,trace (fact2 4)
trace: (fact2 4)
trace: (facti 1 4)
trace: (facti 4 3)
trace: (facti 12 2)
trace: (facti 24 1)
trace: (facti 24 0)
trace: 24


## Dr Racket

Auto complete in Dr. Racket IDE works using Ctrl + /

### Doing IO with racket

reading from files
```racket
;; http://docs.racket-lang.org/teachpack/2htdpbatch-io.html
;; read-file :: file/stdin -> string?
(define s (read-file "ttt.txt"))
(define s (read-file 'stdin))

;; read-lines :: file/stdin -> (listof string?)
(define s (read-lines "ttt.txt))
(define s (read-lines 'stdin))

;; read-line directly reads from input-port, which is stdin by default
;; http://docs.racket-lang.org/reference/ports.html
(define s (read-line))

```

