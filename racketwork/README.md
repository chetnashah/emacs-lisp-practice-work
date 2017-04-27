

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

