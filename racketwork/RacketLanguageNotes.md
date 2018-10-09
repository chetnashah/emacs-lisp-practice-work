
### byte

A byte is an exact integer between 0 and 255.

Useful predicates:
1. `(byte? 22)`

A byte-string is a sequence of bytes.
byte string prints like the ASCII decoding of the byte string, but prefixed with a #. Unprintable ASCII characters or non-ASCII bytes in the byte string are written with octal notation.

### char

A Racket character corresponds to a Unicode scalar value. Roughly, a scalar value is an unsigned integer whose representation fits into 21 bits, and that maps to some notion of a natural-language character or piece of a character

A printable character normally prints as `#\` followed by the represented character e.g. `#\A`. An unprintable character normally prints as `#\u` followed by the scalar value as hexadecimal number. A few characters are printed specially; for example, the space and linefeed characters print as `#\space` and `#\newline`, respectively.

e.g.
```racket
(integer->char 65)     ;; #\A
```

Useful predicates are:
1. char compare `(char=? c1 c2)`
2. char compare ignorecase `(char-ci=? c1 c2)`
3. char type check e.g. `(char? #\A)`

### Racket Module

To begin with the smallest of the three, 
a file that begins with #lang and the name of a language is a `module`.

A `package` is a group of modules that you can install together and that
usually provide one feature/functionality (units of code distribution).
e.g. `pict3d` is a package for 3d stuff and can be installed using
`raco install pict3d`

A `collection` is a group of modules that are related together by a topic.
Modules are referred to and required using collection paths. For example, when you require racket/class, you’re requiring the class module from the racket collection.

### #lang shorthand and module syntax

```rkt
(module cake racket
  (provide print-cake)
 
  (define (print-cake n)
    (show "   ~a   " n #\.)
    (show " .-~a-. " n #\|)
    (show " | ~a | " n #\space)
    (show "---~a---" n #\-))
 
  (define (show fmt n ch)
    (printf fmt (make-string n ch))
    (newline)))
```

is equivalent to a `cake.rkt` with following
```racket
#lang racket
 
(provide print-cake)
 
; draws a cake with n candles
(define (print-cake n)
  (show "   ~a   " n #\.)
  (show " .-~a-. " n #\|)
  (show " | ~a | " n #\space)
  (show "---~a---" n #\-))
 
(define (show fmt n ch)
  (printf fmt (make-string n ch))
  (newline))
```

### Submodule

A module form can be nested within a module, in which case the nested module form declares a submodule. Submodules can be referenced directly by the enclosing module using a quoted name.