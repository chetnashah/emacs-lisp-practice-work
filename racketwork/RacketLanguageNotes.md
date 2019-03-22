
### Racket Reader

Reading from a stream produces one datum. If the result datum is a compound value, then reading the datum typically requires the reader to call itself recursively to read the component data.

The reader can be invoked in either of two modes: `read` mode, or `read-syntax` mode.

1. `read mode`: Symbols and keywords are interned in both read and read-syntax mode. Happens via default `read` which reads from input port and returns a datum.

2. `read-syntax mode`: In read-syntax mode, the result is always a syntax object that includes source-location and (initially empty) lexical information wrapped around the sort of datum that read mode would produce. Happens via `read-syntax`.

`interned`: Symbols, keywords, strings, byte strings, regexps, characters, and numbers produced by the reader in read-syntax mode are interned, which means that such values in the result of read-syntax are always `eq?` when they are `equal?`

`datum`: un-interpreted source, like an s-expression, but different from syntax-object.

`phase level`: 

`identifier`: An identifier is a source program
entity. 
An identifier corresponds to 
1. variables
2. syntactic forms
3. transformers
4. quoted to produce symbols or syntax objects

identifiers will be bound.

Bindings and references are determined through scope sets.

A form is a fragment of program.
A form is represented as syntax-object.
Each syntax-object has an associated scope set.

`(identifier? v)`: Returns `#t` if `v` is a syntax-object and `(syntax-e stx)` produces a symbol
e.g.
```rkt
(identifier? #'abc)  ;;#t arg syntax-obj and expansion returns symbol 'abc
(identifier? (syntax def)) ;;#t arg syntax-obj and expansion returns symbol 'def
(identifier? 15)     ;;#f arg not a syntax obj
(identifier? 'ramen) ;;#f arg not a syntax obj
```

The syntax of a Racket program is defined by

1. a read pass that processes a character stream into a syntax object; and

2. an expand pass that processes a syntax object to produce one that is fully parsed.

For details on the read pass, see The Reader. Source code is normally read in read-syntax mode, which produces a syntax object.

The expand pass recursively processes a syntax object to produce a complete parse of the program. Binding information in a syntax object drives the expansion process, and when the expansion process encounters a binding form, it extends syntax objects for sub-expression with new binding information.



#### readtable

A readtable is consulted at specific times by the reader:

* when looking for the start of a datum;

* when determining how to parse a datum that starts with #;

* when looking for a delimiter to terminate a symbol or number;

* when looking for an opener (such as (), closer (such as )), or . after the first character parsed as a sequence for a pair, list, vector, or hash table; or

* when looking for an opener after #‹n› in a vector of specified length ‹n›.



### void and undefined

`void` identifier is bound to procedure that takes any number
of arguments and returns `#<void>`
```rkt
>(void)
>(void 1 2 3)
>(list (void))
'(#<void>)

(define (returns-nothing)
  (println 'IamPrinted)
  (void)
)
```

undefined: is sometimes used as the result of a reference
whose value is not yet available.

### Macro system

http://www.greghendershott.com/fear-of-macros/Transform_.html

https://docs.racket-lang.org/guide/macros.html

#### Syntax Object

A `syntax object` combines a simple racket value with lexical information, source-location information, syntax properties and tamper status.

The lexical info mentioned above comprises of a set of `scope sets`, one for each `phase level`.

A fully-expanded syntax object corresponds to a parse of a program (i.e., a parsed program), and lexical information on its identifiers indicates the parse.


### Symbol

A sequence that does not start with a delimiter or `#` is parsed as either a `symbol`, a `number` (see Reading Numbers), or a `extflonum` (see Reading Extflonums), except that `.` by itself is never parsed as a symbol or number (unless the `read-accept-dot` parameter is set to `#f`). A `#%` also starts a symbol. The resulting symbol is interned. A successful number or extflonum parse takes precedence over a symbol parse.

As per above definition, quoted strings would be considered symbols.
`(symbol? s)` can be used to find if something is a symbol.

One can think of symbol as a valid sequence that can be used for a variable/identifier.

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