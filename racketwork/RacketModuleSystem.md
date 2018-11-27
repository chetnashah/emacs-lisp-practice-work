
### Require

`(require modulename)` - Makes  the definitions
of the module specified by modulename available in current module i.e the current file. 

Require will bring in all the bindings provided by `modulename` into current
file.

But racket will show error when two modules are providing a binding with same
name and we try to require both the modules.

Solution: 
Use facilities like `prefix-in` or `rename-in` etc.
```racket
(require racket/draw)
(require (prefix-in htdp: 2htdp/image))

make-pen      ; the `make-pen` from racket/draw
htdp:make-pen ; the `make-pen` from 2htdp
```

shadowing module-imported definitions is allowed.

e.g.
```racket
#lang racket
 
(require racket/system)

# shadowing imported defintion
(define (system stt)
  (println stt))
 
(system "aha")
```

### `#lang` is shortcut for `(module mname)`

`#lang ` is a shortcut used in files, and cannot be used in REPL.
It is expanded into module declaration using `module` keyword.

https://docs.racket-lang.org/guide/Module_Syntax.html

###