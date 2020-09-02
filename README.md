

A note on basic Common Lisp implementations:

* Free, no cost and widely ported: ECL, CLISP(For beginners)
* Free, JVM based: ABCL
* Free with high level of native impl: SBCL, CCL, CMUCL

Scheme implementations :
Guile (primarily linux)
Chicken (linux/windows)
chez
chibi
mit

Racket vs Scheme:

- How empty list is written
- Cons cells are immutable in racket
- How modules work.

### MAjor modes

Major mode is usually loaded when currently focused buffer/file has
a matching extension. Modeline shows currently active major mode.

`M-x describe-mode` shows all the active minor modes (and major mode) and a brief description of each.

The major mode that will load depends on `auto-mode-alist` var.
ONly one major mode can be loaded at a time.

Switching to a different mode:
1. either use a different command e.g. `M-x lisp-mode` switches to lisp mode
2. COmments in file i.e. file variables e.g.
/* Local Variables: */
/* mode: c          */
/* comment-column: 0 */
/* End:             */

### Minor modes

Multiple minor modes can be loaded at a time.
Optional editing mode: can be turned on or off.

Some minor modes are buffer-local, i.e. enabled/disabled depending on buffer charachteristics.
Others are global affecting.

Most minor modes implement features that are independent of the major mode, and can thus be used with most major modes. For example, Auto Fill mode works with any major mode that permits text insertion. A few minor modes, however, are specific to a particular major mode. For example, Diff Auto Refine mode is a minor mode that is intended to be used only with Diff mode.

Ideally, a minor mode should have its desired effect regardless of the other minor modes in effect. It should be possible to activate and deactivate minor modes in any order.



