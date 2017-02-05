

;; some completion at point functions
;; try-completion used for completions from a list of possibel strings

(try-completion "b" (list "foo" "baazinga" "badname" "foobaz")) ;; ba


;;region and buffer related functions


;; hooks


;; package, require, loading etc. related functions

;; Loading code means bringing contents into lisp environment in
;; form of lisp objects.
;; Emacs finds and opens the file, reads text, evaluates forms and closes file.
;; just like eval-buffer evaluates all expressions in a buffer
;; Most often, forms in these files are function definitions and variable defnitions


;; 1. basic "load" function 
(load "~/lispwork/testfileforload.el") ;; eval expression (master-name)
;; the variable master-name is loaded from testfileforload.el above
(message (concat "master is " master-name))

;; builtin variable load-path
;; The value of this variable is a list of directories to search when
;; loading files with load function.

;; loading lisp library ??

;;2. autoload facility
;; autoload facility lets you register existence of a function or
;; macro, but put off loading the file that defines it.
;; The first call to the function automatically loads the proper library
;; in order to install real definition and other associated code, then
;; runs real definition like it had been loaded all along.
;; lets say like loading on call..


;;3. with-eval-after-load :
;; with-eval-after-load libraryorfile bodyorfunction..
;; This macro arranges to evaluate body at the end of loading library,
;; each time library is loaded.
;; if library is already loaded, it evaluates body right away
(with-eval-after-load "~/lispwork/testfileforload.el" (message "ahaa"))

;; provide and require

