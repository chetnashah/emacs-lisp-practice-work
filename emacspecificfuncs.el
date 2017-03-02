

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
;; Instead of installing a function by loading the file that contains it
;; or by evaluating the function definition, you can make function
;; available but not actually install it until it is first called.
;; This is known as autoloading.

;; When you execute an autoloaded function, emacs automatically
;; evaluates file that contains that definition.

;; A good use case for autoload is rarely used functions since they
;; will let your emacs startup quickly and their containing file will
;; load only on demand.

;; (autoload funcName fileName &doc type)
;; e.g. (autoload 'html-helper-mode "html-helper" "Edit docs" t)
;; will load html-helper.el when function html-helper-mode is called.
;; the .el file must be present in load-path

;;3. with-eval-after-load :
;; with-eval-after-load libraryorfile bodyorfunction..
;; This macro arranges to evaluate body at the end of loading library,
;; each time library is loaded.
;; if library is already loaded, it evaluates body right away
(with-eval-after-load "~/lispwork/testfileforload.el" (message "ahaa"))

;;4. provide and require
;; provide and require work in terms of named features.
;; autoloading is triggered by calling specific function, but feature
;; is loaded first time another program asks for it by name.

;; A feature name is symbol stands for collection of functions, variables
;; The file that defines them should "provide" the feature.
;; Usually you will see at the end of x-mode.el files saying (provide 'x-mode)
;; calling provide, will 'var-name to global features list 

;; one can see global feature list with eval-expression: features

;; To require presence of a feature, call require with feature name as arg
;; require looks in global variable features to see whether desired feature has
;; been provided already





