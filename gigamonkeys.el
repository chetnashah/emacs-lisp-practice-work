

;; starts from http://www.gigamonkeys.com/book/practical-a-simple-database.html

;; normal lisp list
(list 1 2 3)

;; another flavor of list is property-list or plist
;; list item occur as :symbol value pair
;; keywords are used as names of pair (coz they start with :)

(list :a 1 :b 2 :c 3)

;; the advantage of plists comes by using a function known as "getf"
;; getf takes a list and a keyword and returns value for keyword

;; getf usage with plists
(getf (list :a 1 :b 2) :a) ;; 1
(getf (list :a 1 :b 2) :b) ;; 2

;;; plists can be useful for making structs or record objects
;; e.g. make-cd

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(make-cd "Roses" "kathy" 3 t)  ;; make a cd record

(getf (make-cd "Roses" "kathy" 3 t) :rating) ;; 3 , get rating of the made cd record


;; to make a data base we need more than a record, we need a list of records
;; or in our case list of lists

;; convention global variables have asteriks, we have a global database variable
(defvar *db* nil)

;; we will implement CRUD 

;; insert or add-record to *db*
(defun add-record (cd)
  (push cd *db*))

(add-record (make-cd "Roses" "kathy" 7 t))
(add-record (make-cd "Fly" "Dixie" 8 t))
(add-record (make-cd "Home" "Dixie" 9 t))

*db* ;; 3 records


;; need to understand format
;; function to print *db*
(defun dump-db ()
  (dolist (ab *db*)
    (format t "~{~a:~10t~a~%~}~%" ab)))

(dump-db)
