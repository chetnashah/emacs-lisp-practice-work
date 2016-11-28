;

;; find more stuff at http://www.cs.sfu.ca/CourseCentral/310/pwfong/Lisp/

(defun factorial (N)
  "Compute the factorial of N"
  (if (eql N 1)
      1
    (* N (factorial (- N 1)))))

(factorial 3)
