

### chap 1

* An atom is a string of charachters beginning with a letter or a number
* All numbers are atoms
* A list (empty or otherwise) is never an atom
* All atoms are S-expressions
* All lists are S-expressions
* An S-expression is either an atom or a (possibly empty) list of S-expressions
* A list is collection of 0 or more S-expressions enclosed by parentheses
* () is a list, not an atom
* car is defined only for non-empty list and it is the first S-expression of the given list

* Cdr Law - cdr is defined only for non-empty lists. cdr of any non-empty list is always another list

* Cons takes an S-expression and a list and puts the S-expression in front of the list

* An empty list is a list with zero S-expressions

* In Lisp () and '() are nil, In Scheme only '()

* Predicate to check empty list is null in Lisp and null? in Scheme.

* The primitive null?/null is defined only for lists.

* eq is for Lisp, eq? is for Scheme

* number? is a primitive function which tells you if given 
atom is number or not



* member? can be implemented using eq?

* First Commandment - Always ask null? as first question in expressing any function (Or in other words put base case first)

* Second commandment - Use cons to build lists

* Fourth Commandment - Always change atleast one argument 
in the list in future call in recursion, so that it is closer
to termination (also known as structural recursion). 
The changing argument must be tested in termination condition:
when using cdr, test termination with null?

* what is the natural recursion on a lst? (cdr lst)

* What is the natural recursion on a number? (sub1 n)

* '() is the identity element when cons is the operator under
consideration

* When recurring over s-expressions and trees in general,
Ask three questions :1 (null? l) 2. (atom? (car l)) 3. (list? l). Essentially recurring bot on car(when car is a list) and a recurring on cdr, instead of just recurring on cdr.

* All *-functons work on lists that are either
  - empty
  - an atom consed onto a list
  - a list consed onto a list
  

* Scheme r5rs eq? vs equal? - eq? is just adress/object and atom comparision (most discriminative), and equal? is content comparision.

* and like most fp languages, comparision of lambdas is
undefined.






