

### chap 1

* An atom is a string of charachters beginning with a letter or a number
* All numbers are atoms
* A list (empty or otherwise) is never an atom
* All atoms are S-expressions
* All lists are S-expressions
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

* member? can be implemented using eq?

* First Commandment - Always ask null? as first question in expressing any function (Or in other words put base case first)

* Second commandment - Use cons to build lists







