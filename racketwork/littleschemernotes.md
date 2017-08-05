

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

* a relation is a set of pairs
* a relation is a function if no two keys are same in all pairs

ch- friends and relations
* build functions to collect more than one value at a time

* a function is partial if it is not defined for all possible arguments
* otherwise it is a total function

* Eleventh commandment: Use additional arguments when a function needs to know what other arguments to the function have been like so far,

*  or I think we can put them in an outsde closure, like for memoization??

* A combinator is a lambda expression with no free variables

* Y Combinator : It is a higher order function that takes single argument : a function that is not recursive and returns a version of function that is recursive. Or in other words, it is an operator that enables recursion when you can't refer to a function from within itself
(http://mvanier.livejournal.com/2897.html)

* fixpoint of functions: Fixpoint of a given function is a point where input is same as function applied to input. e.g. cos(0.7390) = 0.7390, We say number 0.7390 is the fixpoint of cosine function. The fixpoint is the value x where f(x)=x
* Bear in mind, fixpoint x can be a number, or x can be also a function, if f is a higher order function.

* Fixpoints can be any type of thing, as long as the functions that generate them have same type of input and output.

* A fix point combinator e.g. Y, Z, etc. satisfy following

Y f = f (Y f)
Z f = f (Z f)

an example implementation of Y is 
Y = &#955;f.(&#955;x.f (x x)) (&#955;x.f (x x))

A typical expansion would go like
Y f = f (f (f (... f (Y f))))
thus helping do recursive stuff with non recursive function f.

This above expansion doesn't work for strict languages since execution
never terminates but fine for lazy evaluation

* letrec can define more than one functions like let

* 13th Commandment: use letrec to hide and protect functions


### Let binding

Evaluating a let expression
```
(let ((name1 exp1)
      (name2 exp2)
      ...
      (namek expk)) body)
```
in an environment creates a single frame that binds each expi. This single frame is created after evaluating each expi, and the frame holds all the bindings namei:expi. The body is then evaluated in the context of this new environment that starts at this single frame. Evaluating a let statement is essentially the same as applying a procedure (see below).

On the other hand, evaluating a let* expression
```
(let* ((name1 exp1)
       (name2 exp2)
       ...
       (namek expk)) body)
```
in an environment creates a sequence of new frames with the specified bindings. Each successive expi is evaluated in the environment containing the new frames created by the first i-1 bindings. A new frame is then created with the single binding namei:expi and appended to the front of the environment. Finally the body is evaluated in the environment with the k new frames.
This is not quite equivalent to creating a procedure object with one parameter for each name (see the Application Rule below), since here the bindings are done sequentially and k different frames are created, whereas in the case of procedures, only one new frame is created with all the bindings.


* use letcc to return values early.
