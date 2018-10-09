
### How is racket different from ML?

1. no static type system, accepts more programs but deal with coercion and errors and at runtime yourself.

2. Advanced features like macros, quoting/eval, continuations etc.

### Interpreter

The result of interpreter is a value.

A value is an expression that evaluates to itself.

Typical values found in all languages are : booleans, numbers, strings etc.

All values are valid expressions. but other stuff like variables/terms/programs according to language rules, are also valid expressions.

All expressions evaluate to a value.

For the sake of the current interpreter, closures are values, functions are not.

An environment is mapping from varnames(strings) to values.

All evaluation takes place in an environment(environment model of evaluation along with substitution model of evaluation).

A variable expression evaluation, just looks up its value in the environment.

Evaluating a function definition expression (i.e function definition) results in creation of a closure value. where the env is set to the env at the time of the definition.

Evaluating a function call/application expression:
First evaluate arguments to get parameter values,
evaluate functionname to get closure,
extend the gotten closure with parameter values, and evaluate the body in this environment.


### 

* Every built in value has a commonType/tag let's say theType: number or string or boolean or pair or symbol etc, which usually comes into picture while using predicates like number?, string? on values.

* Each struct definition allows to create a new constructor/tag dynamically adding it to theType.


###

Beta-reduction captures the idea of rewriting/substitution on function application.

Eta-conversion rule captures the idea that a function can be converted to/from a point-free to non point-free form(unnecessary function wrapping) i.e.
``` sml
let f = x -> x * x;;
let f' x = f x;;
(* f' is not point free  where one could have just done let f' = f *)
```
