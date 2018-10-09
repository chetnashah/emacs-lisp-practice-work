
### Design recipe with Self-referential data definitions

1. If problem is about information of arbitrary size, you need a self-referential data definition to represent it.

A self referential data definition has two parts: a base case and other self referencing with something extra on it.

e.g. A list is either '() or (cons x list)

2. Nothing changes about the header: the signature, purpose and dummy defintion, the purpose should focus on the "what" the function computes rather than how it is computed.

e.g.
``` rkt
;; List-of-Strings -> Number
;; counts how many strings l contains
(define (how-many l)
  0)
```

3. formulating examples, be sure to work through inputs that contain the base clause and the self-referential clause.

4. **Imp** At the core, self referential data definition looks like a data definition of mixed data. like we formulate a cond clause for each clause in data definition. kind of doing pattern matching on the peices of mixed data via cond clauses.

As a result, lot of functions that process self-referential data definitions also structure themselves similarly to data-definitions.

Make a template for the function in this step using wishful thinking.

5. For function body, we specifyy cond lines for each clauses, beginning with the base cases, they have simple answers and do not recurse.


* what are values for base cases?
* what do selector expressions in the recursive clauses compute?
* what do natural recursions compute?
* how can function combine existing values to compute answer?

Finding correct way to combine the values into desired answer isn't always easy.

You can use a table-based approach to figure out.i.e list out values for each example starting from base case.

6. Write test cases that cover all pathways of function.
