https://docs.racket-lang.org/reference/notation.html

* Italic identifiers in grammar are metavariables that correspond to other grammar productions.

* **id** - A metavariable that ends in `id` stands for an identifier.

* **keyword** - A metavariable that ends in `keyword` stands for a `syntax-object keyword`

* **expr** - A metavariable that ends with `expr` stands for any form, and the form will be parsed as an expression.

* **body** - 

* **datum** - A metavariable that ends in datum stands for any form, and the form is normally uninterpreted e.g. quoted

* **number/boolean** - A metavariable that ends with number or boolean stands for a literal number or boolean.

* `...` - The three dots stands for any number(possibly zero) of preceding item. i.e. `form ...` stands for zero or more forms.

Similarly `form ...+` stands for one or more form

E.g you can read syntax
`(lambda formals body ...+)`
as lambda form takes first argument formals
followed by one or more body.

###
TODO add more on modules/contracts etc.