
Input and output are represented via `ports`.

`ports` are generic, as in a `port` is a generic for all forms of i/o e.g. network, stdin, file etc.

### String port

A string port reads or writes from a byte string. An input string port can be created from either a byte string or a string.
```rkt
> (define names (open-input-string "Günter Harder\nFrédéric Paulin\n"))
> (read-line names)
"Günter Harder"

> (read-line names)
"Frédéric Paulin"
```

### read

`read: (p : Port) => any`
`read-line: (p: Port, mode: any) => string | eof-object`

