
### sequences and streams

`Sequences` and `streams` abstract over iteration of elements in a collection. Sequences allow iteration with `for` macros or with sequence operations such as `sequence-map`.

Among built-in datatypes, the sequence datatype includes the following:
1. strings
2. lists
3. mutable lists
4. vectors
5. hash tables
6. dictionaries
7. sets
8. input ports
9. streams
10. exact non-negative numbers

Supported operations:
`sequence? : (s: sequence) => boolean?`


#### Stream

A stream is a kind of sequence that supports functional iteration via `stream-first` and `stream-rest`. The `stream-cons` form constructs a lazy stream, but plain lists can be used as streams, and functions such as `in-range` and `in-naturals` also create streams.
Streams are `lazy`.

* `stream-cons : (f: first-expr, s: second-expr) => stream`: evaluation of both `first-expr` and `second-expr` is delayed until stream operations like `stream-first` or `stream-rest` are called.

* `stream: (s1 s2 ...) => stream`: stream of multiple expressions consed together.

* `stream? : (s: stream) => boolean?`
* `stream-empty? : (s: stream) => boolean?`
* `stream-first : (s: stream) => any`
* `stream-rest : (s: stream) => stream`

e.g. 
```rkt
(define s1 (stream 2 3 (+ 5 3)))
(stream-empty s1)
(stream-first s1)
```

* `empty-stream : () => stream`: returns a stream with no elements.
* `stream->list: (s: stream) => list`
* `stream-length: (s: stream) => exact-nonnegative-integer`
* `stream->ref : (s: stream, i: exact-nonnegative-integer) => any`: return's `ith` element of the stream `s`.

* `stream-append : (s1: stream, s2: stream, ...)`: appends/concat a bunch of streams together and returns resulting stream.

* `stream-filter`
* `stream-map`
* `stream-count`
* `stream-fold`
* `stream-for-each`
* `stream-ormap`
* `stream-andmap`

* `in-stream : (s: stream) => sequence`: Returns a sequence equivalent to stream `s`.

* `(for/stream (for-clause ...) body-or-break ... body)`
* `(for*/stream (for-clause ...) body-or-break ... body)`



