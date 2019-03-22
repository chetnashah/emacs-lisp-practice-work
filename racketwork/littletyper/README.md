
In Pie, all functions are total.

Because all functions are total, order in which
subexpressions are evaluated does not matter.

Reason: We have mappings from domains to ranges.
e.g. 
```
f(
    g(),
    h(),
    i()
    j(
        k(),
        l()
    )
)
```
We do not care if `h()` executes first or `l()` or `k()`
Since they are pure mappings

### Eliminator

An eliminator takes apart values built by constructors.

