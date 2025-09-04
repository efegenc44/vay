# _vay!_ programming language

_vay!_ is an expression based, multi paradigm programming language which features a Hindley-Milner type system with multi-argument function, type constructor and structural typeclass (which are called interfaces in _vay!_) extensions.

_vay!_ also provides algebraic data types, imperative constructs and a structured module system.

---
## Quick Start

```
vay help
```

## Examples

More examples can be found [here](./examples).

#### Hello, World!
```
module Main

fun main() = println("Hello, World");
```

#### Fibonacci
```
module Main

fun fibonacci(n : U64) : U64 = match n {
    let 0 : 0
    let 1 : 1
    let n : fibonacci(n - 1) + fibonacci(n - 2)
}

fun main() : Bool =
    fibonacci(8) == 21
```


