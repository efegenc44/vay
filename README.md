# _vay!_ programming language

_vay!_ is an expression based, multi paradigm programming language which features a Hindley-Milner type system with multi-argument function, type constructor and structural typeclass (which are called interfaces in _vay!_) extensions.

_vay!_ also provides algebraic data types, imperative constructs and a structured module system.

---

Because _vay!_ right now lacks primitives, writing "Hello, World" is not possible but Peano style natural numbers can be implemented.

While this example does not show all of the features of _vay!_ more examples can be found [here](./examples).

```
module Main

import Main (Nat (Zero, Succ))

variant Nat {
    Zero
    Succ(Nat)

    fun add(this, that : Nat) : Nat = match that {
        let .Zero    : this
        let .Succ(n) : Succ(this.add(n))
    }

    fun mul(this, that : Nat) : Nat = match that {
        let .Zero    : Zero
        let .Succ(n) : this.add(this.mul(n))
    }
}

fun main() : Nat =
    Succ(Succ(Succ(Zero))).mul(Succ(Succ(Zero)))
```

---

### Goals to be useful :
- [ ] Operators
- [ ] Primitives
- [ ] Arrays
- [ ] REPL
- [ ] More imperative constructs
- [ ] Polymorphic methods
