---
title: Numeric sequences
nav_order: 9
---

<img src="logo_bw.svg" alt="" style="height:80px;width:auto;float:right;margin:0 0 8px 16px"/>

# Numeric sequences
<div style="clear:both"></div>

Leonardo is named after Leonardo Pisano, so the Fibonacci numbers had better work.

```scala mdoc:silent
import it.grypho.scala.leonardo.core.*
import it.grypho.scala.leonardo.scalar.*

val env = new Environment()
val k   = _Variable("k")
```

## Why "numeric" and not "integer"

A linear recurrence needs only addition and multiplication, so it is closed over whatever its
seeds are. `fib(5, 1.5, -pi)` is as meaningful as `fib(5)`, and with exact seeds the exact
tier carries the result through unrounded. Restricting these to integers would be a choice
with nothing to recommend it — hence *numeric* sequences.

The word **series** is deliberately avoided here: in this library it already means the
analytic expansions (Taylor, Fourier, Laurent) described under
[Calculus](calculus.md).

## Fibonacci and its relatives

```scala mdoc
_Sequence(SeqKind.Fibonacci, _Number(10.0)).eval(env)
```

**Indexing is the standard one** — `x(0) = 0`, `x(1) = 1`, so `fib(10) = 55`, agreeing with
[OEIS A000045](https://oeis.org/A000045) and every published table. The "classic rabbit"
pair is written explicitly as `fib(n, 1, 1)`, which is this same sequence shifted by one:
`fib(n, 1, 1) = fib(n+1)`.

All four named sequences are one node over `x(k) = p·x(k-1) + q·x(k-2)`, differing only in
their seeds and coefficients — so each keeps its own name when printed or saved, while the
recurrence has a single definition:

| Function | Sequence | Seeds `(a, b)` | `(p, q)` | First terms |
|---|---|---|---|---|
| `fib(n)` | [Fibonacci](https://en.wikipedia.org/wiki/Fibonacci_sequence) | `(0, 1)` | `(1, 1)` | 0, 1, 1, 2, 3, 5, 8 |
| `lucas(n)` | [Lucas](https://en.wikipedia.org/wiki/Lucas_number) | `(2, 1)` | `(1, 1)` | 2, 1, 3, 4, 7, 11 |
| `pell(n)` | [Pell](https://en.wikipedia.org/wiki/Pell_number) | `(0, 1)` | `(2, 1)` | 0, 1, 2, 5, 12, 29 |
| `jacobsthal(n)` | [Jacobsthal](https://en.wikipedia.org/wiki/Jacobsthal_number) | `(0, 1)` | `(1, 2)` | 0, 1, 1, 3, 5, 11 |

So `lucas(n)` and `fib(n, 2, 1)` are the same number by construction, and `fib(n, a, b)`
covers any other pair of seeds you care to supply.

## Exactness

A `Double` holds Fibonacci numbers exactly only up to `F(78)`; `F(79) = 14472334024676221`
exceeds 2<sup>53</sup>. In exact mode the computation runs over arbitrary-precision integers
instead, so the answer is right rather than merely close:

```
exact on
fib(100)        -> 354224848179261915075
```

Binet's formula `φⁿ/√5` is deliberately *not* used: it is elegant, inexact, and drifts from
the true value around `n = 70`.

## Combinatorial functions

| Function | Meaning | Reference |
|---|---|---|
| `binom(n, k)` | binomial coefficient, generalised to a real upper index | [Binomial coefficient](https://en.wikipedia.org/wiki/Binomial_coefficient) |
| `catalan(n)` | `binom(2n, n)/(n+1)` | [Catalan number](https://en.wikipedia.org/wiki/Catalan_number) |
| `harmonic(n)` | `1 + 1/2 + … + 1/n` | [Harmonic number](https://en.wikipedia.org/wiki/Harmonic_number) |

`harmonic` is rational-valued, which makes it a natural showcase for the exact tier —
`harmonic(4)` is `25/12`, not `2.0833…` — and it agrees with the
[digamma](https://en.wikipedia.org/wiki/Digamma_function) function already in the library by
`H(n) = ψ(n+1) + γ`.

`binom` uses the falling factorial, so a real upper index works: `binom(-1, 3) = -1`.

## `tabulate` — a row of terms

`tabulate(e, k, lo, hi)` evaluates `e` at each integer `k` in `[lo, hi]` and returns a 1×n
matrix. It is **not specific to the sequences**: every function in the library gains a
tabulated form from it.

```scala mdoc
_Tabulate(_Sequence(SeqKind.Fibonacci, k), k, _Number(0.0), _Number(10.0)).eval(env).toExpression.toString
```

```
tabulate(binom(4, k), k, 0, 4)   -> [[1, 4, 6, 4, 1]]     a Pascal row
tabulate(k^2, k, 1, 5)           -> [[1, 4, 9, 16, 25]]
at(tabulate(fib(k), k, 0, 10), 1, 11)  -> 55              rows are 1-based
```

The result is an ordinary matrix, so indexing with `at`, tuple assignment, the matrix
operations and `:save` all work on it unchanged. `k` is a binder — it belongs to the
`tabulate` and is never substituted from outside. The number of terms is capped, so a
mistyped bound declines rather than building an unbounded row.
