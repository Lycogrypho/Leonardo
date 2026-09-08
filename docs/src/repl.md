---
title: Interactive REPL
nav_order: 10
---

<img src="logo_bw.svg" alt="" style="height:80px;width:auto;float:right;margin:0 0 8px 16px"/>

# Interactive REPL
<div style="clear:both"></div>

```scala mdoc:silent
import it.grypho.scala.leonardo.cli.Session
```

The REPL provides an interactive session over the library.
Launch with `sbt repl` (or `sbt "runMain it.grypho.scala.leonardo.cli.repl"`).

`Session.execute(line): String` is the pure, IO-free core — all examples
below use it directly and are verified at documentation build time.

## Value bindings

`:=` with a constant right-hand side binds a numeric value:

```scala mdoc:silent
val s = Session()
```

```scala mdoc
s.execute("x := 3")
```

```scala mdoc
s.execute("x")
```

## Function definitions

`:=` with free variables creates a late-bound definition.
Redefining `f` automatically updates any `g` defined in terms of `f`:

```scala mdoc
s.execute("f := sin(x) + x")
```

```scala mdoc
s.execute("f")          // evaluated at current x = 3
```

```scala mdoc
s.execute("derive(f, x)")
```

## Equations

Bare `=` is always a relation, never a binding:

```scala mdoc
s.execute("10 * x = 2 * x + 1")
```

Store an equation and solve it:

```scala mdoc
s.execute("h := x^2 = 9")
```

```scala mdoc
s.execute("solve(h, x)")
```

## Simplification and expansion

```scala mdoc
s.execute("simplify x + 0 + 1*x")
```

```scala mdoc
s.execute("expand (x + 1)^3")
```

## Environment inspection

```scala mdoc
s.execute("env")
```

## Precision

```scala mdoc
s.execute("precision 8")
```

```scala mdoc
s.execute("sin(x)")     // now at 8-digit precision
```

Reset to default:

```scala mdoc
s.execute("precision 5")
```

## Exact arithmetic

`exact on` switches numeric literals from `Double` to exact rationals, so results that a
binary float cannot represent come out right:

```scala mdoc
val e = new Session()
e.execute("exact on")
e.execute("0.1 + 0.2")     // exactly three tenths, not 0.30000000000000004
e.execute("1/3 * 3")       // exactly one
```

The mode is decided when the input is *parsed*, and it has to be: once `0.1` has been read
as a `Double` the tenth that was meant is already gone, and no later stage can recover it.

Exact are `+`, `-`, `*`, `/` and integer powers.  Everything else is irrational — every
transcendental function, any fractional power — so it is computed and then re-approximated
to the **working precision**, a separate setting from the display precision:

```scala mdoc
e.execute("exact precision 40")
e.execute("exact")
```

Those kernels are genuinely arbitrary-precision, so raising the working precision sharpens
the `sin()` itself and not merely the arithmetic around it:

```scala mdoc
e.execute("exact precision 40")
e.execute("exp(1000)")     // 435 digits; floating point gives Infinity
```

A short result displays as a fraction and a long one as a decimal, but `:save` always
writes the exact fraction, so nothing is lost across a round-trip:

```scala mdoc
e.execute("pi")            // readable
```

Matrices and factorials are exact too:

```scala mdoc
e.execute("exact precision 30")
e.execute("H := [[1, 1/2, 1/3], [1/2, 1/3, 1/4], [1/3, 1/4, 1/5]]")
e.execute("det(H)")        // exactly 1/2160; the Double path reports 4.6E-4
e.execute("H * inv(H)")    // exactly the identity
```

`fact` loses the `170!` ceiling that existed only because a `Double` overflows there —
though a compute cap remains, since an unbounded factorial is easy to type by accident.

What still computes in `Double`: the iterative decompositions (`lu`, `qr`, `eigen`, `eig`,
`jordan`), which cannot be exact whatever their input, and `A^n`.  They demote an exact
operand rather than refuse it; write `A * A` for an exact product.

Mixing an exact value with an inexact one gives an inexact result.  That is deliberate:
absorbing the `Double` would be lossless, but it would dress representation error up as an
exact answer.

## Matrix display

Matrices with two or more rows can be shown multi-line with right-aligned columns
(`pretty on`; `pretty off` restores the single-line form). Single-row matrices and
decomposition results stay on one line. The setting is persisted by `:save` / `:load`.

```scala mdoc
s.execute("pretty on")
```

```scala mdoc
s.execute("[[1, 200], [30, 4]]")
```

```scala mdoc
s.execute("pretty off")
```

## Range sampling

```scala mdoc
s.execute("samples x*x x 0 1 5")
```

## Unset a binding

```scala mdoc
s.execute("unset x")
```

```scala mdoc
s.execute("f")          // x is free again — stays symbolic
```

## Session scripts

`:save file` serialises the session (precision, bindings, definitions) as a
replayable script; `:load file` replays it. The pure `Session.script` and
`Session.load` methods are IO-free:

```scala mdoc:silent
val s2 = Session()
s2.execute("a := 2")
s2.execute("g := a * x")
val script = s2.script
```

```scala mdoc
script
```

Restore into a fresh session:

```scala mdoc:silent
val s3 = Session()
s3.load(script)
```

```scala mdoc
s3.execute("g")
```

## Inline help

```scala mdoc
Session().execute("help :=")
```

Use `help <command>` for any REPL keyword, or bare `help` for the full listing.
`?` is an alias for `help`.

## Quick reference

| Input | Meaning |
|-------|---------|
| `x := 3` | Bind value |
| `f := sin(x)` | Define function |
| `h := lhs = rhs` | Bind equation |
| `lhs = rhs` | Evaluate relation (true/false) |
| `lhs == rhs` | Explicit equality check |
| `simplify <expr>` | Structural simplification |
| `expand <expr>` | Distribute products over sums |
| `eval <expr>` | Force numeric evaluation |
| `samples e v lo hi [n]` | Sample function over range |
| `precision <n>` | Set decimal digits |
| `exact on\|off` | Exact rational arithmetic (default `off`) |
| `exact precision <n>` | Digits an irrational is approximated to |
| `pretty on` / `off` | Multi-line, column-aligned matrix display |
| `env` | Show session state |
| `unset <name>` | Remove binding or definition |
| `:save <file>` | Write replayable script |
| `:load <file>` | Replay a script |
| `help [topic]` | Show help |
| `quit` / `exit` | Leave the REPL |
