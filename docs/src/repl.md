---
title: Interactive REPL
---

<img src="logo_bw.svg" alt="" height="80" style="float:right;margin:0 0 8px 16px"/>

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
| `env` | Show session state |
| `unset <name>` | Remove binding or definition |
| `:save <file>` | Write replayable script |
| `:load <file>` | Replay a script |
| `help [topic]` | Show help |
| `quit` / `exit` | Leave the REPL |
