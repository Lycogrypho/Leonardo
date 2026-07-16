---
title: Getting Started
---

<img src="logo_bw.svg" alt="" height="80" style="float:right;margin:0 0 8px 16px"/>

# Getting Started
<div style="clear:both"></div>

## Prerequisites

- Scala 3.3 or later
- sbt 1.9 or later

## Adding Leonardo to your project

> **Note**: Leonardo is not yet published to Maven Central (see roadmap).
> To use it locally, clone the repository and run `sbt publishLocal`, then
> add the following to your `build.sbt`:

```scala
libraryDependencies += "it.grypho.scala" %% "leonardo" % "@VERSION@"
```

## Import conventions

Leonardo is split across focused packages. Import what you need:

```scala mdoc:silent
import it.grypho.scala.leonardo.core.*      // _Expression, _Value, _Number, _Variable, Environment
import it.grypho.scala.leonardo.scalar.*    // Sum, Product, Power, Sin, derive, simplify, …
import it.grypho.scala.leonardo.matrix.*    // _Matrix, MatSum, MatProduct, Transpose, Determinant, Inverse
import it.grypho.scala.leonardo.equation.*  // _Equation, solve, solveSystem
import it.grypho.scala.leonardo.parser.Parser
import it.grypho.scala.leonardo.cli.Session
```

## Parsing

The `Parser` converts a string into an expression tree:

```scala mdoc
Parser.parse("sin(x)^2 + cos(x)^2").get.toString
```

The result is immutable; the same tree object can be evaluated many times with
different environments.

Parsing failures return a `Failure` result rather than throwing:

```scala mdoc
val bad = Parser.parse("sin(")
bad.successful
```

## Evaluating

An expression evaluates to either a concrete `_Value` (wrapped in `Right`) or a
symbolic remainder (wrapped in `Left`):

```scala mdoc:silent
val x    = _Variable("x")
val expr = Sum(Sin(x), Cos(x))
```

Without any binding, the expression stays symbolic:

```scala mdoc
expr.eval(new Environment())
```

Bind `x` and the result folds to a number:

```scala mdoc
val env = new Environment(5, Map("x" -> _Number(0.0)))
expr.eval(env)
```

## Precision

`Environment` carries a decimal precision used for display and rounding.
The default is `5`; override it per call:

```scala mdoc
val highPrecision = new Environment(10, Map("x" -> _Number(math.Pi / 4)))
expr.eval(highPrecision)
```

## Using the REPL

Launch with `sbt repl` (or `sbt "runMain it.grypho.scala.leonardo.cli.repl"`).
Programmatically, `Session.execute` is the pure testable core:

```scala mdoc:silent
val s = Session()
s.execute("x := 1.5708")   // π/2 ≈ 1.5708
```

```scala mdoc
s.execute("sin(x) + cos(x)")
```
