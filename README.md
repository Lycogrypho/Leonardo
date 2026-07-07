# Leonardo

## Introduction

Leonardo is a Scala 3 symbolic math library and Computer Algebra System (CAS). The name is an homage to [Leonardo Pisano](https://en.wikipedia.org/wiki/Fibonacci), commonly known as Fibonacci, the Italian mathematician who introduced Arabic numerals and mathematical notation to the Western world.

This project is loosely inspired by the Scala project [Cascala/Galileo]([https://github.com/cascala/galileo|), though the codebase has been completely rewritten from scratch.

## Overview

Leonardo is a lightweight CAS designed to parse, represent, and evaluate mathematical expressions. It builds an Abstract Syntax Tree (AST) from textual input and can evaluate expressions both numerically and symbolically.

### Main Characteristics

- **Expression Parsing**: Parses mathematical expressions using a recursive descent parser based on `scala-parser-combinators`. Supports standard operators, mathematical functions (`sin`, `cos`, `tan`/`tg`, `asin`, `acos`, `atan`, `exp`, `log`), implicit multiplication, multi-character variable names (`theta`, `x1`, `alpha`), and built-in constants `pi` and `e`.

- **Dual Evaluation**: Expressions evaluate to either:
  - A numeric result (`Double`) if all variables are bound
  - A symbolic result (an AST node) if variables remain unbound

- **Variable Binding**: Support for binding variables to numeric values, allowing mixed symbolic-numeric evaluation of complex expressions. Multi-character variable names are fully supported.

- **Rich AST Representation**: Expressions are represented as a type-safe AST with nodes for:
  - Numbers and variables
  - Binary operations (addition, subtraction, multiplication, division)
  - Unary functions (exponential, logarithm, trigonometric)
  - Power operations
  - Higher-order operators (derivatives and integrals — stubs for future development)

- **Precision Control**: Configurable decimal precision for numeric results, with rational approximation semantics.

- **Clean API**: Environment-aware evaluation with no implicit global state. Expressions are immutable and composable.

## Interactive CLI

A minimal REPL ships alongside the library:

```
sbt "runMain it.grypho.scala.leonardo.cli.repl"
```

```
leonardo> x = 3.001            -- bind a value (constant right-hand side)
leonardo> f = sin(x) + x       -- define a function (free variables ⇒ definition)
leonardo> f                    -- evaluate against current bindings
3.14113
leonardo> derive(f, x)         -- differentiation works through definitions
0.00987
leonardo> simplify x + 0       -- structural simplification (ignores bindings)
x
leonardo> precision 8          -- set decimal precision
leonardo> env                  -- list precision, bindings, definitions
leonardo> quit
```

Definitions are late-bound: redefining `f` also changes any `g` defined in terms
of `f`. Whether an assignment binds a value or defines a function is decided by the
right-hand side alone — constant expressions fold to a numeric binding, expressions
with free variables become definitions.

## Planned Features

- Symbolic differentiation and integration
- Expression simplification and normalization
- Support for multi-character variable names
- Additional mathematical functions and constants
