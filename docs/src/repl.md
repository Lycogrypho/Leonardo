---
title: Interactive REPL
nav_order: 11
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

## LaTeX

`latex on` renders every result as LaTeX source as well.  **It does not change what is
printed**, and that is the whole design: a terminal cannot typeset, so rerouting the answer
would leave it showing markup where a result belongs.  The LaTeX goes on a *second* channel,
`Session.lastLatex`, which a front end that can typeset reads instead — the
[browser REPL](https://lycogrypho.github.io/Leonardo/app/) is the one that does.

```scala mdoc:silent
val sl = Session()
sl.execute("latex on")
sl.execute("simplify (a + b) / (c + d)")
```

```scala mdoc
sl.lastLatex
```

Both pairs of parentheses are **gone**: `\frac` carries its own grouping, so transcribing them
would be correct and unreadable.  That pass — every node reporting the precedence it renders
at, every slot asking for the minimum that needs no brackets — is the substance of the
emitter, and it is also why a power under a power *is* re-bracketed: `x^{2}^{3}` is not ugly,
it is a LaTeX double-superscript error.

```scala mdoc:silent
sl.execute("derive(sin(theta)^2, theta)")
```

```scala mdoc
sl.lastLatex
```

The source carries no `$` or `\[ \]` delimiters, so the caller chooses its own.  The channel
holds the last *expression* and nothing else: after `help`, a setting or an error it is empty,
so a formula can never be shown beside a different command's answer.

Fractions, radicals, exact rationals, matrices, the integral / derivative / limit / transform
notations, Greek names and the operator macros (`\sin`, not `\mathrm{sin}` — the macro carries
the spacing that tells a reader a function from a product of three letters) all have rules.
Anything without one degrades to `\mathrm{…}` of its ordinary spelling, which is plain but
never wrong.

## Range sampling

```scala mdoc
s.execute("samples x*x x 0 1 5")
```

The same sampling backs the browser REPL's plots, so a figure and this table always show the
same numbers — `Session.samplePoints` returns the points and `samples` prints them. Note that
the printed table is rounded to the session precision while the data is not, which is why a
plot is never built from this text.

## In the browser

The [browser REPL](https://lycogrypho.github.io/Leonardo/app/) runs this same `Session`
compiled to JavaScript. Every command on this page works there unchanged, including `:save`
and `:load` — which keep their exact spelling but write to your browser's storage rather than
to files. Nothing you type leaves the tab.

Two commands exist **only** there, because a terminal cannot honour them:

| Command | Draws |
|---|---|
| `plot <expr> <var> <lo> <hi> [<n>]` | `y = f(x)` as a line — the arguments are `samples`' arguments |
| `points <expr> <var> <lo> <hi> [<n>]` | the same sampling as points, **with the axes locked to the same scale** |
| `bode <expr> <var> <wMin> <wMax> [<n>]` | gain and **unwrapped** phase over a logarithmic frequency axis |
| `nyquist <expr> <var> <wMin> <wMax> [<n>]` | the same sweep in the complex plane, axes locked |

For `bode` and `nyquist` the bounds are a frequency **band**, so `wMin` must be greater than
zero — a geometric grid cannot start at zero. See
[Control Systems](control.md#sweeping-a-band) for what the sweep does and why the phase has to
be unwrapped.

Use `points` whenever the picture is *geometry* rather than a function of one variable — a
vector drawn as a coordinate, or anything in the complex plane. On unequal axes a circle looks
like an ellipse, so the lock is a correctness matter rather than a preference.

`latex on` is browser-visible in a way it cannot be in a terminal: with it set, each result is
**typeset** in the transcript instead of printed, by a vendored render-only build of MathLive.
The maths fonts are fetched only when a formula needs them, so leaving the toggle off costs
nothing.

A row of controls above the buttons edits the session settings without typing — precision,
`pretty`, `latex`, the t-norm family, symmetric ternary and the exact tier. **It holds no
state of its own**: it is built from the session's settings and a changed control issues the
same command you would have typed, so the two can never disagree. Type `latex on` at the
prompt and the checkbox moves; give a control a value the session refuses and the field snaps
back with the refusal in the transcript. `colors` is deliberately absent, since the page does
not syntax-highlight and a control that changes nothing visible is worse than none.

The **Copy shareable link** button puts the whole session in the URL fragment. Opening that
link restores the bindings and definitions; because it is a fragment, it is never sent to any
server.

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
| `g := consolidate(expr)` | Freeze the current value/simplified form (not late-bound) |
| `L, U, P := lu(A)` | Tuple binding from a 1×n decomposition result |
| `lhs = rhs` | Evaluate relation (true/false) |
| `lhs == rhs` | Explicit equality check |
| `simplify <expr>` | Structural simplification |
| `expand <expr>` | Distribute products over sums |
| `eval <expr>` | Force numeric evaluation |
| `samples e v lo hi [n]` | Sample function over range |
| `truth <expr>` | Boolean truth table over the free variables |
| `truth3 <expr>` | Three-valued (Kleene) truth table |
| `logic symmetric on\|off` | Spell truth values as `-1 / 0 / 1` |
| `logic minmax\|product\|lukasiewicz` | Select the fuzzy t-norm family |
| `precision <n>` | Set decimal digits |
| `exact on\|off` | Exact rational arithmetic (default `off`) |
| `exact precision <n>` | Digits an irrational is approximated to |
| `pretty on` / `off` | Multi-line, column-aligned matrix display |
| `latex on\|off` | Also render each result as LaTeX (the printed text is unchanged) |
| `colors dark\|light\|none` | Syntax-highlight scheme |
| `env` / `vars` | Show session state |
| `unset <name>` | Remove binding or definition |
| `:save <file>` | Write replayable script |
| `:load <file>` | Replay a script |
| `help [topic]` | Show help |
| `quit` / `exit` | Leave the REPL |

The [cheat sheet](cheatsheet.md) lists every command and expression form on one page.
