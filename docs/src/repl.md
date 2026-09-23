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

Both reduce a **functional** — a derivative, integral, limit or transform — wherever it appears,
so `simplify` never returns something less reduced than what you gave it:

```scala mdoc
s.execute("simplify 1 + derive(sin(x), x)")
```

Neither folds a **binding**, though, and that is the difference from plain evaluation: with
`x := 2`, `simplify x + 0` is still `x`. Definitions *are* expanded first, so
`g := sin(x)` makes `simplify derive(g, x)` give `cos(x)`.

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

It applies **wherever a number appears in the answer**, not only when the answer is itself a
number — inside a sum, a function argument, an integral, a relation or a matrix operation
alike. It is a display setting only: the computation always carries full `Double` precision,
and `:save` writes the unrounded value, so a result shown as `0.33` still round-trips.

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
decomposition results stay on one line. The setting is persisted by `:save` / `:load`,
and the assignment echo honours it, so `A := [[1, 2], [3, 4]]` stacks like any other matrix.

`pretty` and [`latex`](#latex) are **mutually exclusive**: turning either on turns the
other off and says so. A typeset matrix already carries the layout, so a stacked text beside it
would be the same thing twice.

A matrix **nested inside a larger expression** stacks as well, indented to the column it sits
in, so `[[12, sin(x)], [exp(x), exp(-x)]] * cos(x)` is laid out rather than run onto one line.
Only the outermost matrix stacks: a decomposition result, whose cells are themselves matrices,
stays on one line so the grid survives.

```scala mdoc
s.execute("pretty on")
```

```scala mdoc
s.execute("[[1, 200], [30, 4]]")
```

```scala mdoc
s.execute("pretty off")
```

## The math editor

The [browser REPL](https://lycogrypho.github.io/Leonardo/app/) has a **second, optional input**
below the prompt: a MathLive `<math-field>` where a formula is typed as it looks.  The text
line above it is unchanged and still owns every command — `:save`, `latex off`,
`samples x 0 10`, an assignment — none of which a math field can express.

What the field holds is LaTeX.  Leonardo converts that to its own grammar and submits it
through exactly the same path a typed line takes, so the transcript, the history and every
setting behave identically.

**The calculus notations convert** — an integral written as an integral becomes
`integral(f, x)`, with bounds `integral(f, x, a, b)` and iterated integrals nesting inside
out; `d/dx` (and `∂/∂x`, and `d²/dx²`) becomes `derive(f, x)`; a limit with its subscript
becomes `limit(f, x, a)`, one-sided arrows and `∞` included.  A limit's or derivative's
operand extends to the end of its bracket group — the standard reading — so brackets are how
you confine it.

**What cannot be converted is refused rather than submitted**, and
the reason is the section below: handing the grammar something it does not recognise produces a
confident product, not an error.  A refusal names the spelling to use — a summation reports
`tabulate(f, k, lo, hi)`, an integral missing its differential names `integral(f, x)` — and
leaves the field alone so it can be corrected.

On Enter the transcript shows **both spellings of what you drew**: the formula, typeset, and
the grammar text it became — which is what runs, what the history keeps, and what `:save`
writes, so it is also how you would type the same thing at the prompt.

The editor is only in the browser.  A terminal cannot typeset, which is the same reason
`latex on` adds a channel rather than changing what is printed.

## When a name is invented

The grammar has **no unknown-identifier error**.  An unrecognised name is a perfectly good
variable and juxtaposition is multiplication, so `sqrt(x)` is not an error — it is the *product*
of a free variable named `sqrt` with `x`.  There is no `sqrt` function; a radical is written
`x^0.5` or `x^(1/2)`.  The same is true of a miscapitalised built-in: `Sin(x)` is `Sin` times
`x`.

So the REPL says so:

```
> sqrt(4)
(sqrt * 4.0)
  note: 'sqrt' is not a function here; 'sqrt(...)' parses as a product with 'sqrt'
```

That note is **unconditional** — there is no legitimate reading it could be suppressing.  Each
name is mentioned once per session, so a deliberate `f(2)` does not nag.

`names on` adds the wider report: every newly-seen free variable, once each.

```
> names on
names = on
> a := sin(x)
a := sin(x)
  note: new names: a, x
```

It is **off by default**, because a free variable is the normal case in a CAS — `derive(x^2, x)`
is supposed to have one — so announcing them all is chatty.  It catches what the first note
cannot: a miscapitalised *constant* such as `PI`, which is a variable rather than a call.
Neither note appears while a `:load` script is replaying, whose output is a transcript.

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

It is also empty whenever **the text carries more than the formula**, because a front end
shows the formula *instead of* the text.  Two cases: a `:load`, whose output is the whole
script's transcript rather than one result; and an answer carrying a domain note —
`limit(ln(x), x, -1)` explains that `-1` is outside `ln`'s domain, and that sentence is the
value of the answer, so nothing may replace it.

Fractions, radicals, exact rationals, matrices, the integral / derivative / limit / transform
notations, Greek names — in binder positions too, so `\int … \,d\theta` — a logarithm's base
as a subscript, and the operator macros (`\sin`, not `\mathrm{sin}` — the macro carries the
spacing that tells a reader a function from a product of three letters) all have rules.
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

The count defaults to 200 and is capped at 100 000 — generous for any figure, and refused
rather than attempted beyond that, because the whole grid is built before anything is drawn.
The same bounds apply to the browser's `plot`, `points`, `bode` and `nyquist`, which read
their arguments through the same code.

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
**typeset** in the transcript *beside* its text, by the vendored MathLive build. Both are shown
— the text is the canonical grammar form, the one you would retype or `:save`, so it is never
hidden — and `latex on` turns `pretty` off, since a typeset matrix already carries the layout.
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
| `names on\|off` | Announce each newly-seen free variable (default `off`) |
| `colors dark\|light\|none` | Syntax-highlight scheme |
| `env` / `vars` | Show session state |
| `unset <name>` | Remove binding or definition |
| `:save <file>` | Write replayable script |
| `:load <file>` | Replay a script |
| `help [topic]` | Show help |
| `quit` / `exit` | Leave the REPL |

The [cheat sheet](cheatsheet.md) lists every command and expression form on one page.
