---
title: Interactive REPL
nav_order: 12
---

<img src="logo_bw.svg" alt="" style="height:80px;width:auto;float:right;margin:0 0 8px 16px"/>

# Interactive REPL
<div style="clear:both"></div>

*New to the REPL? **[Lessons: at the prompt](lessons/repl.md)** teaches it in ten short lessons
starting in the browser, with nothing to install. This page is the reference.*

```scala mdoc:silent
import it.grypho.scala.leonardo.cli.Session
```

The REPL provides an interactive session over the library.
Launch with `sbt repl` (or `sbt "replModuleJVM/runMain it.grypho.scala.leonardo.cli.repl"`), in the
[browser](https://lycogrypho.github.io/Leonardo/app/), or with no clone at all through

```
cs launch it.grypho:leonardo-repl_3:@VERSION@ -M it.grypho.scala.leonardo.cli.repl
```

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

Whether an assignment binds a value or defines a function is decided by the right-hand side
alone: a constant expression folds to a value, an expression with free variables becomes a
definition.

A definition whose formula already computes with the current bindings says so on a second
line, `  = …`, so `L := chol(G)` (which computes) and `M := chol(B)` for a `B` that `chol`
refuses no longer echo alike; the definition itself stays late-bound.  In exact mode a matrix
assigned with no name in its formula, such as `A := [[1, 2], [3, 4]]`, echoes the matrix rather
than the formula.

### Freezing a result: `consolidate`

`name := consolidate(expr)` is the opposite of a late-bound definition: the expression is
simplified and evaluated against the current bindings at once, and that snapshot is stored, so a
later change to a binding it used does not reach it:

```scala mdoc:silent
val c = Session()
c.execute("a := 2")
c.execute("p := a + 1")
```

```scala mdoc
c.execute("q := consolidate(p + p)")
c.execute("a := 100")
c.execute("q")
c.execute("p + p")
```

If free variables remain, the simplified form is frozen instead, with the bindings of the moment
folded in.

### Differentiating with respect to a definition

A derivative with respect to a *defined* name applies the chain rule over the definition's
single free variable, `dg/df = (dg/dt) / (df/dt)`; a definition with several free variables is
refused with a message:

```scala mdoc
c.execute("u := t^2")
c.execute("w := u^3")
c.execute("derive(w, u)")
```

The answer is written in the free variable `t`: `3(t²)² · 2t / 2t`, which is `3u²`.
`simplify` does not cancel the common factor `2t`, so the quotient stays as the chain rule
builds it.

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

Both evaluate **every call** first — a derivative, integral or limit, a solver, a statistic, a
function of a number — wherever it appears, so `simplify` never returns something less reduced
than what you gave it (a `solve` inside `simplify` answers without binding its unknown):

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

Exact are `+`, `-`, `*`, `/` and integer powers, and **square roots**: the root of a fraction
is kept as a surd `a + b√d`, displayed `√13` or `(3 + 2√2)` and typed with `√` (`√13`,
`2√13`, `√(x + 1)`), so a quadratic's roots, a 2-norm, a standard deviation and a Cholesky
factor come back exact.  Surds of one root combine exactly, in matrices too — `det`, `inv`, `lu`
and `A^n` work over them — and `:save` writes them in ASCII, `(3 + 2 * 2^(1/2))`.  Everything
else is irrational — every transcendental function, any other fractional power, two different
roots together — so it is computed and then re-approximated to the **working precision**, a
separate setting from the display precision:

```scala mdoc
e.execute("solve(x^2 - 2*x - 1 = 0, x)")
e.execute("unset x_1")
e.execute("unset x_2")
```

```scala mdoc
e.execute("exact precision 40")
e.execute("exact")
```

The working precision runs from 1 to **1000 digits**.  The ceiling is not where the tier stops
being useful — it is where one command stops being interruptible: the cost grows near `n^2.4`,
so a single `pi*e` takes about 0.1 s at 100 digits, 2 s at 1000 and a full minute at 4000.  An
unbounded setting therefore bought an unbounded computation, which a terminal could only end
with Ctrl-C and a browser tab could not end at all.  1000 is still 33× the default.

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

`lu` and `A^n` are exact on an exact matrix, a singular one included for `lu` (a zero on `U`'s
diagonal).  What still computes in `Double`: the iterative decompositions (`qr`, `eigen`,
`eig`), which cannot be exact and demote an exact operand rather than refuse it, and `jordan`
unless every eigenvalue is rational -- then its Jordan form is exact, blocks included.  Square
roots stay exact surds (`√13`, typed back with `√`).

Mixing an exact value with an inexact one gives an inexact result.  That is deliberate:
absorbing the `Double` would be lossless, but it would dress representation error up as an
exact answer.

**The way back is `rationalize(x[, digits])`**, and only when you ask for it: every decimal of
`x` becomes the nearest fraction good for `digits` digits.  Left out, `digits` is the fewest
that turn every decimal into the small fraction it looks like, so a computed `1.9999999999999991`
becomes `2`; when a decimal does not look like one, the default is the working precision, at
most 15 (all a `Double` carries).  A matrix becomes an exact matrix, a symbolic expression
keeps its shape.  A complex number or an interval has no exact form, so the call declines and
says why.  When an exact input comes back as decimals that all look like small fractions, the
REPL offers it in a note, and the offer is exactly what `rationalize(...)` gives; a decimal
that does not look like a small fraction gets no such offer, and a square root needs none (in
exact mode `norm([[2], [3]])` is the exact `√13`):

```scala mdoc
e.execute("qr([[3, 0], [4, 5]])")
e.execute("rationalize(0.75)")
```

## Matrix display

Matrices with two or more rows can be shown multi-line with right-aligned columns
(`pretty on`; `pretty off` restores the single-line form). Single-row matrices and
decomposition results stay on one line. The setting is persisted by `:save` / `:load`,
and the assignment echo honours it, so `A := [[1, 2], [3, 4]]` stacks like any other matrix.

`pretty on` also writes every answer and echo **the way a reader writes it**: the fewest
brackets the grammar needs and subtraction as a minus, so `4 - k*k` answers `4 - k*k` rather
than `(4 + (-1 * (k * k)))`. The text still parses back to the same value, so an answer can be
copied into the prompt; terms keep their order. `pretty off` shows the fully bracketed form
`:save` writes, which `pretty on` leaves unchanged.

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
s.execute("4 - k*k")
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
becomes `limit(f, x, a)`, one-sided arrows and `∞` included; a summation or a product with
its range becomes `sum(f, k, lo, hi)` or `product(f, k, lo, hi)`, and a binomial `binom(n, k)`;
`|x|` becomes `abs(x)`, the modulus included, and `∥x∥` becomes `norm(x)`.  A limit's or derivative's
operand extends to the end of its bracket group — the standard reading — so brackets are how
you confine it.

**What cannot be converted is refused rather than submitted**, and
the reason is the section below: handing the grammar something it does not recognise produces a
confident product, not an error.  A refusal names the spelling to use — an integral missing
its differential names `integral(f, x)`, a limit missing its `(x → a)` names `limit(f, x, a)`,
and a contour integral is refused outright — and leaves the field alone so it can be corrected.

On Enter the transcript shows **both spellings of what you drew**: the formula, typeset, and
the grammar text it became — which is what runs, what the history keeps, and what `:save`
writes, so it is also how you would type the same thing at the prompt.

The editor is only in the browser.  A terminal cannot typeset, which is the same reason
`latex on` adds a channel rather than changing what is printed.

## When a name is invented

The grammar has **no unknown-identifier error**.  An unrecognised name is a perfectly good
variable and juxtaposition is multiplication, so `sqrt(x)` is not an error — it is the *product*
of a free variable named `sqrt` with `x`.  There is no `sqrt` function; a radical is written
`√x`, `x^0.5` or `x^(1/2)` (in exact mode `√13` stays the exact surd).  The same is true of a miscapitalised built-in: `Sin(x)` is `Sin` times
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
The same bounds apply to the browser's `plot`, `points`, `bode`, `nyquist`, `rlocus`, `hist`
and `curve`, which read their arguments through the same code.

**A sample with no finite value is dropped, and the count says so.**  A pole, an argument
outside the domain, or a `sum` whose bound is not a whole number all leave a grid point with
nothing to plot.  Where that happens the table appends `note: only N of M samples had a finite
value` and the browser answers `(plotted N of M samples)`; when nothing is lost, neither says
anything extra.

That last case is **refused rather than drawn**, because it has a remedy the others do not.
`sum`, `product` and `tabulate` need whole bounds, so `sum(k, k, 1, n)` has a value only where
the grid lands exactly on an integer — over `0 … 100` at 200 points the step is `100/199` and
exactly one sample qualifies.  Drawing that one point would be a true picture of nothing you
asked for, its spacing an artefact of where the step fell, so the command says so and names the
grid that fits:

```
> plot sum(k, k, 1, n) n 0 100
plot: sum, product and tabulate need whole bounds, so sum(k, k, 1, n) has a value only where
n is a whole number
note: one point per integer plots it — try: sum(k, k, 1, n) n 0 100 101
```

Taking that advice works, which is what keeps it advice rather than an obstacle — at one point
per integer every sample evaluates, nothing is lost and nothing is refused:

```
plot sum(k, k, 1, n) n 0 100 101      # step = 1.0, so every sample is a whole number
```

The refusal is keyed on the **shape** of the expression — a reduction whose bound is the sampled
variable — together with real loss on this particular grid.  Loss alone would report every pole
as a grid problem, and the shape alone would refuse the grid that already works.

## In a terminal

The terminal prompt has full line editing and history (powered by JLine): the arrow keys edit
the current line and recall earlier commands, which persist across sessions in
`~/.leonardo_history`. `Ctrl-C` abandons the current line without leaving the session;
`Ctrl-D` (or `quit`/`exit`) ends it. When no interactive console is attached (piped input, CI),
the prompt degrades to a plain line reader.

Token-level syntax highlighting colours the input as you type, in one of three schemes chosen
with the `colors` command and persisted by `:save` / `:load`:

| Command | Scheme |
|---|---|
| `colors dark` | bold yellow commands · cyan functions · magenta constants · green numbers (default) |
| `colors light` | bold blue commands · green functions · magenta constants · red numbers |
| `colors none` | no colouring |

## In the browser

The [browser REPL](https://lycogrypho.github.io/Leonardo/app/) runs this same `Session`
compiled to JavaScript. Every command on this page works there unchanged, including `:save`
and `:load` — which keep their exact spelling but write to your browser's storage rather than
to files. Nothing you type leaves the tab.

Nine commands exist **only** there, because a terminal cannot honour them:

| Command | Draws |
|---|---|
| `plot <expr> <var> <lo> <hi> [<n>]` | `y = f(x)` as a line — the arguments are `samples`' arguments |
| `points <expr> <var> <lo> <hi> [<n>]` | the same sampling as points, **with the axes locked to the same scale** |
| `bode <expr> <var> <wMin> <wMax> [<n>]` | gain and **unwrapped** phase over a logarithmic frequency axis |
| `nyquist <expr> <var> <wMin> <wMax> [<n>]` | the same sweep in the complex plane, axes locked |
| `rlocus <G> <var> <kMin> <kMax> [<n>]` | the root locus: the closed-loop poles of `G` over a gain band (geometric when `kMin > 0`), one continuous line per branch, axes locked |
| `hist <sample> [<bins>]` | a histogram over the sample's range, Sturges' number of bins unless given — a trailing integer is the count only when what precedes it is a sample, so `hist y * 2` draws `y * 2`; a cell that is not a real number refuses the sample; `bincounts(x, lo, hi, k)` computes the same counts anywhere |
| `curve <2×1 column> <var> <lo> <hi> [<n>]` | the point the column gives as the variable runs over the range, axes locked: `curve A*[[cos(t)], [sin(t)]] t 0 6.2832` is the unit circle's image under `A` |
| `arrows <2×n matrix>` | an arrow from the origin to each column, axes locked |
| `polygon <2×n matrix>` | the filled polygon whose vertices are the columns (at least three), axes locked |

**Several drawing commands on one line, separated by `;`, draw one figure**, each a layer on
shared axes with a legend: `arrows A ; polygon [[0, 2, 3, 1], [0, 1, 3, 2]]` shows two vectors
and the parallelogram they span.  The axes are locked when any layer is geometry in the plane;
a `bode` diagram has two panels and is drawn alone.

The same lines draw outside the browser too: `Session.figure(lines*)` returns the figure as an
SVG image, from the same session data, which is how a page rendered ahead of time carries a
figure that `/app` redraws from the same text.

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
link restores the settings, bindings and definitions; because it is a fragment, it is never sent
to any server. It carries the environment, not the transcript: **a figure is not in it**, since
`plot` leaves nothing in the session, so a reader draws it again.

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
| `expand <expr>` | Distribute products over sums and collect like terms (a product whose expansion would be deeper than 200 levels, about 200 terms, is left as written, with a note) |
| `eval <expr>` | Force numeric evaluation |
| `samples e v lo hi [n]` | Sample function over range |
| `truth <expr>` | Boolean truth table over the free variables |
| `truth3 <expr>` | Three-valued (Kleene) truth table |
| `logic symmetric on\|off` | Spell truth values as `-1 / 0 / 1` |
| `logic minmax\|product\|lukasiewicz` | Select the fuzzy t-norm family |
| `precision <n>` | Set decimal digits |
| `exact on\|off` | Exact rational arithmetic (default `off`) |
| `exact precision <n>` | Digits an irrational is approximated to (1 to 1000) |
| `pretty on` / `off` | Multi-line, column-aligned matrix display, and answers with the fewest brackets |
| `latex on\|off` | Also render each result as LaTeX (the printed text is unchanged) |
| `names on\|off` | Announce each newly-seen free variable (default `off`) |
| `colors dark\|light\|none` | Syntax-highlight scheme |
| `env` / `vars` | Show session state |
| `unset <name>` | Remove binding or definition |
| `:save <file>` | Write replayable script |
| `:load <file>` | Run a script: restores a saved session's settings, bindings and definitions |
| `help [topic]` | Show help |
| `quit` / `exit` | Leave the REPL |

The [cheat sheet](cheatsheet.md) lists every command and expression form on one page.
