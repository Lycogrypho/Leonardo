---
title: Control Systems
nav_order: 10
---

<img src="logo_bw.svg" alt="" style="height:80px;width:auto;float:right;margin:0 0 8px 16px"/>

# Control Systems
<div style="clear:both"></div>

A transfer function in Leonardo is **an ordinary expression** — a `Ratio` of polynomials in a
variable you name. There is no `TransferFunction` type, no model object, and nothing to
construct: `1/(s+1)` is one, and so is anything the rest of the library produces that happens
to be rational.

That is a deliberate decision, and it has a visible consequence in every signature on this
page: **the frequency variable is always an explicit argument**, because a plain `Ratio`
carries no type to dispatch on. You write `poles(g, s)`, never `g.poles`.

```scala mdoc:silent
import it.grypho.scala.leonardo.core.*
import it.grypho.scala.leonardo.control.*
import it.grypho.scala.leonardo.parser.Parser

val env = new Environment()
val s   = _Variable("s")
val t   = _Variable("t")
val z   = _Variable("z")

def tf(src: String): _Expression = Parser.parse(src).get
```

## Why there is no carrier

The argument for a dedicated type is real: it would let the frequency variable be implicit and
would give `G * H` a meaning. The argument against it won, on three counts.

A carrier would be a **wall**. `simplify`, `derive`, `integrate`, `substitute`, the exact
tier and the parser all operate on expressions; a transfer function that is not one would need
each of them taught about it, or would be cut off from all of them. As an expression, a
transfer function is differentiable, simplifiable, exactly representable and printable for free.

The one thing a carrier genuinely buys — a result that is a single tidy fraction rather than a
nested one — is supplied here instead: every interconnection returns `N(s)/D(s)` in **lowest
terms**.

And the cost of the decision is paid in documentation, which is what this page is.

## Interconnection

Three connections, each returning one normalised rational.

```scala mdoc
series(tf("1/(s+1)"), tf("2/(s+3)"), s).toString
parallel(tf("1/(s+1)"), tf("1/(s+2)"), s).toString
```

`feedback` is **negative feedback**, `G/(1 + G·H)`:

```scala mdoc
feedback(tf("1/(s*(s+2))"), _Number(1), s).toString
```

**The sign convention is stated because it cannot be inferred.** Negative feedback is the
control convention and what a unity-feedback loop means by default, but a substantial part of
the literature writes the positive form `G/(1 − G·H)`. For positive feedback, negate `h`.
This is the same care the spherical polar angle convention needed: choosing silently would
make every closed-loop result wrong for half its readers.

Notice what the closed loop above returned: `1/(s² + 2s + 1)`, not
`(s² + 2s)/((s² + 2s)(s² + 2s + 1))`. The common factor is divided out, because otherwise the
poles would come back as `{0, −2, −1, −1}` — a confident answer to a different question.

## Poles, zeros and stability

```scala mdoc
poles(tf("(s+2)/((s+1)*(s+3))"), s).map(_.mkString(", "))
zeros(tf("(s+2)/((s+1)*(s+3))"), s).map(_.mkString(", "))
dcgain(tf("5/(s+2)"), s)
```

Poles are `_Value`s, not `Double`s, because **a complex pole is the interesting case** — an
oscillatory mode is a conjugate pair, and reducing the result to reals would silently drop
exactly the systems control theory is about.

```scala mdoc
poles(tf("1/(s^2 + 1)"), s).map(_.mkString(", "))
```

Stability is decided **from the poles**, strictly:

```scala mdoc
isStable(tf("1/(s^2 + 3*s + 2)"), s)
isStable(tf("1/(s - 1)"), s)
isStable(tf("1/(s^2 + 1)"), s)
```

The last one is the marginal case, and it is `false`. An oscillator that never decays is not a
stable system. A [Routh array](https://en.wikipedia.org/wiki/Routh%E2%80%93Hurwitz_stability_criterion)
is available as `routhTable(g, s)` for inspecting a margin by hand — from Scala only, since
it has no grammar production and `routh` is consequently an ordinary variable name, not a
reserved word — but `isStable` does not consult it: Routh's degenerate cases (a zero in the first column, an identically zero row) each
need their own repair, and a mishandled one yields a wrong verdict rather than a refusal. Root
location has neither failure mode.

## Time response

```scala mdoc
stepResponse(tf("1/(s+1)"), s, t).toString
impulseResponse(tf("2/(s+3)"), s, t).toString
```

These are the inverse Laplace transform of `G(s)/s` and of `G(s)` — almost entirely a
re-spelling of machinery that already existed. The response variable `t` is free in the result,
so it can be evaluated, plotted with `sample`, or differentiated like any other expression.

## Frequency response

```scala mdoc
bode(tf("1/(s+1)"), s, 1.0)
nyquist(tf("1/(s+1)"), s, 1.0)
```

`(magnitude, phase-in-radians)` and `(real, imaginary)` — one computation presented two ways.
At the corner frequency of a first-order lag the magnitude is `1/√2` (−3 dB) and the phase
−45°, as it should be. The whole of the frequency response is a single substitution `s → iω`
riding the existing complex closure; no new arithmetic was written for it.

An integrator has no finite response at `ω = 0`, and that is reported as absence rather than a
fabricated infinity:

```scala mdoc
bode(tf("1/s"), s, 0.0)
```

## State space

A model is a `1×4` row of matrices — the same shape `lu`, `qr` and `eig` return — so `at(m, 1, k)`
indexes it and a session `:save` round-trips it with no new machinery.

```scala mdoc
controllable(tf("[[0, 1], [-2, -3]]"), tf("[[0], [1]]"))
controllable(tf("[[1, 0], [0, 2]]"), tf("[[1], [0]]"))
```

Observability is *defined* as controllability of the dual pair `(Aᵀ, Cᵀ)` rather than restated,
so the two can never disagree.

Both answers are **independent of the units the model is written in**, which is less obvious
than it sounds. Rank is decided by QR of the transposed controllability matrix, not by
`det(M·Mᵀ)` against a threshold: a determinant scales like `‖M‖^(2n)`, so with the older test
the same plant with `B` in millivolts rather than volts came back *uncontrollable*. Scaling
`B` cannot change which states the input can reach, and the tests pin that it does not.

## Discrete time

Discretisation always names its method. The same plant discretised by zero-order hold and by
Tustin has **different** discrete poles, so a silent default would make two correct-looking
answers disagree with no way to see why.

```scala mdoc
c2d(tf("1/(s+1)"), s, z, 0.1, Tustin).map(_.toString)
```

Tustin is exactly invertible, which is what makes the round trip return to where it started.
The result below is `2/(s+3)` with a factor of `-0.2` left in both numerator and denominator —
Leonardo does not cancel a common *numeric* factor out of a fraction, so read the value, not
the spelling:

```scala mdoc:silent
val back = c2d(tf("2/(s+3)"), s, z, 0.05, Tustin).flatMap(d2c(_, z, s, 0.05, Tustin))
```

```scala mdoc
back.map(_.toString)
back.map(_.eval(env.withBinding("s", _Number(1.0))))    // 2/(1+3) = 0.5
```

Discrete stability is the **unit circle**, not the left half-plane — applying the continuous
rule to a z-domain function inverts the answer rather than degrading it:

```scala mdoc
isStableDiscrete(tf("1/(z - 0.5)"), z)
isStableDiscrete(tf("1/(z - 1.5)"), z)
```

Exact state-space discretisation uses the matrix exponential of the block matrix
`[[A, B], [0, 0]]·Ts`, **not** `B_d = A⁻¹(A_d − I)B`. That is a correctness matter: the block
form is defined for a **singular** `A`, and any system with an integrator has one.

```scala mdoc
c2dExact(tf("[[0, 1], [0, 0]]"), tf("[[0], [1]]"), 0.5)
```

## What it declines

The refusals are as much of the design as the features.

A dead-time term is not rational, so it has no poles — and it is not silently
Padé-approximated. `pade` exists, and reaching for it is the user's decision:

```scala mdoc
poles(tf("exp(-2*s)/(s+1)"), s).map(_.mkString(", "))
```

An improper `G` has a step response containing an impulse at `t = 0` that the
inverse-transform tier does not represent, so it returns unevaluated rather than handing back
only the smooth part:

```scala mdoc
stepResponse(tf("s^2/(s+1)"), s, t).toString
```

A coefficient whose sign cannot be determined leaves stability undecided, rather than guessed:

```scala mdoc
isStable(tf("1/(s^2 + a*s + 1)"), s)
```

Also absent, deliberately: `d2c` by zero-order hold (it needs a matrix logarithm), root-locus
and margin *plots* (the numbers are here; plotting is not this library's job), and a discrete
Kronecker delta, which `core` has no node for.

## From the REPL

```
> feedback(1/(s*(s+2)), 1, s)
> step(1/(s+1), s, t)
> impulse(1/(s+1), s, t)
```

Note that `step` is **arity-overloaded**: `step(x)` is the Heaviside unit step, while
`step(G, s, t)` is the step response of a system.
