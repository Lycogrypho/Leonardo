---
title: Control Systems
nav_order: 10
---

<img src="logo_bw.svg" alt="" style="height:80px;width:auto;float:right;margin:0 0 8px 16px"/>

# Control Systems
<div style="clear:both"></div>

A [transfer function](https://en.wikipedia.org/wiki/Transfer_function) in Leonardo is **an
ordinary expression** — a `Ratio` of polynomials in a variable you name. There is no
`TransferFunction` type, no model object, and nothing to construct: `1/(s+1)` is one, and so
is anything the rest of the library produces that happens to be rational.

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

`(magnitude, phase-in-radians)` — the data of a
[Bode plot](https://en.wikipedia.org/wiki/Bode_plot) — and `(real, imaginary)` — the data of
a [Nyquist plot](https://en.wikipedia.org/wiki/Nyquist_stability_criterion) — one
computation presented two ways.
At the corner frequency of a first-order lag the magnitude is `1/√2` (−3 dB) and the phase
−45°, as it should be. The whole of the frequency response is a single substitution `s → iω`
riding the existing complex closure; no new arithmetic was written for it.

An integrator has no finite response at `ω = 0`, and that is reported as absence rather than a
fabricated infinity:

```scala mdoc
bode(tf("1/s"), s, 0.0)
```

### Sweeping a band

`bode` and `nyquist` answer for **one** frequency. A diagram needs a vector, and it needs two
things that a plain `sample` cannot give it:

```scala mdoc
frequencyResponse(tf("1/(s+1)"), s, 0.1, 10.0, 5, new Environment())
```

`(ω, magnitude in dB, phase in degrees)` — dB and degrees because that is what a Bode plot
*is*; the raw pair stays available from `bode` itself.

**The grid is geometric, not linear.** A frequency response is read across decades, so a
linear grid of 200 points over `0.01 .. 100` would put 199 of them in the last decade and none
near a corner at `0.1` — the interesting part of the curve is exactly the part it fails to
resolve.

**The phase is unwrapped**, and this is the substance. `atan2` has principal value `(−π, π]`,
so a swept curve jumps a full turn wherever it crosses the branch cut. That jump is an artefact
of the arctangent, not of the plant. A third-order lag tends to −270°:

```scala mdoc
frequencyResponse(tf("1/(s+1)^3"), s, 1.0, 100.0, 3, new Environment()).map(_._3)
```

A raw sweep would report the last of those as `+90`. One caveat worth stating: unwrapping
cannot tell a genuine half-turn step from a grid too coarse to resolve a fast one, so a sparse
sweep across a lightly damped resonance can unwrap the wrong way — the remedy is points.

`nyquistSweep` reads the same grid and returns `(real, imaginary)`, the one sweep presented
two ways that `bode` and `nyquist` already are.

Both take an `Environment`, so a plant carrying a bound parameter folds — which is what lets
the [browser REPL](https://lycogrypho.github.io/Leonardo/app/) draw a diagram from a session
where you wrote `K := 10` first. There, `bode G s 0.01 100` and `nyquist G s 0.01 100` draw
them.

## State space

A [state-space model](https://en.wikipedia.org/wiki/State-space_representation) is a `1×4`
row of matrices — the same shape `lu`, `qr` and `eig` return — so `at(m, 1, k)` indexes it
and a session `:save` round-trips it with no new machinery.
[Controllability](https://en.wikipedia.org/wiki/Controllability) asks whether the input can
steer every state; [observability](https://en.wikipedia.org/wiki/Observability) whether the
output can distinguish them:

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

## Linearising a nonlinear model

A grey-box plant `x' = F(x, u)`, `y = G(x, u)` reaches the linear machinery above through its
[linearisation](https://en.wikipedia.org/wiki/Linearization) at an operating point.
`linearize(F, G, x, u, x0, u0)` returns the state-space row `[[A, B, C, D]]` with
`A = ∂F/∂x`, `B = ∂F/∂u`, `C = ∂G/∂x` and `D = ∂G/∂u` at `(x0, u0)`, so
`A, B, C, D := linearize(…)` binds the four. Here the pendulum at rest:

```scala mdoc
tf("linearize([[x2], [-sin(x1) + u]], [[x1]], [[x1], [x2]], u, [[0], [0]], 0)").eval(env).toExpression.toString
```

The state and input names are an **ordered tuple** — a column of names, or one bare name — and
the column order of `A` is the order given, never inferred. At a point that is not an
equilibrium the model needs its offsets too: `linearize(…, affine)` appends `F0 = F(x0, u0)` and
`G0 = G(x0, u0)`, giving `[[A, B, C, D, F0, G0]]` for `x' ≈ A·δx + B·δu + F0`. A point with a free
parameter gives a symbolic linearisation, and exact input stays exact.

It **declines rather than linearising where the model is not defined**: a point outside the
domain of `F` or `G` (`ln` of a negative), and a value or derivative that is not a real number
there (`abs(x)` has no derivative at `0`), each with the entry named.

## Prediction matrices for MPC

A [model-predictive controller](https://en.wikipedia.org/wiki/Model_predictive_control) writes
the outputs over a prediction horizon `Np` as `Y = Φ·x + Γ·U`, where `U` holds the next `Nc`
input moves. `mpcMatrices(A, B, C, Np, Nc)` returns the row `[[Phi, Gamma]]` — the `lu`/`stateSpace`
shape, so `Phi, G := mpcMatrices(…)` binds both at the prompt:

```scala mdoc
tf("mpcMatrices(0.5, 1, 1, 3, 2)").eval(env).toExpression.toString
```

`Φ` stacks `C·Aⁱ` for `i = 1..Np`; `Γ`'s block `(i, j)` is `C·Aⁱ⁻ʲ·B` below the diagonal and zero
above it. **The last column accumulates**: past `Nc` the last move is *held*, so that column is
`Σ C·Aᵏ·B` — the `1.5` above is `1 + 0.5`. That is stated because the other common convention,
moves beyond `Nc` set to zero, gives a different `Γ` for the same plant. A number is read as a
`1×1` matrix, as here; a name with no value yet is never guessed to be one. The matrices are
built from ordinary products and the block-matrix words (`hcat`, `vcat`, `blkdiag`, `kron`,
`submatrix` — see [Matrices](matrix.md)), so exact input gives exact `Φ` and `Γ`.

## Identifying a model from data (ARX)

A black-box model can be learned from recorded inputs and outputs. An
[ARX model](https://en.wikipedia.org/wiki/Autoregressive_model#Variants) is written here in the
**predictor form**

`y[k] = a₁·y[k−1] + … + a_na·y[k−na] + b₁·u[k−nk] + … + b_nb·u[k−nk−nb+1]`

and **the sign of `a` is stated because it cannot be inferred**: MATLAB's `arx` writes
`A(q) = 1 + a₁q⁻¹ + …`, so its coefficients are the negatives of these (`a_MATLAB = −a`). The
predictor sign is the one every recurrence and regression in this library already uses.
`arx(u, y, na, nb, nk)` fits `[[a, b]]` by least squares; here noise-free data from
`y[k] = 0.8·y[k−1] + 0.5·u[k−1]`:

```scala mdoc:silent
val u = Vector.tabulate(40)(k => math.sin(0.7 * k) + ((k * 7919) % 11 - 5) / 5.0)
val y = u.indices.foldLeft(Vector.empty[Double]) { (acc, k) =>
  acc :+ (if k == 0 then 0.0 else 0.8 * acc(k - 1) + 0.5 * u(k - 1))
}
```

```scala mdoc
arx(_MatrixValue(1, 40, u.toArray), _MatrixValue(1, 40, y.toArray), 1, 1, 1, env)
```

`u` is `m×N` (a row per input) or `N×m`, so a plant with several inputs fits in one call and
`b` comes back `nb×m`, a column per input; several outputs are one call per output. **The delay
`nk` is an argument of the converters**, visible where it acts, so `b` holds exactly what was
estimated: `arx2tf(a, b, nk, z)` gives the discrete transfer function, whose pole is the `0.8`
above, and `arx2ss(a, b, nk)` a state-space row `[[A, B, C, D]]` (the observer canonical form).

```scala mdoc
arx2tf(Vector(0.8), Vector(Vector(0.5)), 1, z).map(_.map(_.toString))
arx2tf(Vector(0.8), Vector(Vector(0.5)), 1, z).toOption.flatMap(gs => poles(gs.head, z))
```

**It declines data that does not excite the model** rather than fitting it. The regressor
matrix must have full numerical rank at the session precision — the relative tolerance `rank`
uses — before least squares is asked: QR alone answers a barely varying input with numbers
that describe rounding, not the plant. A series too short for the orders declines too, and the
REPL says which. With `na = 0` there is no `a`, and `arx` returns `0` in its place, the
absent-group convention of `qp`.

## Simulating a discrete model

`dlsim(sys, u, x0)` runs `x[k+1] = A·x[k] + B·u[k]`, `y[k] = C·x[k] + D·u[k]` over the inputs
`u` (`m×N`, column `k` is `u[k]`) from `x[0] = x0`, and returns the outputs `p×N` — the
prediction-error metric a learned model is judged by. `sys` is the `[[A, B, C, D]]` row that
`stateSpace`, `linearize` and `arx2ss` all build; `x0 = 0` starts from rest, and `D = 0` means no
feedthrough. With the word `states` it returns `[[Y, X]]`, and `X` runs from `x[0]` to **`x[N]`**:
the final state is the one a simulation continues from, so a long run splits into calls with no
gap — where MATLAB's and scipy's `dlsim` stop at `x[N−1]`.

The identity that ties the two halves of this page together: a plant discretised by the
zero-order hold and driven by a unit step reproduces the continuous step response at the sample
instants.

```scala mdoc
c2dExact(tf("[[0, 1], [-2, -3]]"), tf("[[0], [1]]"), 0.1).flatMap { (ad, bd) =>
  dlsim(ad, bd, _MatrixValue(1, 2, Array(1.0, 0.0)), _MatrixValue(1, 1, Array(0.0)),
        _MatrixValue(1, 5, Array.fill(5)(1.0)), _MatrixValue(2, 1, Array(0.0, 0.0))).toOption
}.map(_._1.toVector)
stepResponse(tf("1/(s^2 + 3*s + 2)"), s, t).eval(env.withBinding("t", _Number(0.4))).toExpression.toString
```

An unstable model run long enough leaves the finite numbers; `dlsim` then declines and names the
step, rather than answering with a matrix of `Infinity`.

## LQR and the Kalman filter

`dare(A, B, Q, R)` solves the
[discrete algebraic Riccati equation](https://en.wikipedia.org/wiki/Algebraic_Riccati_equation)
`AᵀPA − P − AᵀPB(R + BᵀPB)⁻¹BᵀPA + Q = 0` for its **stabilising** solution, and
`dlqr(A, B, Q, R)` returns `[[K, P]]`, the gain of the law **`u = −K·x`** that minimises
`Σ xᵀQx + uᵀRu`. The sign is stated because the literature also writes `u = K·x`. The scalar
case `A = B = Q = R = 1` gives the golden ratio:

```scala mdoc
tf("dlqr(1, 1, 1, 1)").eval(env).toExpression.toString
```

The solver is structure-preserving doubling, which needs only products and inverses. Its answer is
**certified** twice: the equation's residual against the `=` tolerance, and the closed loop `A − B·K`
proved stable. So a problem with no stabilising solution is refused, never answered with one that
does not stabilise: `(A, B)` not stabilisable, or an unstable mode `Q` does not penalise. `R` must be
symmetric positive definite and `Q` symmetric positive semidefinite. `dlyap(A, Q)` solves the Stein
equation `A·X·Aᵀ − X + Q = 0` for a stable `A`.

The Kalman filter is the same equation in its dual form. `kalmanGain(A, C, Qw, Rv)` returns
`[[M, P]]`: the steady-state measurement-update gain and the a priori error covariance. The model is
`x[k+1] = A·x + B·u + w`, `y = C·x + D·u + v`, with process noise `w ~ N(0, Qw)` and measurement noise
`v ~ N(0, Rv)`. `kalmanStep(xhat, P, u, y, sys, Qw, Rv)` takes one **update, then predict** step on the
model row and returns `[[xhat', P']]`, the prior for the next step, so its output is its next
input. Here a constant state, believed to be `0` with variance `100`, is measured as `2.9` with
unit noise; one step moves the estimate almost all the way to the measurement and shrinks
the variance below one:

```scala mdoc
val one = tf("kalmanStep(0, 100, 0, 2.9, stateSpace(1, 0, 1, 0), 0, 1)").eval(env).toExpression
one.toString
```

The covariance update is the Joseph form, which stays symmetric positive semidefinite under
rounding where the textbook `(I − M·C)·P` does not.

## Discrete time

Discretisation always names its method — the
[zero-order hold](https://en.wikipedia.org/wiki/Zero-order_hold) `Zoh` or the
[bilinear (Tustin) transform](https://en.wikipedia.org/wiki/Bilinear_transform) `Tustin`.
The same plant discretised the two ways has **different** discrete poles, so a silent
default would make two correct-looking answers disagree with no way to see why.

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

Five more methods cover the rest of the usual menu, each with its convention stated:

| Method | Rule | Note |
|---|---|---|
| `Foh` | `(z − 1)²/(Ts·z) · Z{L⁻¹[G/s²]}`, the triangle hold | keeps the DC gain |
| `ImpulseInvariant` | `Ts · Z{g(n·Ts)}` | **aliases**: faithful only well below `π/Ts`; needs no direct feedthrough |
| `MatchedPoleZero` | roots mapped by `e^(s·Ts)`, all but one zero at infinity to `z = −1` | gain matched at low frequency, integrators included |
| `ForwardEuler` | `s = (z − 1)/Ts` | **can make a stable plant unstable**: pole `p` goes to `1 + p·Ts` |
| `BackwardEuler` | `s = (z − 1)/(Ts·z)` | keeps a stable plant stable |

Prewarped Tustin is not a method of its own but Tustin with a frequency, `c2dPrewarped(G, s, z,
Ts, w)`. It matches the continuous response *exactly* at `w`, where plain Tustin, which squeezes
the whole frequency axis into the Nyquist band, puts a filter's corner somewhere else. Forward
Euler shows its caveat on a fast pole:

```scala mdoc
c2d(tf("1/(s + 30)"), s, z, 0.1, ForwardEuler).flatMap(isStableDiscrete(_, z))
c2d(tf("1/(s + 30)"), s, z, 0.1, BackwardEuler).flatMap(isStableDiscrete(_, z))
```

`d2c` inverts the three substitutions — Tustin, both Euler methods — and `d2cPrewarped` the
prewarped one; the response equivalences and matched pole-zero would need a logarithm and are
declined.

Discrete stability is the **unit circle**, not the left half-plane — applying the continuous
rule to a z-domain function inverts the answer rather than degrading it:

```scala mdoc
isStableDiscrete(tf("1/(z - 0.5)"), z)
isStableDiscrete(tf("1/(z - 1.5)"), z)
```

Exact state-space discretisation uses the
[matrix exponential](https://en.wikipedia.org/wiki/Matrix_exponential) of the block matrix
`[[A, B], [0, 0]]·Ts`, **not** `B_d = A⁻¹(A_d − I)B`. That is a correctness matter: the block
form is defined for a **singular** `A`, and any system with an integrator has one.

```scala mdoc
c2dExact(tf("[[0, 1], [0, 0]]"), tf("[[0], [1]]"), 0.5)
```

## What it declines

The refusals are as much of the design as the features.

A [dead-time](https://en.wikipedia.org/wiki/Dead_time) term is not rational, so it has no
poles — and it is not silently Padé-approximated. `pade` exists, and reaching for it is the
user's decision:

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

The three response equivalences — `zoh`, `foh` and `impulse` — sample the plant's time response
and z-transform it, and the z-transform table has no entry for a damped oscillation
`e^(an)·cos(bn)`. **A plant with a complex pole pair is therefore declined by those three**, not
answered in part: `tustin`, `matched` and the two Euler methods are substitutions and take it
unchanged.

Also absent, deliberately: `d2c` by zero-order hold (it needs a matrix logarithm), root-locus
and margin *plots* (the numbers are here; plotting is not this library's job), and a discrete
Kronecker delta, which `core` has no node for.

## From the REPL

```
> feedback(1/(s*(s+2)), 1, s)
> step(1/(s+1), s, t)
> impulse(1/(s+1), s, t)
> a, b := arx(u, y, 1, 1, 1)
> arx2tf(a, b, 1, z)
> A, B, C, D := arx2ss(a, b, 1)
> tfpoles(1/(s^2 + 3*s + 2), s)
> isStable(1/(s^2 + 3*s + 2), s)
> isStableDiscrete(arx2tf(a, b, 1, z), z)
> c2d(1/(s+1), s, z, 0.1, zoh)
> A, B, C, D := stateSpace([[0, 1], [-2, -3]], [[0], [1]], [[1, 0]], [[0]])
> Y, X := dlsim(arx2ss(a, b, 1), u, 0, states)
```

The poles and zeros are **`tfpoles`/`tfzeros`** at the prompt, because `zeros(r, c)` already
builds a zero matrix and `zeros(G, s)` could not be told from `zeros(3, n)`; in Scala they keep
the names `poles`/`zeros` used above. The roots come back as a row sorted by real part, or
`false` when there are provably none. Every analysis word reads `G` with the session's bound
parameters but never with a binding of the frequency variable itself, and stays as written with
a note when it cannot answer. The `c2d` method is a word in that slot only — `zoh`, `tustin`,
`foh`, `impulse`, `matched`, `forward` and `backward` — and the first six stay free as names
(`impulse` names the impulse response elsewhere). `tustin` may be followed by a prewarp
frequency: `c2d(G, s, z, Ts, tustin, w)`.

Note that `step` is **arity-overloaded**: `step(x)` is the
[Heaviside unit step](https://en.wikipedia.org/wiki/Heaviside_step_function), while
`step(G, s, t)` is the [step response](https://en.wikipedia.org/wiki/Step_response) of a
system.
