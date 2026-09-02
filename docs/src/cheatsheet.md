---
title: Cheatsheet
nav_order: 10
---


# Leonardo Cheatsheet

Quick reference for the interactive REPL and all mathematical expressions.
For full detail on any command run `help <command>` at the REPL prompt.

---

## REPL session commands

| Command | Description |
|---|---|
| `help` / `?` | Full command listing |
| `help <cmd>` / `? <cmd>` | Detail for one command |
| `env` / `vars` | Show all bindings and definitions |
| `precision <n>` | Set display/comparison precision (default 5) |
| `colors dark\|light\|none` | Syntax-highlight scheme (default `dark`) |
| `pretty on\|off` | Multi-line matrix display (default `off`) |
| `exact on\|off` | Exact rational arithmetic (default `off`) |
| `erf(x)` `erfc(x)` | Error function and its complement |
| `digamma(z)` | Logarithmic derivative of Gamma; makes `Gamma`/`fact` differentiable |
| `gammaP(a,x)` `gammaQ(a,x)` | Regularised incomplete gamma (lower / upper) |
| `betaI(x,a,b)` | Regularised incomplete beta |
| `normal(m,s)` `uniform(a,b)` `exponential(l)` | Continuous distributions |
| `binomial(n,p)` `poisson(l)` | Discrete distributions |
| `pdf(d,x)` `cdf(d,x)` | Density/mass and cumulative distribution |
| `prob(d,lo,hi)` `quantile(d,p)` | Interval probability and inverse cdf |
| `prob(X < 2)` | Probability of a predicate; `and` intersects, `or` is not supported |
| `<` `>` `<=` `>=` `!=` | Comparisons; reduce to `true`/`false` and compose with `and`/`or`/`not` |
| `studentt(nu)` `chisq(k)` | Student-t and chi-squared distributions |
| `mean(x)` `variance(x)` `stddev(x)` | Sample or distribution; `variance` of a sample is the unbiased n-1 form |
| `pvariance(x)` `pstddev(x)` | Population forms, dividing by n |
| `covariance(x,y)` `correlation(x,y)` | Two-sample statistics |
| `regress(X, y)` | Least squares by QR; no intercept added, supply a ones column |
| `ttest(sample, mu)` | Two-sided one-sample t-test p-value |
| `confint(sample, level)` | Confidence interval for the mean, as `[[lo, hi]]` |
| `chisqtest(obs, exp)` | Pearson goodness-of-fit p-value |
| `expect(d)` `variance(d)` | A distribution's own moments |
| `expect(e,X)` `variance(e,X)` | Moments of an expression, by linearity |
| `exact precision <n>` | Digits an irrational is approximated to (default 30) |
| `simplify <expr>` | Structural simplification |
| `expand <expr>` | Distribute products over sums |
| `eval <expr>` | Evaluate substituting current bindings |
| `samples <expr> <v> <lo> <hi> [n]` | Sample function on a grid (default 200 pts) |
| `truth <expr>` | Truth table over the expression's free variables |
| `truth3 <expr>` | Three-valued (Kleene) truth table: false / unknown / true |
| `logic symmetric on\|off` | Spell truth values as -1 / 0 / 1 (default: off) |
| `logic minmax\|product\|lukasiewicz` | Fuzzy t-norm family (default: minmax) |
| `unset <name>` | Remove a binding or definition |
| `:save <file>` | Write session to a replayable script |
| `:load <file>` | Replay a session script from file |
| `quit` / `exit` | Leave the REPL |

---

## Assignment

```
x := 3.14              numeric binding (constant RHS)
f := sin(x) + x        symbolic definition (late-bound: follows redefinitions)
h := 2*x = x + 1      named equation (pass to solve)
g := consolidate(f)    freeze current value of f into g (not late-bound)
L, U, P := lu(A)       tuple binding from a 1×n decomposition result
```

`=` is always an equation relation, never assignment. Use `:=` for binding.

---

## Expression syntax

| Syntax | Meaning |
|---|---|
| `+` `-` `*` `/` | Arithmetic |
| `^` | Power (right-associative: `2^3^2` = `2^9`) |
| `-x` | Unary minus |
| `3sin(x)` / `3x` | Implicit multiplication |
| `(expr)` | Grouping |

### Built-in constants

| Token | Value |
|---|---|
| `pi` | π ≈ 3.14159 |
| `e` | Euler's number ≈ 2.71828 |
| `i` | Imaginary unit (√−1) |
| `inf` | +∞ |
| `-inf` | −∞ |

---

## Mathematical functions

| Expression | Meaning |
|---|---|
| `exp(x)` | eˣ |
| `ln(x)` | Natural logarithm |
| `log(x)` | log₁₀(x) |
| `log(x, b)` | log_b(x) |
| `sin(x)` | Sine |
| `cos(x)` | Cosine |
| `tan(x)` / `tg(x)` | Tangent |
| `asin(x)` | Arcsine |
| `acos(x)` | Arccosine |
| `atan(x)` | Arctangent |
| `sinh(x)` / `cosh(x)` / `tanh(x)` | Hyperbolic sine / cosine / tangent |
| `asinh(x)` / `acosh(x)` / `atanh(x)` | Inverse hyperbolic functions |
| `sec(x)` / `csc(x)` / `cot(x)` | Secant / cosecant / cotangent (`1/cos`, `1/sin`, `cos/sin`) |
| `sech(x)` / `csch(x)` / `coth(x)` | Hyperbolic secant / cosecant / cotangent |
| `step(x)` | Heaviside unit step (1 if x ≥ 0, 0 otherwise) |

`simplify` folds the ratio spelling into the reciprocal node, so there is one form of each:
`1/cos(x)` → `sec(x)`, `cos(x)/sin(x)` → `cot(x)`, `1/cosh(x)` → `sech(x)`, `1/cos(x)^2` → `sec(x)^2`.

---

## Special functions

```
fact(5)                 -> 120.0    exact for integers up to 170!; 171! stays symbolic
fact(0.5)               -> 0.88623  analytic continuation, = Gamma(1.5) = sqrt(pi)/2
dfact(7)                -> 105.0    double factorial 7!! = 7*5*3*1
mfact(10, 3)            -> 280.0    multifactorial, step 3: 10*7*4*1
Gamma(5)                -> 24.0     Gamma(n) = (n-1)!
Gamma(0.5)              -> 1.77245  = sqrt(pi)
lgamma(1e5)                         ln|Gamma| -- finite where Gamma overflows
Beta(1, 4)              -> 0.25     Gamma(a)Gamma(b)/Gamma(a+b)
Γ(5)                -> 24.0     Greek alias; type it with ALT-\ g or ALT-G
β(1, 4)                -> 0.25     Greek alias; type it with ALT-\ b
Si(1)                   -> 0.94608  sine integral (also Ci, Ei, li)
fresnelS(1)             -> 0.43826  Fresnel integrals (also fresnelC)
```

`Gamma` and `Beta` are **capitalised** so that lowercase `gamma` and `beta` remain
usable as ordinary variable names.  The Greek aliases Γ and β parse to the same
nodes; `toString` always emits the ASCII spelling, so `:save` scripts stay portable.
Capital Greek Beta is not accepted: it is a homoglyph of Latin `B`.  Poles (0, -1, -2, ...), overflow and complex
arguments all stay symbolic rather than returning infinities.

---
## Calculus

### Differentiation

```
derive(sin(x), x)               -> cos(x)
derive(x^3, x)                  -> (3.0 * (x ^ 2.0))
derive(f, x, y)                 mixed partial ∂²f/∂x∂y
derive(f, x, x)                 second derivative d²f/dx²
```

### Indefinite integration

```
integral(x^2, x)                -> (1/3) * x^3
integral(exp(x), x)             -> exp(x)
integral(sin(x), x)             -> -cos(x)
integral(1/x, x)                -> ln(x)
integral(step(x), x)            -> x*step(x)
integral(tan(x), x)             -> -ln(cos(x))            from the data-driven table
integral(k^x, x)                -> k^x / ln(k)            symbolic base free of x
integral(1/(a^2 + x^2), x)      -> atan(x/a) / a          symbolic parameter a
integral(x * exp(x^2), x)       -> exp(x^2)/2             non-linear u-substitution (u = x²)
integral(sin(x)^3 * cos(x), x)  -> sin(x)^4/4             u = sin(x)
integral(tan(x)^3, x)           -> tan²/2 + ln(cos x)     tan/sec/csc/cot power reduction (3.11)
integral(1/((x-1)^2*(x-2)), x)                            full partial fractions, repeated root (3.12)
integral(1/((x^2+1)*(x-1)), x)                            partial fractions, complex pair (3.12)
integral(1/(4 - x^2)^0.5, x)    -> asin(x/2)              trig substitution (3.13)
integral((4 - x^2)^1.5, x)                                any half-integer power (3.18)
integral(1/(x^2 + 1)^0.5, x)    -> asinh(x)               hyperbolic substitution (3.13)
integral(1/(2 + cos(x)), x)                               Weierstrass t = tan(x/2) (3.14)
integral(sin(x)/x, x)           -> Si(x)                  special integral functions (3.15)
integral(exp(-x^2), x)          -> sqrt(pi)/2 * erf(x)
integral(1/ln(x), x)            -> li(x)
integral(asin(x), x)            -> x*asin(x) + (1-x^2)^0.5 from the transcribed table (3.16)
integral(sin(2*x)*cos(5*x), x)                            product-to-sum
integral(1/(a^2 - x^2), x)      -> atanh(x/a)/a           symbolic parameter a
integral(1/(x^2 + a^2)^0.5, x)  -> asinh(x/a)
integral(x^a, x)                -> x^(a+1)/(a+1)          symbolic exponent
integral(exp(a*x)*sin(b*x), x)                            general cyclic pair
```

### Vector calculus

A vector field is an n×1 matrix; the coordinate tuple is explicit and **ordered**.

```
grad(x^2*y, x, y)               -> [[2xy], [x^2]]         gradient (n x 1)
div([[x^2], [y^3]], x, y)       -> 2x + 3y^2              divergence (scalar)
curl([[-y], [x], [0]], x, y, z) -> [[0], [0], [2]]        curl (3-D only)
laplacian(x^3*y, x, y)          -> 6xy                    = div(grad(f))
jacobian([[x*y], [y^2]], x, y)  -> [[y, x], [0, 2y]]      m x n
hessian(x^2*y^3, x, y)                                    n x n, symmetric
```

`curl` outside three dimensions, a component/coordinate count mismatch, and a repeated
coordinate all stay symbolic rather than being guessed.

An optional trailing keyword selects the coordinate system (default `cartesian`):

```
laplacian(1/r, r, t, p, spherical)        -> 0        the Newtonian potential is harmonic
div([[1/r], [0], [0]], r, t, z, cylindrical) -> 0     the 2-D point source is source-free
grad(f, r, t, z, cylindrical)             -> [[df/dr], [(1/r)*df/dt], [df/dz]]
```

`cylindrical` is `(r, θ, z)`; `spherical` is `(r, θ, φ)` with **θ the polar angle** (the
physics convention) and `sphericalmaths` is `(r, θ, φ)` with **θ azimuthal and φ polar** (the
mathematics one).  All are three-dimensional, and the coordinates are identified by
**position, not by name** — call them whatever you like, but pass them in that order.  The two
spherical keywords therefore differ in argument *order*, not in naming: pass the polar angle
second for `spherical`, third for `sphericalmaths`.

### Definite integration (Simpson's rule)

```
integral(sin(x), x, 0, pi)      -> ≈ 2.0
```

### Limits

```
limit(sin(x)/x, x, 0)           -> 1.0
limit(1/x, x, 0, +)             -> inf    (from right)
limit(1/x, x, 0, -)             -> -inf   (from left)
limit(atan(x), x, inf)          -> 1.5708 (π/2)
limit((1 + 1/x)^x, x, inf)     -> 2.71828 (e)
```

### Sampling

```
samples sin(x) x -pi pi
samples f x 0 10 500            500 points for defined function f
```

---

## Series expansions

```
maclaurin(exp(x), x, 4)     -> 1 + x + x^2/2 + x^3/6 + x^4/24
maclaurin(sin(x), x, 7)     odd powers only
maclaurin(cos(x), x, 6)     even powers only
maclaurin(1/(1-x), x, 5)    the geometric series
taylor(exp(x), x, 1, 4)     expanded about x = 1
taylor(x^2, x, a, 2)        symbolic centre: a polynomial in (x - a)
fourierSeries(x, x, 2*pi, 4)      sawtooth: 2sin(x) - sin(2x) + (2/3)sin(3x) - ...
fourierSeries(x^2, x, 2*pi, 4)    even function: cosine terms only, a0/2 = pi^2/3
fourierSeries(sin(pi*x), x, 2, 3) period 2, so omega = pi
pade(exp(x), x, 1, 1)             -> (2 + x)/(2 - x)   rational [m/n] approximant
pade(exp(x), x, 2, 2)             beats maclaurin(exp(x), x, 4) away from 0
pade(1/(1+x), x, 0, 1)            a rational function reproduces itself exactly
```

`fourierSeries` expands over one period centred on 0, with coefficients computed
numerically by Simpson — so the period must be a concrete positive number.  It is a
different operation from `fourier(e, t, w)`, the Fourier *transform*.  Coefficients are
not chopped: a term that vanishes analytically returns at the integrator noise floor.

`maclaurin(e, v, n)` is sugar for `taylor(e, v, 0, n)`.  The expansion variable stays
free in the result, so binding it evaluates the polynomial.  Order is capped at 20;
an expression whose derivative stays symbolic (`Gamma`, `fact`) returns unevaluated
rather than a bogus series.

---
## Equations & solving

### Equation relations

```
2*x + 1 = 5                     symbolic equation (evaluates to _Bool when x is bound)
x^2 + 1 == x^2 + 1              equality check (not solvable via solve)
```

### solve — single equation

```
solve(2*x + 1 = 5, x)           -> x = 2.0  (linear)
solve(x^2 = 4, x)               -> [[x = -2.0, x = 2.0]]  (quadratic)
solve(sin(x) = 0, x)            numeric bisection fallback
```

### solve — matrix equation

```
solve(A * X = B, X)             -> X = inv(A) * B
solve(X * A = B, X)             -> X = B * inv(A)
solve(A * X * D = B, X)         -> X = inv(A) * B * inv(D)
solve(A * X + C = B, X)         affine: X = inv(A) * (B - C)
solve(A * X + X * B = C, X)     Sylvester equation (Kronecker vectorization)
```

### solveSystem — linear systems

```
solveSystem([[2*x + y = 3, x - y = 0]], x, y)   -> [[x = 1.0, y = 1.0]]
```

---

## Logic

### Connectives

```
true and false                  -> false   (true/false are literals)
not a                           negation
a implies b                     material implication (right-associative)
a xor b                         exclusive or
a or b and c                    = a or (b and c)
x = 1 and y = 2                 connectives bind looser than "="
false and (1/0 = 0)             -> false   (short-circuit: right side never evaluated)
```

Precedence (tightest to loosest): `not` > `and` > `xor` > `or` > `implies`.

### Three-valued (Kleene) logic

```
unknown                         the third truth value (a bindable value like true/false)
not unknown                     -> unknown   (0.5 is the negation fixpoint)
false and unknown               -> false     (0 annihilates min)
true or unknown                 -> true      (1 annihilates max)
unknown and unknown             -> unknown
unknown implies unknown         -> unknown
```

### Symmetric ternary (an encoding, not a semantics)

```
logic symmetric on              spell truth values as -1 / 0 / 1
logic symmetric off             false / unknown / true (default)
logic symmetric                 show the current setting
-1 and 0                        -> -1   (false and unknown, with the toggle on)
1 or 0                          -> 1
0 and 0                         -> 0
not 0                           -> 0
2 * 3 + 1                       -> 7.0  (arithmetic is never reinterpreted)
1 and 0                         stays symbolic while the toggle is OFF
```

Related by `t = (s + 1) / 2`; the rule table is identical either way.
`:save` persists the toggle but always writes the word spelling.
### Fuzzy logic ([0, 1] degrees)

```
truth(0.3)                      a graded degree (also how one prints, so it round-trips)
truth(0.3) and truth(0.7)       -> truth(0.3)   (min-max)
not truth(0.3)                  -> truth(0.7)
very(0.5)                       -> truth(0.25)  concentration: d^2
somewhat(0.25)                  -> unknown      dilation: sqrt(d) = 0.5
trimf(x, a, b, c)               triangular membership curve
trapmf(x, a, b, c, d)           trapezoidal
gaussmf(x, mean, sigma)         gaussian
sigmf(x, a, c)                  sigmoid
defuzz(trimf(x,0,5,10), x, 0, 10)  -> 5.0   crisp value by centre of gravity
```

### Custom membership curves

The four built-in shapes are a convenience, not a limit: `truth(<expr>)` turns ANY
scalar expression of one variable into a membership curve.

```
truth(1 / (1 + (x - 5)^2))      a Cauchy bell -- a curve of your own
bell := truth(1 / (1 + (x-5)^2))   name it, then use the name
defuzz(bell, x, 0, 10)          -> 5.0    custom curves defuzzify like built-ins
very(bell)                      and compose with hedges and connectives
truth(2)                        stays symbolic: a degree must lie in [0, 1]
defuzz(1/(1+(x-5)^2), x, 0, 10) a bare scalar curve works too (no wrapper needed)
defuzz(trimf(x,0,3,6) or trimf(x,4,7,10), x, 0, 10)
                                aggregate curves with the connectives, then defuzzify
```

### t-norm families

```
logic minmax                    and = min, or = max (default; the only lattice)
logic product                   and = a*b, or = a+b-a*b
logic lukasiewicz               and = max(0,a+b-1), or = min(1,a+b)
logic                           show both logic settings
```

All three agree with classical logic on true/false, so the boolean and ternary tiers
are unaffected.  Idempotence and absorption hold for graded degrees under minmax only.
### Simplification & truth tables

```
simplify a and true             -> a
simplify not not a              -> a
simplify a or (a and b)         -> a   (absorption)
simplify unknown and not unknown  -> unknown   (complement is gated on crisp operands)
truth a and b                   two-valued table over the free variables (max 16)
truth3 a and not a              three-valued table: false / unknown / true (max 10)
```
## Complex numbers

`i` is the imaginary unit; arithmetic with `i` produces complex results automatically.

```
(2 + 3i) * (1 - i)             -> (5.0 + 1.0i)
exp(i * pi)                     -> -1.0  (Euler's formula)
(-1)^0.5                        -> (0.0 + 1.0i)  (principal value)
ln(-1)                          -> (0.0 + 3.14159i)
```

---

## Matrix domain

### Literals

```
[[1, 2], [3, 4]]                2×2 matrix
[[1, 2, 3]]                     1×3 row vector
```

### Constructors

```
eye(n)                          n×n identity matrix
zeros(r, c)                     r×c zero matrix
zeros(n)                        n×n zero matrix
```

### Operations

```
A + B                           element-wise sum
A - B                           element-wise difference
A * B                           matrix product (or scalar multiple if one is a number)
k * A                           scalar multiple
transpose(A)                    transpose
det(A)                          determinant (scalar)
inv(A)                          inverse
A ^ n                           integer matrix power (binary exponentiation)
pow(A, n)                       same as A^n
```

### Decompositions — result is a 1×n row, use `at(result, 1, k)` to index

```
lu(A)                           -> [[L, U, P]]  (P·A = L·U, partial pivoting)
qr(A)                           -> [[Q, R]]     (A = Q·R, modified Gram-Schmidt)
eigen(A)                        -> [[λ₁, …, λₙ]]  eigenvalues only
eig(A)                          -> [[V, D]]     A·V = V·D  (eigenvectors + diagonal)
jordan(A)                       -> [[P, J]]     A = P·J·P⁻¹
```

### Indexing decomposition results

```
L, U, P := lu(A)                tuple binding
at(lu(A), 1, 1)                 first element (L)
at(eigen(A), 1, 2)              second eigenvalue
```

### Scalar functions over matrices (element-wise)

```
sin([[pi/2, 0], [0, pi]])       element-wise sin
exp([[1, 0], [0, 1]])           element-wise exp
```

---

## Integral transforms

### Laplace transform  `L{e(t)}`

```
laplace(1, t, s)                -> 1/s
laplace(t^2, t, s)              -> 2/s^3
laplace(sin(3*t), t, s)         -> 3/(s^2+9)
laplace(exp(2*t)*cos(t), t, s)  -> (s-2)/((s-2)^2+1)  first-shift
laplace(t^3*exp(-t), t, s)      derivative-of-transform rule
```

### Fourier transform  `F{e(t)} = L{e(t)}|_{s=iω}`

```
fourier(exp(-2*t), t, w)        -> 1/(2 + i*w)
fourier(1, t, w)                -> 1/(i*w)
```

### Inverse Laplace transform  `L⁻¹{f(s)}`

```
invlaplace(1/s, s, t)           -> 1
invlaplace(1/s^2, s, t)         -> t
invlaplace(1/(s-3), s, t)       -> exp(3*t)
invlaplace(2/(s^2+4), s, t)     -> sin(2*t)
invlaplace(3/((s-2)^2+9), s, t) -> exp(2*t)*sin(3*t)
```

---

## Ordinary differential equations

First-order IVP  `y' = f(t, y)`,  `y(t₀) = y₀`,  returns `y(target)`.

```
ode(k*y, y, t, 0, 1, 1)        y' = k·y, y(0)=1  -> e^k  (symbolic in k)
ode(-y, y, t, 0, 1, 1)         y' = -y,  y(0)=1  -> 0.36788 (= 1/e)
ode(-y+t, y, t, 0, 1, 1)       y' = -y+t, y(0)=1 -> integrating factor (closed form)
ode(y*y, y, t, 0, 1, 0.5)      nonlinear -> RK4 numeric fallback
```

Closed-form tier covers linear `y' = a(t)·y + b(t)`.
RK4 (4th-order Runge-Kutta) is the numeric fallback for all other shapes.
