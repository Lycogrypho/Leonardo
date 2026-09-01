package it.grypho.scala.leonardo
package scalar

import core.*


/** The data-driven table of integrals — issue 6.21's engine, issue 3.7's content.
 *
 *  Every rule here is a `RewriteRule`, not a compiled `case` arm.  That is the whole point
 *  of the hybrid: this table is *expected to keep growing*, one entry at a time, and a rule
 *  added here needs no change to [[integrate]]'s match.
 *
 *  **The integration variable is a pattern variable named `v`.**  A rule is written over the
 *  integrand, and the binder is matched like any other hole — so `∫ tan(v) dv` is the pattern
 *  `Tg(?v)`.
 *
 *  **`?v` may bind a linear function of the integration variable, not only the variable
 *  itself** (issue 3.7).  A pattern matches a *shape*, so `Tg(?v)` alone would cover
 *  `∫ tan(x) dx` and not `∫ tan(2x) dx` — yet the compiled tiers have handled a linear inner
 *  argument since the beginning by dividing through by the slope, and there is no reason for
 *  the table to be weaker.  [[applyTo]] therefore requires `?v` to bind some `a·x + b` with
 *  `a ≠ 0` and divides the instantiated result by `a`.  Every rule in the table gains its
 *  chain-rule case from that one place.
 *
 *  That requirement subsumes the "is it the right variable?" check this table needed before:
 *  `∫ tan(y) dx` binds `?v` to `y`, whose derivative in `x` is zero, so no rule fires.  A
 *  non-linear argument (`∫ tan(x²) dx`) is rejected the same way, which keeps it correctly
 *  unsolved rather than wrongly solved.
 *
 *  **Consulted only after every compiled arm has declined**, so adding a rule cannot change
 *  an integral that already works.  The corollary is that nothing here duplicates a compiled
 *  rule: `1/(1+v²)`, `1/√(1-v²)`, `v·eᵛ`, `ln v`, `sinⁿ v`, `√v` and the rational tier all
 *  close earlier, and a second definition of the same fact is only a way for the two to
 *  disagree.
 *
 *  **Products and sums are matched in the order written.**  Unification is structural, so
 *  `Product(Exp(?v), Sin(?v))` does not match `sin(x)·exp(x)`; the commuted spelling of a
 *  commutative operand pair is a separate entry, because a user types either one.
 */
object integralRules:

  /** The pattern hole standing for the integration variable — or for a linear function of it. */
  private val V: _Expression = _Pattern("v")

  /** The pattern hole standing for a constant exponential base. */
  private val A: _Expression = _Pattern("a")

  /** `Some(antiderivative)` when a rule matches `e` as an integrand in `v`.
   *
   *  @param e the integrand
   *  @param v the integration variable
   *  @return the antiderivative, already simplified, or `None` when no rule applies
   */
  private[scalar] def applyTo(e: _Expression, v: _Variable): Option[_Expression] =
    // Normalise into the canonical spelling first (issue 3.9): `1/cos → sec`, `cos/sin → cot`,
    // so a rule written over the reciprocal-function nodes matches a ratio the user typed.
    val ne = simplifyFully(e)
    rules.iterator
      .flatMap { r =>
        for
          b     <- unify(r.lhs, ne)
          inner <- b.get("v")
          slope <- linearSlope(inner, v)   // ?v must be a linear a·v + b, with a ≠ 0
          if r.condition(b, v)
          out   <- instantiate(r.rhs, b)
        yield if slope == 1.0 then out else Ratio(out, _Number(slope))
      }
      .nextOption()
      .map(simplifyFully)

  /** Admissible bases for `∫ aᵛ dv = aᵛ/ln(a)` (issue 3.8 retired the old refusal).
   *
   *  A **symbolic** base is accepted and answered formally — the variable is now threaded, so
   *  `freeOf("a")` already established it is free of the integration variable.  A **numeric**
   *  base must be `> 0` (or `aᵛ` is not real-valued) and `≠ 1` (or the `ln(a)` denominator
   *  vanishes and the rule would emit a `1/0`-shaped result).  Pair this with `freeOf("a")`.
   */
  private def admissibleExpBase(b: Map[String, _Expression]): Boolean =
    b.get("a").exists { a =>
      a.eval(EmptyEnv) match
        case Right(_Number(d)) => d > 0.0 && d != 1.0   // numeric: real-valued, ln(a) ≠ 0
        case _                 => a.freeVars.nonEmpty    // symbolic base free of v -> formal
    }

  /** The table.  Order is significant — more specific patterns first. */
  private[scalar] val rules: List[RewriteRule] = List(

    // ── trigonometric ────────────────────────────────────────────────────────
    // Written over the first-class sec/csc/cot nodes (issue 3.9); `applyTo` normalises a
    // ratio the user typed (`1/cos`, `cos/sin`) into these before matching.

    // ∫ tan(v) dv = -ln(cos(v))
    RewriteRule(
      lhs  = Tg(V),
      rhs  = Product(_Number(-1), Ln(Cos(V))),
      name = "tan"),

    // NOTE: the integer POWERS tan^n / cot^n / sec^n / csc^n (n >= 2) are handled by the
    // compiled reduction tier (issue 3.11, reduceTanCotPower / reduceSecCscPower in
    // Integrate.scala), which recurses down to these n=1 base cases. No tan^2 / sec^2 / csc^2
    // entries live here: a table rule that never fires (the compiled tier closes it first)
    // would only be a second, silently-diverging definition of the same fact.

    // ∫ cot(v) dv = ln(sin(v))
    RewriteRule(lhs = Cot(V), rhs = Ln(Sin(V)), name = "cot"),

    // ∫ sec(v)·tan(v) dv = sec(v).  The `sin/cos²` spelling does not normalise to a node
    // (its numerator is not 1), so it keeps its own ratio entry beside the product form.
    RewriteRule(lhs = Ratio(Sin(V), Power(Cos(V), _Number(2))), rhs = Sec(V), name = "sec*tan(ratio)"),
    RewriteRule(lhs = Product(Sec(V), Tg(V)), rhs = Sec(V), name = "sec*tan"),
    RewriteRule(lhs = Product(Tg(V), Sec(V)), rhs = Sec(V), name = "tan*sec"),

    // ∫ sec(v) dv = ln((1 + sin(v)) / cos(v))
    // The (1+sin)/cos form is the familiar ln|sec v + tan v| written over a common
    // denominator, which is what this library can spell.
    RewriteRule(
      lhs  = Sec(V),
      rhs  = Ln(Ratio(Sum(_Number(1), Sin(V)), Cos(V))),
      name = "sec"),

    // ∫ csc(v) dv = ln((1 - cos(v)) / sin(v))     [= ln|tan(v/2)|]
    RewriteRule(
      lhs  = Csc(V),
      rhs  = Ln(Ratio(Sum(_Number(1), Product(_Number(-1), Cos(V))), Sin(V))),
      name = "csc"),

    // ∫ sin(v)·cos(v) dv = sin(v)^2 / 2
    RewriteRule(
      lhs  = Product(Sin(V), Cos(V)),
      rhs  = Ratio(Power(Sin(V), _Number(2)), _Number(2)),
      name = "sin*cos"),

    RewriteRule(
      lhs  = Product(Cos(V), Sin(V)),
      rhs  = Ratio(Power(Sin(V), _Number(2)), _Number(2)),
      name = "cos*sin"),

    // ── hyperbolic (issue 3.9) ────────────────────────────────────────────────

    RewriteRule(lhs = Sinh(V), rhs = Cosh(V), name = "sinh"),
    RewriteRule(lhs = Cosh(V), rhs = Sinh(V), name = "cosh"),
    RewriteRule(lhs = Tanh(V), rhs = Ln(Cosh(V)), name = "tanh"),
    RewriteRule(lhs = Coth(V), rhs = Ln(Sinh(V)), name = "coth"),
    RewriteRule(lhs = Power(Sech(V), _Number(2)), rhs = Tanh(V), name = "sech^2"),
    RewriteRule(
      lhs  = Power(Csch(V), _Number(2)),
      rhs  = Product(_Number(-1), Coth(V)),
      name = "csch^2"),
    RewriteRule(lhs = Product(Sech(V), Tanh(V)), rhs = Product(_Number(-1), Sech(V)), name = "sech*tanh"),
    RewriteRule(lhs = Product(Tanh(V), Sech(V)), rhs = Product(_Number(-1), Sech(V)), name = "tanh*sech"),
    RewriteRule(lhs = Product(Csch(V), Coth(V)), rhs = Product(_Number(-1), Csch(V)), name = "csch*coth"),
    RewriteRule(lhs = Product(Coth(V), Csch(V)), rhs = Product(_Number(-1), Csch(V)), name = "coth*csch"),

    // ── exponential and logarithmic ──────────────────────────────────────────

    // ∫ 1/(v·ln(v)) dv = ln(ln(v))   -- listed before the ln(v)/v rule: both involve a
    // logarithm over a ratio, and this one is the more specific shape.
    RewriteRule(
      lhs  = Ratio(_Number(1), Product(V, Ln(V))),
      rhs  = Ln(Ln(V)),
      name = "1/(v ln v)"),

    // ∫ ln(v)/v dv = ln(v)^2 / 2
    RewriteRule(
      lhs  = Ratio(Ln(V), V),
      rhs  = Ratio(Power(Ln(V), _Number(2)), _Number(2)),
      name = "ln(v)/v"),

    // ∫ ln(v)^2 dv = v·ln(v)^2 - 2v·ln(v) + 2v
    RewriteRule(
      lhs  = Power(Ln(V), _Number(2)),
      rhs  = Sum(Sum(Product(V, Power(Ln(V), _Number(2))),
                     Product(_Number(-2), Product(V, Ln(V)))),
                 Product(_Number(2), V)),
      name = "ln^2"),

    // ∫ eᵛ/(1 + eᵛ) dv = ln(1 + eᵛ)      -- both operand orders, since a sum commutes
    RewriteRule(
      lhs  = Ratio(Exp(V), Sum(_Number(1), Exp(V))),
      rhs  = Ln(Sum(_Number(1), Exp(V))),
      name = "exp/(1+exp)"),

    RewriteRule(
      lhs  = Ratio(Exp(V), Sum(Exp(V), _Number(1))),
      rhs  = Ln(Sum(Exp(V), _Number(1))),
      name = "exp/(exp+1)"),

    // ── the cyclic pair ──────────────────────────────────────────────────────
    // Integration by parts reproduces these after two applications, so `MaxPartsDepth`
    // deliberately stops it.  As table entries they are simply closed forms.

    // ∫ eᵛ·sin(v) dv = eᵛ(sin(v) - cos(v)) / 2
    RewriteRule(
      lhs  = Product(Exp(V), Sin(V)),
      rhs  = Ratio(Product(Exp(V), Sum(Sin(V), Product(_Number(-1), Cos(V)))), _Number(2)),
      name = "exp*sin"),

    RewriteRule(
      lhs  = Product(Sin(V), Exp(V)),
      rhs  = Ratio(Product(Exp(V), Sum(Sin(V), Product(_Number(-1), Cos(V)))), _Number(2)),
      name = "sin*exp"),

    // ∫ eᵛ·cos(v) dv = eᵛ(sin(v) + cos(v)) / 2
    RewriteRule(
      lhs  = Product(Exp(V), Cos(V)),
      rhs  = Ratio(Product(Exp(V), Sum(Sin(V), Cos(V))), _Number(2)),
      name = "exp*cos"),

    RewriteRule(
      lhs  = Product(Cos(V), Exp(V)),
      rhs  = Ratio(Product(Exp(V), Sum(Sin(V), Cos(V))), _Number(2)),
      name = "cos*exp"),

    // ── special integral functions (issue 3.15) ──────────────────────────────
    // These antiderivatives are NOT elementary; the named nodes (Si/Ci/Ei/li, Fresnel S/C,
    // erf) ARE the answers, the same convention that lets Gamma be a symbolic node with a
    // numeric kernel. Every rule gains the linear-argument chain rule from applyTo, so
    // ∫ sin(2v)/(2v) closes as Si(2v)/2.

    // ∫ sin(v)/v dv = Si(v)
    RewriteRule(lhs = Ratio(Sin(V), V), rhs = Si(V), name = "sin/v"),

    // ∫ cos(v)/v dv = Ci(v)
    RewriteRule(lhs = Ratio(Cos(V), V), rhs = Ci(V), name = "cos/v"),

    // ∫ eᵛ/v dv = Ei(v)
    RewriteRule(lhs = Ratio(Exp(V), V), rhs = Ei(V), name = "exp/v"),

    // ∫ dv/ln(v) = li(v)
    RewriteRule(lhs = Ratio(_Number(1), Ln(V)), rhs = Li(V), name = "1/ln"),

    // ∫ e^(−v²) dv = (√π/2)·erf(v) — the near-free connection: Erf existed since 4.O,
    // integrate simply never produced it.
    RewriteRule(
      lhs  = Exp(Product(_Number(-1), Power(V, _Number(2)))),
      rhs  = Product(_Number(math.sqrt(math.Pi) / 2), Erf(V)),
      name = "exp(-v^2)"),

    // ∫ sin(πv²/2) dv = fresnelS(v) and ∫ cos(πv²/2) dv = fresnelC(v).  Both spellings of
    // the constant are listed, because they parse differently and neither simplifies into
    // the other: `pi*v^2/2` is Ratio(π·v², 2) while `pi/2*v^2` folds π/2 into one number.
    RewriteRule(
      lhs  = Sin(Ratio(Product(_Number(math.Pi), Power(V, _Number(2))), _Number(2))),
      rhs  = FresnelS(V),
      name = "sin(pi v^2/2)"),
    RewriteRule(
      lhs  = Sin(Product(_Number(math.Pi / 2), Power(V, _Number(2)))),
      rhs  = FresnelS(V),
      name = "sin(pi/2 v^2)"),
    RewriteRule(
      lhs  = Cos(Ratio(Product(_Number(math.Pi), Power(V, _Number(2))), _Number(2))),
      rhs  = FresnelC(V),
      name = "cos(pi v^2/2)"),
    RewriteRule(
      lhs  = Cos(Product(_Number(math.Pi / 2), Power(V, _Number(2)))),
      rhs  = FresnelC(V),
      name = "cos(pi/2 v^2)"),

    // ── rational / parameterised (issue 3.8) ──────────────────────────────────
    // ∫ dv/(a² + v²) = atan(v/a)/a, for `a` free of v and non-zero.  A numeric base closes
    // earlier in the compiled rational tier, so this fires only for a SYMBOLIC `a` — exactly
    // the parameterised capability 3.8 unlocks.  Both operand orders, since a sum commutes.
    RewriteRule(
      lhs       = Ratio(_Number(1), Sum(Power(A, _Number(2)), Power(V, _Number(2)))),
      rhs       = Ratio(Atan(Ratio(V, A)), A),
      condition = freeOf("a") && nonZero("a"),
      name      = "1/(a^2+v^2)"),

    RewriteRule(
      lhs       = Ratio(_Number(1), Sum(Power(V, _Number(2)), Power(A, _Number(2)))),
      rhs       = Ratio(Atan(Ratio(V, A)), A),
      condition = freeOf("a") && nonZero("a"),
      name      = "1/(v^2+a^2)"),

    // ∫ aᵛ dv = aᵛ / ln(a).  LAST: the pattern is the most general in the table, and only the
    // condition keeps it from claiming every power.  Symbolic bases are answered formally;
    // numeric bases are guarded to `> 0` and `≠ 1` (issue 3.8).
    RewriteRule(
      lhs       = Power(A, V),
      rhs       = Ratio(Power(A, V), Ln(A)),
      condition = freeOf("a") && ((b, _) => admissibleExpBase(b)),
      name      = "a^v")
  )
