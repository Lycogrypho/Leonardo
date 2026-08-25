package it.grypho.scala.leonardo
package scalar

import core.*


/** The data-driven table of integrals — issue 6.21's payload, and 6.22's home.
 *
 *  Every rule here is a `RewriteRule`, not a compiled `case` arm.  That is the whole point
 *  of the hybrid: this table is *expected to keep growing*, one entry at a time, and a rule
 *  added here needs no change to [[integrate]]'s match.
 *
 *  **The integration variable is a pattern variable named `v`.**  A rule is written over the
 *  integrand, and the binder is matched like any other hole — so `∫ tan(v) dv` is the pattern
 *  `Tg(?v)`.  The condition then checks that `?v` bound to *the actual integration variable*,
 *  which is what stops `∫ tan(y) dx` matching a rule about `x`.  Without that check the table
 *  would fire on integrands that merely have the right shape in the wrong variable.
 *
 *  **Consulted only after every compiled arm has declined**, so adding a rule cannot change
 *  an integral that already works.
 */
object integralRules:

  /** The pattern hole standing for the integration variable. */
  private val V: _Expression = _Pattern("v")

  /** `Some(antiderivative)` when a rule matches `e` as an integrand in `v`. */
  private[scalar] def applyTo(e: _Expression, v: _Variable): Option[_Expression] =
    rules.iterator
      .flatMap { r =>
        unify(r.lhs, e)
          .filter(b => b.get("v").contains(v))   // the hole must be THIS variable
          .filter(r.condition)
          .flatMap(b => instantiate(r.rhs, b))
      }
      .nextOption()
      .map(simplifyFully)

  /** The table.  Order is significant — more specific patterns first. */
  private[scalar] val rules: List[RewriteRule] = List(

    // ∫ tan(v) dv = -ln(cos(v))
    RewriteRule(
      lhs  = Tg(V),
      rhs  = Product(_Number(-1), Ln(Cos(V))),
      name = "tan"),

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
      name = "ln(v)/v")
  )
