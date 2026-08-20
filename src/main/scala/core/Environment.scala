package it.grypho.scala.leonardo
package core


/** The truth-functional semantics selecting the t-norm / t-conorm pair used by the
 *  logic connectives.
 *
 *  All three agree with classical logic on the crisp values `{0, 1}` and differ only on
 *  graded degrees, and all three share the strong negation `1 - a`.  Every one has `0` as
 *  the annihilator of its t-norm and `1` as the annihilator of its t-conorm, which is why
 *  the connective short-circuits are sound under each.
 *
 *  A parameterization of `eval`, never separate packages and never separate nodes.
 */
enum LogicSemantics:
  /** Kleene/Zadeh: `min` / `max`.  The default, and the only one of the three that is a
   *  lattice — so idempotence and absorption hold for graded degrees here alone.
   */
  case MinMax
  /** Probabilistic: `a * b` / `a + b - a * b`. */
  case Product
  /** Lukasiewicz: `max(0, a + b - 1)` / `min(1, a + b)`. */
  case Lukasiewicz


/** Companion holding the global precision constant. */
object Environment:
  /** Canonical default decimal precision for display rounding; single source of truth
   *  across all packages that convert numbers to strings.
   */
  val DefaultPrecision: Int = 5


/** Immutable variable-binding context shared across all domains.
 *
 *  Bindings map names to fully-reduced [[_Value]]s (a number, a matrix, a boolean) —
 *  never to a symbolic expression.  Kept in `core` because `eval` takes one, and `core`
 *  must not depend on any domain.
 *
 *  `withBinding` returns a new `Environment`; the original is unchanged (structural sharing).
 *
 *  @param precision      decimal places used when rendering numbers
 *  @param symmetricLogic when `true`, the digits `{-1, 0, 1}` are read as the symmetric
 *                        ternary spelling of `{false, unknown, true}` in logical
 *                        connective positions, and truth values render in that alphabet.
 *                        This is an *encoding* toggle, not a semantics: the min–max rule
 *                        table is untouched and the two alphabets are related by the
 *                        affine map `t = (s + 1) / 2` (see `core._Truth.fromSymmetric`).
 *                        Defaults to `false`, the `false`/`unknown`/`true` spelling.
 *  @param semantics      the t-norm / t-conorm pair the logic connectives evaluate with;
 *                        defaults to [[LogicSemantics.MinMax]].  Like `symmetricLogic`
 *                        this is a knob on `eval`, not a separate node hierarchy.
 */
class Environment(val precision: Int = Environment.DefaultPrecision,
                  private val variables: Map[String, _Value] = Map(),
                  val symmetricLogic: Boolean = false,
                  val semantics: LogicSemantics = LogicSemantics.MinMax):

  /** Returns the value bound to `variable`, or `None` if it is free.
   *  @param variable the name to look up
   */
  def get(variable: String): Option[_Value] =
    variables.get(variable)

  /** Returns `true` when `variable` has a binding in this environment.
   *  @param variable the name to test
   */
  def isBound(variable: String): Boolean =
    variables.contains(variable)

  /** Returns a new environment with `variable` bound to `value`.
   *  @param variable the name to bind
   *  @param value    the concrete value to associate
   */
  def withBinding(variable: String, value: _Value): Environment =
    new Environment(precision, variables + (variable -> value), symmetricLogic, semantics)
