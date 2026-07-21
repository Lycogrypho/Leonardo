package it.grypho.scala.leonardo
package scalar

import core.*


/**
 * Substitution of named definitions into an expression: every _Variable whose name
 * appears in the definitions map is replaced by that definition's body, recursively,
 * so chained definitions (g defined in terms of f) resolve in one call.
 *
 * Termination: a seen set breaks reference cycles — inside the expansion of f, any
 * further occurrence of f is left as a free variable, so self- and mutually-recursive
 * definitions cannot loop.
 *
 * Binder positions are never substituted: the variable of differentiation/integration
 * (and every other binder — limit, the integral transforms, solveSystem, ode) names the
 * binder, not a use, so substitution must not touch it. This needs no special-casing:
 * binders are excluded from `children` (see _Expression.children) and carried through by
 * `rebuild`, so the single generic `other.rebuild(other.children.map(...))` clause recurses
 * into the use positions while preserving every binder. Only the _Variable case is
 * special — it performs the actual replacement.
 */
def substitute(e: _Expression, definitions: Map[String, _Expression]): _Expression =
  def loop(e: _Expression, seen: Set[String]): _Expression = e match
    case _Variable(n) if definitions.contains(n) && !seen.contains(n) =>
      loop(definitions(n), seen + n)
    case other =>
      other.rebuild(other.children.map(loop(_, seen)))

  loop(e, Set())
