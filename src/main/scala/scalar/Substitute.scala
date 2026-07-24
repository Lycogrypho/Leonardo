package it.grypho.scala.leonardo
package scalar

import core.*


/** Substitutes named definitions into an expression, replacing every `_Variable` whose
 *  name appears in `definitions` with that definition's body, recursively, so chained
 *  definitions (`g` defined in terms of `f`) resolve in a single call.
 *
 *  Termination: a `seen` set breaks reference cycles -- inside the expansion of `f`, any
 *  further occurrence of `f` is left as a free variable, so self- and mutually-recursive
 *  definitions cannot loop.
 *
 *  Binder positions are never substituted: the variable of differentiation / integration
 *  (and every other binder -- limit, integral transforms, `solveSystem`, `ode`) names the
 *  binder, not a use, so substitution must not touch it.  No special-casing is needed:
 *  binders are excluded from `children` and carried through by `rebuild`, so the generic
 *  `other.rebuild(other.children.map(...))` clause recurses into use positions while
 *  preserving every binder.  Only the `_Variable` case performs the actual replacement.
 *
 *  @param e           the expression to substitute into
 *  @param definitions map from variable name to its replacement body
 *  @return            `e` with every bound name replaced by its definition
 */
def substitute(e: _Expression, definitions: Map[String, _Expression]): _Expression =
  def loop(e: _Expression, seen: Set[String]): _Expression = e match
    case _Variable(n) if definitions.contains(n) && !seen.contains(n) =>
      loop(definitions(n), seen + n)
    case other =>
      other.rebuild(other.children.map(loop(_, seen)))

  loop(e, Set())
