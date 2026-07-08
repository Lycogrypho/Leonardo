package it.grypho.scala.leonardo
package scalar

import core.*


// Whether expression e contains variable v as a free occurrence. Uses the cached
// freeVars set on each node (computed once per node on first call, then O(1)),
// so repeated dependsOn calls on the same expression tree are effectively free.
def dependsOn(e: _Expression, v: _Variable): Boolean =
  e.freeVars.contains(v.variable)
