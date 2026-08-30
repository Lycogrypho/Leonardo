package it.grypho.scala.leonardo
package expr

import core._Expression

enum EvalResult:
  case Numeric(value: Double)
  case Symbolic(expr: _Expression)
  case Undefined(reason: String)
