package it.grypho.scala.leonardo
package scalar

import core.*


def dependsOn(e: _Expression, v: _Variable): Boolean = e match
  case _: _Number              => false
  case x: _Variable            => x.variable == v.variable
  case Sum(a, b)               => dependsOn(a, v) || dependsOn(b, v)
  case Product(a, b)           => dependsOn(a, v) || dependsOn(b, v)
  case Ratio(a, b)             => dependsOn(a, v) || dependsOn(b, v)
  case Power(a, b)             => dependsOn(a, v) || dependsOn(b, v)
  case Exp(a)                  => dependsOn(a, v)
  case Log(a)                  => dependsOn(a, v)
  case Sin(a)                  => dependsOn(a, v)
  case Cos(a)                  => dependsOn(a, v)
  case Tg(a)                   => dependsOn(a, v)
  case _Derivative(f, _)       => dependsOn(f, v)
  case _Integral(f, _)         => dependsOn(f, v)
  case _DefIntegral(f, _, l, h) => dependsOn(f, v) || dependsOn(l, v) || dependsOn(h, v)
