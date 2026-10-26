package it.grypho.scala.leonardo
package cli

import core.*
import scalar.{Sum, Product, Ratio, Power}
import matrix.{MatSum, MatProduct, MatScale}
import equation.{_Equation, _EqualityCheck, _Comparison, CompareOp}

/** The conventional display of an expression (F_0224): grammar text with the fewest brackets
 *  the grammar needs, and subtraction written as a binary minus.
 *
 *  `toString` brackets every operation, `(4 + (-1 * (k * k)))`, because it is the round-trip
 *  format `:save` and share links write.  This is the form a reader expects, `4 - k*k`, shown
 *  under `pretty on` only (the owner's decision) and never stored.  **It must still re-parse to
 *  an expression of the same value**, since a reader copies answers back into the prompt;
 *  `ConventionalTest` checks that on a generated corpus.
 *
 *  - Precedence, loosest first: relation, sum, product, power, atom.  A sum's or a product's
 *    operands lose the brackets associativity makes redundant; a power base, an exponent, the
 *    right operand of `/` and an operand of `*` keep the ones they need, and an operand that
 *    is not first in its operation is bracketed when it starts with a minus (`x*(-y)`).
 *  - **Subtraction** is read the way `latex.ToLatex` reads it: `a + (-1)·b`, a negative
 *    coefficient or a negative number is a term subtracted.  An exact coefficient is negated
 *    exactly, which is safe here since the text is only shown.
 *  - `a + b`, `a - b`, `a = b` are spaced, `a*b`, `a/b`, `a^b` tight.  Terms keep their order.
 *  - **Only the census's infix nodes are written here** (`Sum`, `Product`, `Ratio`, `Power`,
 *    `MatSum`, `MatProduct`, `MatScale`, the relations); every other node prints call or
 *    bracketed syntax, so it is rebuilt over its children's text ([[Session.Rendered]], the
 *    F_0035 mechanism) and keeps its own `toString` -- no printed form is defined twice.
 *  - A leaf's text is atomic only when it is a bare token or fully bracketed (the F_0046
 *    rule), so an exact `2/3` as a power base is `(2/3)^x`.
 */
private[cli] object Conventional:

  private val RelationLevel = 1
  private val SumLevel      = 2
  private val ProductLevel  = 3
  private val PowerLevel    = 4
  private val AtomLevel     = 5

  /** The conventional text of `e`.
   *  @param e    the expression
   *  @param leaf the text of a leaf whose printed form depends on the caller (a value at the
   *              session's precision, a stacked matrix), or `None` for its own `toString`
   *  @return the text
   */
  def render(e: _Expression, leaf: _Expression => Option[String]): String = text(e, leaf)._1

  /** The text of `e` and the precedence level it binds at. */
  private def text(e: _Expression, leaf: _Expression => Option[String]): (String, Int) =
    def at(c: _Expression, min: Int, signed: Boolean = false): String =
      val (t, level) = text(c, leaf)
      if level < min || (!signed && t.startsWith("-")) then s"($t)" else t
    leaf(e) match
      case Some(t) => (t, levelOf(t))
      case None => e match
        case Negated(p) => ("-" + at(p, ProductLevel), ProductLevel)
        case s @ (_: Sum | _: MatSum) =>
          val terms = summands(s)
          val rest  = terms.tail.map {
            case Negated(p) => " - " + at(p, ProductLevel)
            case t          => " + " + at(t, ProductLevel)
          }
          ((at(terms.head, ProductLevel, signed = true) +: rest).mkString, SumLevel)
        case p @ (_: Product | _: MatProduct) =>
          val fs = factors(p)
          ((at(fs.head, ProductLevel, signed = true) +: fs.tail.map(at(_, ProductLevel))).mkString("*"), ProductLevel)
        case MatScale(k, m)   => (at(k, ProductLevel, signed = true) + "*" + at(m, ProductLevel), ProductLevel)
        case Ratio(a, b)      => (at(a, ProductLevel, signed = true) + "/" + at(b, PowerLevel), ProductLevel)
        case Power(b, x)      => (at(b, AtomLevel) + "^" + at(x, PowerLevel), PowerLevel)
        case _Equation(l, r)      => relation(at(l, SumLevel, signed = true), "=", at(r, SumLevel, signed = true))
        case _EqualityCheck(l, r) => relation(at(l, SumLevel, signed = true), "==", at(r, SumLevel, signed = true))
        case _Comparison(l, op, r) =>
          relation(at(l, SumLevel, signed = true), CompareOp.symbol(op), at(r, SumLevel, signed = true))
        case other =>
          val children = other.children
          if children.isEmpty then (other.toString, levelOf(other.toString))
          else (other.rebuild(children.map(c => Session.Rendered(render(c, leaf)))).toString, AtomLevel)

  private def relation(l: String, op: String, r: String): (String, Int) = (s"$l $op $r", RelationLevel)

  /** The terms of a chain of sums of one kind, in order. */
  private def summands(e: _Expression): List[_Expression] = e match
    case Sum(a, b)    => List(a, b).flatMap { case s: Sum => summands(s); case t => List(t) }
    case MatSum(a, b) => List(a, b).flatMap { case s: MatSum => summands(s); case t => List(t) }
    case other        => List(other)

  /** The factors of a chain of products of one kind, in order; a negated factor stays whole. */
  private def factors(e: _Expression): List[_Expression] = e match
    case Product(a, b)    => List(a, b).flatMap { case p: Product if Negated.unapply(p).isEmpty => factors(p); case f => List(f) }
    case MatProduct(a, b) => List(a, b).flatMap { case p: MatProduct => factors(p); case f => List(f) }
    case other            => List(other)

  /** The level a leaf's text binds at: atomic only when nothing could attach to its inside. */
  private def levelOf(raw: String): Int =
    val t = raw.filter(_ >= ' ')   // the stacked-matrix markers are control characters
    if BareToken.matches(t) || fullyBracketed(t) then AtomLevel
    else if t.contains(' ') then SumLevel
    else ProductLevel

  private val BareToken = """\d+(\.\d+)?([eE][-+]?\d+)?|[A-Za-z_][A-Za-z0-9_]*""".r

  /** Whether `t` is one bracketed group, so nothing outside it can bind into it. */
  private def fullyBracketed(t: String): Boolean =
    t.nonEmpty && (t.head == '(' || t.head == '[') && {
      val depths = t.scanLeft(0)((d, c) => if c == '(' || c == '[' then d + 1 else if c == ')' || c == ']' then d - 1 else d)
      depths.last == 0 && depths.tail.init.forall(_ > 0)
    }

  /** A term subtracted, answering its positive counterpart (display only, never stored). */
  private object Negated:
    def unapply(e: _Expression): Option[_Expression] = e match
      case Product(r: _Rational, b) if r.signum < 0  => Some(if r == _Rational(-1) then b else Product(r.negate, b))
      case Product(n: _Number, b) if n.d < 0         => Some(if n.d == -1.0 then b else Product(_Number(-n.d), b))
      case MatScale(r: _Rational, m) if r.signum < 0 => Some(if r == _Rational(-1) then m else MatScale(r.negate, m))
      case MatScale(n: _Number, m) if n.d < 0        => Some(if n.d == -1.0 then m else MatScale(_Number(-n.d), m))
      case r: _Rational if r.signum < 0              => Some(r.negate)
      case n: _Number if n.d < 0                     => Some(_Number(-n.d))
      case _                                         => None
