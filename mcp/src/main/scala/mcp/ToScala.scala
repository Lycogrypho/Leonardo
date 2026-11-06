package it.grypho.scala.leonardo
package mcp

import core.*


/** Scala source constructing a value of the library (F_0126 Decision D): what a result is, written
 *  so that it pastes into a program depending on `leonardo` (after `import it.grypho.scala.leonardo.*`)
 *  and, compiled, equals the original.
 *
 *  **Generic over the case classes**: an AST node is rendered from its own constructor fields — the
 *  class's name relative to the library's root package and each field in turn — so a node added later
 *  needs no rule here.  The values with private constructors are written through their factories
 *  (`_Rational.of`, `_Complex.of`, `_Truth.of`, `_Surd.of`, `_Interval.of`, `_Based.of`,
 *  `_Distribution.of`) — the round trip found the last one, the compiler refusing its private
 *  constructor; an enum case is
 *  `Kind.Case`; anything else (a class outside the library, a non-case class, an anonymous one) is
 *  **declined by name** rather than rendered loosely.  The round trip is pinned on the cheat sheet's
 *  whole syntax column, parsed and evaluated, by a generated test source.
 */
object ToScala:

  /** The import the rendered source assumes. */
  val Import = "import it.grypho.scala.leonardo.*"

  private val Root = "it.grypho.scala.leonardo."

  /** Renders `value`.
   *  @param value an expression, a value, or a field of one
   *  @return the source, or the name of what cannot be rendered */
  def apply(value: Any): Either[String, String] = value match
    case r: _Rational =>
      if r.den == BigInt(1) then Right(s"core._Rational(${big(r.num)})")
      else Right(s"core._Rational.of(${big(r.num)}, ${big(r.den)}).get")
    case c: _Complex  => Right(s"core._Complex.of(${dbl(c.re)}, ${dbl(c.im)})")
    case t: _Truth    => Right(s"core._Truth.of(${dbl(t.d)})")
    case b: _Based    => Right(s"core._Based.of(${dbl(b.value)}, ${b.base}, ${b.balanced})")
    case s: _Surd     => for a <- apply(s.a); b <- apply(s.b) yield s"core._Surd.of($a, $b, ${big(s.d)}).get"
    case i: _Interval => for lo <- apply(i.lo); hi <- apply(i.hi) yield s"core._Interval.of($lo, $hi).get"
    case d: probability._Distribution =>
      for k <- apply(d.kind); ps <- apply(d.params) yield s"probability._Distribution.of($k, $ps).get"
    case m: _MatrixValue =>
      Right(s"core._MatrixValue(${m.rows}, ${m.cols}, Array[Double](${m.toVector.map(dbl).mkString(", ")}))")
    case n: _Number   => Right(s"core._Number(${dbl(n.d)})")
    case s: String    => Right(quoted(s))
    case b: Boolean   => Right(b.toString)
    case i: Int       => Right(i.toString)
    case l: Long      => Right(s"${l}L")
    case d: Double    => Right(dbl(d))
    case b: BigInt    => Right(big(b))
    case None         => Right("None")
    case Some(x)      => apply(x).map(s => s"Some($s)")
    case v: Vector[?] => all(v).map(xs => s"Vector(${xs.mkString(", ")})")
    case l: List[?]   => all(l).map(xs => s"List(${xs.mkString(", ")})")
    case e: scala.reflect.Enum if e.productArity == 0 =>
      // a singleton case is an instance of an anonymous subclass of the enum
      val enumClass = Iterator.iterate[Class[?]](e.getClass)(_.getSuperclass).find(!_.getName.contains("$$"))
      enumClass.toRight(e.getClass.getName).flatMap(nameOf).map(n => s"$n.${e.toString}")
    case p: Product   => nameOf(p.getClass).flatMap(n => all(p.productIterator.toVector).map(xs => s"$n(${xs.mkString(", ")})"))
    case other        => Left(Option(other).fold("a missing value")(_.getClass.getName))

  private def all(xs: Iterable[?]): Either[String, Vector[String]] =
    xs.foldLeft[Either[String, Vector[String]]](Right(Vector.empty))((acc, x) => acc.flatMap(v => apply(x).map(v :+ _)))

  /** A library class's name relative to the root package, nested classes joined with `.`. */
  private def nameOf(c: Class[?]): Either[String, String] =
    val n = c.getName
    if !n.startsWith(Root) || n.contains("$$") || n.endsWith("$") then Left(n)
    else Right(n.stripPrefix(Root).replace('$', '.'))

  private def dbl(d: Double): String =
    if d.isNaN then "Double.NaN"
    else if d.isPosInfinity then "Double.PositiveInfinity"
    else if d.isNegInfinity then "Double.NegativeInfinity"
    else _Number.render(d)

  private def big(b: BigInt): String =
    if b.isValidInt then s"BigInt(${b.toInt})" else s"BigInt(\"$b\")"

  private def quoted(s: String): String =
    "\"" + s.flatMap {
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case c if c < ' ' || c > '~' => f"\\u${c.toInt}%04x"
      case c    => c.toString
    } + "\""
