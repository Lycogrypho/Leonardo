package it.grypho.scala.leonardo
package core


/** An integer that remembers the base it is written in — issue 3.5.
 *
 *  The **fourth sibling** of [[_Number]], after [[_Complex]], [[_Truth]] and [[_Rational]],
 *  and built to the same pattern: a private constructor behind a smart factory, and a value
 *  that is *read as a number* through `_Number`'s widening extractor so no existing
 *  `case _Number(x)` site has to learn about it.
 *
 *  **Why the base lives on the value.**  A base conversion that returns bare digits is only
 *  a decomposition: `[[15, 15]]` is indistinguishable from a matrix typed by hand, and the
 *  caller has to remember the base to reassemble it.  Putting the base on the value is what
 *  makes `A := 0xFF` still *be* `FF` when it is printed an hour later.
 *
 *  **The tag is shallow, deliberately.**  `0xFF + 1` prints `256` in decimal, because
 *  `Sum.eval` reads both operands as numbers and builds a plain `_Number`.  Carrying the
 *  base through arithmetic would require deciding whose base wins in `0xFF + 0b1011`, and
 *  there is no defensible answer — so the base survives *display and storage*, which is what
 *  a base is for, and arithmetic falls back to decimal.
 *
 *  @param value    the integer value, held as a `Double` like every other numeric tier
 *  @param base     the radix, `2 .. 36`
 *  @param balanced whether the digits are the balanced set (base 3 only)
 */
case class _Based private (value: Double, base: Int, balanced: Boolean) extends _Value:

  /** The literal form, which re-parses to this same value — the round-trip invariant.
   *
   *  Bases 2, 8 and 16 and balanced ternary have prefix literals; any other base has none,
   *  so it prints as the `tobase(n, b)` call that produces it.  Both re-parse.
   */
  override def toString: String = display(Environment.DefaultPrecision)

  /** Renders the digits.  `precision` is accepted for uniformity with the other tiers but
   *  is unused: a based integer has no fractional part to round.
   */
  def display(precision: Int): String =
    if balanced then "0t" + _Based.balancedDigits(value.toLong)
    else base match
      case 2  => "0b" + _Based.digitsOf(value.toLong, 2)
      case 8  => "0o" + _Based.digitsOf(value.toLong, 8)
      case 16 => "0x" + _Based.digitsOf(value.toLong, 16)
      case b  => s"tobase(${value.toLong}, $b)"

  override def eval(env: Environment): Either[_Expression, _Value] = Right(this)

  /** A leaf, like every other numeric tier. */
  override def children: List[_Expression] = List.empty
  override def rebuild(c: List[_Expression]): _Expression = this


object _Based:

  /** Radix bounds: 2 is the smallest meaningful base, 36 exhausts `0-9A-Z`. */
  val MinBase: Int = 2
  val MaxBase: Int = 36

  /** Builds a based integer, collapsing to a plain [[_Number]] when there is nothing to
   *  remember.
   *
   *  Base 10, a non-integral value, a non-finite one, or a base outside `2 .. 36` all yield
   *  a `_Number`: only a genuinely based integer becomes a `_Based`, exactly as `_Truth.of`
   *  collapses the crisp endpoints and `_Complex.of` collapses a zero imaginary part.
   */
  def of(value: Double, base: Int, balanced: Boolean = false): _Value =
    if !value.isWhole || value.isNaN || value.isInfinite then _Number(value)
    else if balanced then
      if base == 3 then new _Based(value, 3, true) else _Number(value)
    else if base == 10 || base < MinBase || base > MaxBase then _Number(value)
    else new _Based(value, base, false)

  /** Digit characters for bases up to 36. */
  private val Alphabet = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  /** Unsigned digit string of `n` in `base`, with a leading `-` for a negative value. */
  def digitsOf(n: Long, base: Int): String =
    if n == 0 then "0"
    else
      val sign  = if n < 0 then "-" else ""
      var rest  = math.abs(n)
      val sb    = new StringBuilder
      while rest > 0 do
        sb.append(Alphabet((rest % base).toInt))
        rest /= base
      sign + sb.reverse.toString

  /** Balanced-ternary digits, using `T` for the digit `-1`.
   *
   *  The digit set is `{-1, 0, 1}` — the same alphabet 4.G's symmetric ternary logic uses,
   *  which is why this base belongs in the library rather than being a bare utility.
   */
  def balancedDigits(n: Long): String =
    if n == 0 then "0"
    else
      var rest = n
      val sb   = new StringBuilder
      while rest != 0 do
        // Remainder taken into {-1, 0, 1} rather than {0, 1, 2}; a remainder of 2 becomes
        // -1 with a carry, which is what makes the representation balanced.
        val r = math.floorMod(rest, 3) match
          case 2 => -1
          case x => x.toInt
        sb.append(if r == -1 then 'T' else ('0' + r).toChar)
        rest = (rest - r) / 3
      sb.reverse.toString

  /** Parses a digit string in `base`, or `None` on an invalid digit. */
  def parseDigits(s: String, base: Int): Option[Long] =
    val (negative, body) = if s.startsWith("-") then (true, s.drop(1)) else (false, s)
    if body.isEmpty then None
    else
      body.toUpperCase.foldLeft(Option(0L)) { (acc, c) =>
        acc.flatMap { n =>
          val d = Alphabet.indexOf(c)
          Option.when(d >= 0 && d < base)(n * base + d)
        }
      }.map(n => if negative then -n else n)

  /** Parses a balanced-ternary digit string (`T` = -1), or `None` on an invalid digit. */
  def parseBalanced(s: String): Option[Long] =
    if s.isEmpty then None
    else s.toUpperCase.foldLeft(Option(0L)) { (acc, c) =>
      acc.flatMap { n =>
        val d = c match
          case 'T' => Some(-1L)
          case '0' => Some(0L)
          case '1' => Some(1L)
          case _   => None
        d.map(n * 3 + _)
      }
    }
