package it.grypho.scala.leonardo
package core


/** The named mathematical constants (F_0289).
 *
 *  `pi` and `e` were folded to numbers by the parser until F_0289; `eulergamma` and `goldenratio` are
 *  new.  `gamma` and `phi` stay free variable names (`phi` is the spherical coordinate, and a released
 *  name cannot be reclaimed without breaking saved scripts), so the two new constants are spelled out,
 *  with the Greek glyphs accepted as aliases — the ASCII name is what is printed and saved.
 *
 *  @param keyword the word the grammar reads and `toString` writes
 *  @param glyph   a Greek alias the grammar also reads, if any
 *  @param value   the constant as a `Double`
 */
enum ConstantKind(val keyword: String, val glyph: Option[String], val value: Double) extends Keyworded:
  /** The circle constant π. */
  case Pi          extends ConstantKind("pi", Some("π"), math.Pi)
  /** Euler's number, the base of the natural logarithm. */
  case E           extends ConstantKind("e", None, math.E)
  /** The Euler–Mascheroni constant γ = lim (Hₙ − ln n). */
  case EulerGamma  extends ConstantKind("eulergamma", Some("γ"), 0.5772156649015329)
  /** The golden ratio φ = (1 + √5)/2. */
  case GoldenRatio extends ConstantKind("goldenratio", Some("φ"), 1.618033988749895)


/** A named constant kept as a symbol (F_0289), so a formula, a definition and `:save` say `pi`
 *  rather than `3.14159`.
 *
 *  **Outside exact mode it evaluates to its `Double`**, so every value computed outside exact mode
 *  is what it was when the parser folded the constant.  **In exact mode it stays symbolic** — `pi/2`
 *  is `pi/2`, and the functions answer exactly where a rule knows the value (`sin(pi/6)` is `1/2`)
 *  — except `goldenratio`, which is algebraic and so evaluates to the exact surd `1/2 + √5/2`.  Where
 *  a number is needed and no rule applies, [[evalNumeric]] reads every constant at the working
 *  precision, as the parser's rational reading did.
 *
 *  An `_Expression`, not a `_Value`: nothing widens it to a `Double` by accident, and an exact
 *  computation can keep it.  It has no free variables, so it is a constant to every pass.
 *
 *  @param kind which constant
 *  @param exact whether it was read in exact mode, which makes it symbolic even where the
 *               environment does not say exact mode (a library caller's exact parse)
 */
final case class _Constant(kind: ConstantKind, exact: Boolean = false) extends _Expression:
  override def children: List[_Expression]                   = Nil
  override def rebuild(newChildren: List[_Expression]): _Expression = this
  override def toString: String                               = kind.keyword

  override def eval(env: Environment): Either[_Expression, _Value] =
    if !(exact || env.exactMode) then Right(_Number(kind.value))
    else if kind == ConstantKind.GoldenRatio then _Constant.goldenRatio.toRight(this)
    else Left(this)

  /** The constant as a number in `env`'s tier: its `Double` outside exact mode, its reading at the
   *  working precision inside it.
   *  @param env the environment
   *  @return the reading */
  def reading(env: Environment): _Value =
    if !(exact || env.exactMode) then _Number(kind.value)
    else
      val r: Option[_Value] = kind match
        case ConstantKind.Pi          => piAt(env.workingPrecision)
        case ConstantKind.E           => eAt(env.workingPrecision)
        case ConstantKind.EulerGamma  => eulerGammaAt(env.workingPrecision)
        case ConstantKind.GoldenRatio => _Constant.goldenRatio
      r.getOrElse(_Number(kind.value))

/** The readers of the named constants. */
object _Constant:
  /** `(1 + √5)/2` as an exact surd. */
  private[core] lazy val goldenRatio: Option[_Value] =
    _Rational.of(BigInt(1), BigInt(2)).flatMap(h => _Surd.of(h, h, BigInt(5)))

  /** The constant spelled `word` (its keyword or its glyph), if any.
   *  @param word the word
   *  @return the kind */
  def named(word: String): Option[ConstantKind] =
    ConstantKind.values.find(k => k.keyword == word || k.glyph.contains(word))


/** Whether a named constant occurs in `e`.
 *  @param e the expression
 *  @return true when `e` holds a [[_Constant]] */
def containsConstant(e: _Expression): Boolean = e match
  case _: _Constant => true
  case other        => other.children.exists(containsConstant)

/** `e` with every named constant replaced by its reading in `env` (F_0289).
 *  @param e   the expression
 *  @param env the environment, which decides the tier and the working precision
 *  @return the expression with no constant left */
def foldConstants(e: _Expression, env: Environment): _Expression = e match
  case c: _Constant => c.reading(env)
  case other if other.children.isEmpty => other
  case other => other.rebuild(other.children.map(foldConstants(_, env)))

/** `e` evaluated where a number is needed (F_0289): as `eval`, and when that leaves a closed
 *  expression holding a named constant, evaluated again with every constant read at the working
 *  precision — what the parser's rational reading of `pi` gave every computation before.
 *  @param e   the expression
 *  @param env the environment
 *  @return the evaluation */
def evalNumeric(e: _Expression, env: Environment): Either[_Expression, _Value] =
  e.eval(env) match
    case Left(r) if r.freeVars.isEmpty && containsConstant(r) =>
      foldConstants(r, env).eval(env)
    case other => other
