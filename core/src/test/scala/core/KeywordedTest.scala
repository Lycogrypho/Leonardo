package it.grypho.scala.leonardo
package core

import org.scalatest.flatspec.AnyFlatSpec


/** Every prompt-facing kind enum spells its word through [[Keyworded]] (F_0148), and the reverse
 *  lookup finds each case again — so no two cases of one kind share a word. */
class KeywordedTest extends AnyFlatSpec:

  private val kinds: List[(String, Iterable[Keyworded])] = List(
    "StatKind"             -> statistics.StatKind.values,
    "PairStatKind"         -> statistics.PairStatKind.values,
    "TestKind"             -> statistics.TestKind.values,
    "Alternative"          -> statistics.Alternative.values,
    "DistKind"             -> probability.DistKind.values,
    "Query"                -> probability.Query.values,
    "Subspace"             -> matrix.Subspace.values,
    "RiccatiKind"          -> control.RiccatiKind.values,
    "DiscretisationMethod" -> control.DiscretisationMethod.values,
    "StructureKind"        -> control.StructureKind.values,
    "OdeMethod"            -> ode.OdeMethod.values,
    "MinimizeMethod"       -> optimize.MinimizeMethod.values,
    "NlpMethod"            -> optimize.NlpMethod.values,
    "DefuzzMethod"         -> logic.DefuzzMethod.values,
    "NormalKind"           -> logic.NormalKind.values,
    "ReduceKind"           -> scalar.ReduceKind.values,
    "SeqKind"              -> scalar.SeqKind.values)

  /** Words two cases of one kind share on purpose: `prob(d, lo, hi)` and the predicate form
   *  `prob(X < 2)` are told apart by their arguments, not their word. */
  private val shared = Set("Query" -> "prob")

  "every kind" should "find each of its cases again by its keyword" in {
    for (name, values) <- kinds; v <- values do
      assert(Keyworded.byKeyword(values, v.keyword).exists(_.keyword == v.keyword), s"$name.$v spelled ${v.keyword}")
  }

  it should "give each case its own word, except the documented sharing" in {
    for (name, values) <- kinds do
      val repeated = values.toVector.groupBy(_.keyword).collect { case (w, cs) if cs.size > 1 => w }
      assert(repeated.forall(w => shared.contains(name -> w)), s"$name repeats ${repeated.mkString(", ")}")
  }

  it should "agree with the published companion tables it delegates to" in {
    assert(probability.DistKind.values.forall(k => k.keyword == probability.DistKind.keyword(k)))
    assert(probability.DistKind.ChiSquared.keyword == "chisq")
    assert(statistics.StatKind.Median.keyword == "median")
    assert(optimize.MinimizeMethod.fromKeyword("pbfgs").contains(optimize.MinimizeMethod.ProjectedBFGS))
  }
