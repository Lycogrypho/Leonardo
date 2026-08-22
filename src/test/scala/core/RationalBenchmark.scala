package it.grypho.scala.leonardo
package core

/** The gcd-policy benchmark of issue 4.M — run it with `sbt bench`.
 *
 *  A plain `main` rather than a ScalaTest suite on purpose: it takes minutes, so it must
 *  not run as part of `sbt test`.  The correctness half of the spike — that all three
 *  policies agree on every workload — lives in `RationalTest` where it does run, every time.
 *
 *  **The hypothesis it exists to test** is not "which policy is faster".  It is:
 *
 *  > Re-approximation to the working threshold already caps operand size, so
 *  > [[GcdPolicy.Eager]]'s advantage shrinks as the re-approximation runs more often.
 *
 *  That is why every workload is swept across working precisions rather than measured at
 *  one: a two-point comparison at a single precision would very likely mislead.  The
 *  `exact` column (no re-approximation at all) is the control.
 *
 *  Operand bit-length is reported next to wall-clock because it is the variable that
 *  *explains* the wall-clock and predicts behaviour at precisions that were not measured.
 */
object RationalBenchmark:

  /** Working precisions swept, in decimal digits; `None` is the no-re-approximation control. */
  private val Precisions: List[Option[Int]] = List(Some(10), Some(30), Some(100), None)

  /** The precision the verdict is read at — the candidate default working precision for
   *  issue 4.L, which wants to be well above the display precision of 5.
   */
  private val VerdictPrecision: Option[Int] = Some(30)

  private val Policies: List[GcdPolicy] =
    List(GcdPolicy.Eager, GcdPolicy.Lazy, GcdPolicy.Threshold(_Rational.DefaultThresholdBits))

  /** Above this, a cell is measured once instead of repeatedly — reported, never hidden. */
  private val SingleShotThresholdMs: Long = 2000L

  private def label(p: GcdPolicy): String = p match
    case GcdPolicy.Eager            => "Eager"
    case GcdPolicy.Lazy             => "Lazy"
    case GcdPolicy.Threshold(bits)  => s"Thr($bits)"

  private def label(d: Option[Int]): String = d.fold("exact")(n => s"${n}d")

  /** One measured cell of the sweep. */
  private case class Cell(workload: String, policy: GcdPolicy, digits: Option[Int],
                          ms: Double, maxBits: Int, meanBits: Double,
                          allocMB: Double, singleShot: Boolean, value: Any)

  private val threadBean: Option[com.sun.management.ThreadMXBean] =
    java.lang.management.ManagementFactory.getThreadMXBean match
      case b: com.sun.management.ThreadMXBean if b.isThreadAllocatedMemorySupported => Some(b)
      case _                                                                        => None

  private def allocatedBytes: Long =
    threadBean.fold(0L)(_.getCurrentThreadAllocatedBytes)

  /** Runs `body` warmed up, returning the best of several repetitions plus the statistics
   *  of the last one.  The minimum rather than the mean: it is the measurement least
   *  polluted by GC pauses and scheduler noise, which is what we want when comparing
   *  algorithms rather than predicting throughput.
   */
  private def measure(workload: String, policy: GcdPolicy, digits: Option[Int])
                     (body: BitStats => Any): Cell =
    // One untimed pass to let the JIT settle and to size the repetition count.
    val probeStart = System.nanoTime()
    val probeStats = new BitStats
    val value      = body(probeStats)
    val probeMs    = (System.nanoTime() - probeStart) / 1e6

    val singleShot = probeMs.toLong > SingleShotThresholdMs
    val reps       = if singleShot then 1 else math.max(3, math.min(50, (300.0 / math.max(probeMs, 0.05)).toInt))

    var best   = Double.MaxValue
    var stats  = probeStats
    var allocs = 0.0
    for _ <- 1 to reps do
      val s   = new BitStats
      val a0  = allocatedBytes
      val t0  = System.nanoTime()
      body(s)
      val ms  = (System.nanoTime() - t0) / 1e6
      val mb  = (allocatedBytes - a0).toDouble / (1024.0 * 1024.0)
      if ms < best then { best = ms; stats = s; allocs = mb }

    Cell(workload, policy, digits, best, stats.max, stats.mean, allocs, singleShot, value)

  private def run(): Unit =
    val chainSize   = 3000
    val hilbertSize = 8
    val seriesTerms = 150
    val x           = _Rational.make(BigInt(1), BigInt(3), GcdPolicy.Eager)

    val workloads: List[(String, (GcdPolicy, Option[Int], BitStats) => Any)] = List(
      s"chain(n=$chainSize)"      -> ((p, d, s) => RationalWorkloads.primitiveChain(chainSize, p, d, s)),
      s"hilbert(n=$hilbertSize)"  -> ((p, d, s) => RationalWorkloads.hilbertSolve(hilbertSize, p, d, s)),
      s"expSeries(n=$seriesTerms)"-> ((p, d, s) => RationalWorkloads.expSeries(seriesTerms, x, p, d, s))
    )

    // The report is meant to be pasted into docs/ and compared across machines, so it must
    // not print decimal commas on an Italian JVM and decimal points on an American one.
    java.util.Locale.setDefault(java.util.Locale.ROOT)

    println(s"JVM ${System.getProperty("java.version")} / ${System.getProperty("java.vm.name")}")
    println(s"OS  ${System.getProperty("os.name")} ${System.getProperty("os.arch")}")
    println(s"CPU ${Runtime.getRuntime.availableProcessors} cores")
    println()

    val cells =
      for
        (name, body)  <- workloads
        digits        <- Precisions
        policy        <- Policies
      yield measure(name, policy, digits)((s: BitStats) => body(policy, digits, s))

    // --- correctness: representation may differ, the value may not ---
    var disagreements = 0
    for
      (name, _) <- workloads
      digits    <- Precisions
    do
      val group = cells.filter(c => c.workload == name && c.digits == digits)
      val vals  = group.map(_.value).distinct
      if vals.size != 1 then
        disagreements += 1
        println(s"!! POLICIES DISAGREE on $name @ ${label(digits)}: ${group.map(c => label(c.policy)).mkString(", ")}")

    // --- the sweep ---
    for (name, _) <- workloads do
      println(s"== $name ".padTo(72, '='))
      println(f"${"precision"}%-10s${"policy"}%-11s${"ms"}%10s${"maxBits"}%10s${"meanBits"}%10s${"allocMB"}%10s")
      for digits <- Precisions do
        for policy <- Policies do
          cells.find(c => c.workload == name && c.digits == digits && c.policy == policy) match
            case None    => println(f"${label(digits)}%-10s${label(policy)}%-11s${"-- not measured --"}%10s")
            case Some(c) =>
              val flag = if c.singleShot then "  (single shot: over budget)" else ""
              println(f"${label(digits)}%-10s${label(policy)}%-11s${c.ms}%10.2f${c.maxBits}%10d${c.meanBits}%10.1f${c.allocMB}%10.2f$flag")
      println()

    // --- the exit criteria, applied as written before the numbers existed ---
    println("== verdict ".padTo(72, '='))
    println(s"Read at the candidate default working precision: ${label(VerdictPrecision)}.")
    println()

    def cellsAt(policy: GcdPolicy, digits: Option[Int]) =
      cells.filter(c => c.policy == policy && c.digits == digits)

    val lazyAtDefault  = cellsAt(GcdPolicy.Lazy, VerdictPrecision)
    val eagerAtDefault = cellsAt(GcdPolicy.Eager, VerdictPrecision)

    val speedups = lazyAtDefault.flatMap { l =>
      eagerAtDefault.find(_.workload == l.workload).map(e => (l.workload, e.ms / math.max(l.ms, 1e-9)))
    }
    speedups.foreach((w, s) => println(f"  Lazy speedup vs Eager on $w%-22s: $s%5.2fx"))

    // Worst operand-size ratio across EVERY swept precision, per the second criterion.
    val sizeRatios = for
      l <- cells.filter(_.policy == GcdPolicy.Lazy)
      e <- cells.find(c => c.policy == GcdPolicy.Eager && c.workload == l.workload && c.digits == l.digits)
    yield (s"${l.workload} @ ${label(l.digits)}", l.maxBits.toDouble / math.max(e.maxBits.toDouble, 1.0))

    val worstRatio = sizeRatios.maxByOption(_._2)
    worstRatio.foreach((where, r) => println(f"%n  Worst Lazy/Eager operand size: $r%5.2fx  (at $where)"))

    val minSpeedup = speedups.map(_._2).minOption.getOrElse(0.0)
    val maxRatio   = worstRatio.map(_._2).getOrElse(Double.MaxValue)

    val verdict =
      if maxRatio > 4.0 then
        f"REJECT Lazy - its operand size reaches $maxRatio%.2fx Eager's (criterion: > 4x rejects " +
          "regardless of timing).  Choose Threshold, and record the bit bound."
      else if minSpeedup > 1.25 && maxRatio <= 2.0 then
        f"CHOOSE Lazy - at least $minSpeedup%.2fx faster on every workload (criterion: > 25%%) and " +
          f"operand size within $maxRatio%.2fx (criterion: <= 2x)."
      else
        f"CHOOSE Threshold - Lazy is neither clearly faster (min $minSpeedup%.2fx, needs > 1.25x) " +
          f"nor clearly safe enough to prefer outright (size $maxRatio%.2fx)."

    println()
    println(s"  $verdict")
    println()
    println(s"  Cross-policy value agreement: ${if disagreements == 0 then "OK on every cell" else s"$disagreements DISAGREEMENTS"}")
    println()

    // --- which bound? the exit criteria require recording it, not just naming Threshold ---
    println("== threshold bound sweep ".padTo(72, '='))
    println("Swept on BOTH arms.  On `exact` the bound is the only thing bounding growth, so")
    println("it is where a bad bound is dangerous.  At a finite precision it decides something")
    println("different: a bound BELOW the operand size implied by that precision fires on every")
    println("operation, which is Eager wearing a different name -- so the two arms pull opposite")
    println("ways and the default has to clear the higher of them.")
    println()
    println(f"${"workload"}%-22s${"prec"}%7s${"bound"}%8s${"ms"}%9s${"maxBits"}%9s${"vs Eager"}%10s")
    for
      (name, body) <- workloads
      digits       <- List(None, Some(30))
    do
      val eagerMs = cells.find(c => c.workload == name && c.digits == digits && c.policy == GcdPolicy.Eager)
      for bits <- List(16, 32, 64, 128, 256, 1024) do
        val policy = GcdPolicy.Threshold(bits)
        val c = measure(name, policy, digits)((s: BitStats) => body(policy, digits, s))
        val rel = eagerMs.fold("--")(e => f"${e.ms / math.max(c.ms, 1e-9)}%.2fx")
        println(f"$name%-22s${label(digits)}%7s$bits%8d${c.ms}%9.2f${c.maxBits}%9d$rel%10s")
      println()

  /** Entry point.
   *  @param args ignored; the sweep is fixed so runs stay comparable across machines
   */
  def main(args: Array[String]): Unit = run()
