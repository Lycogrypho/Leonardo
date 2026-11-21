package it.grypho.scala.leonardo
package mcp

import org.scalatest.flatspec.AnyFlatSpec

/** F_0367: the JSON and reactive stack the server runs on, as the build resolves it.
 *
 *  `build.sbt` overrides Jackson and Reactor (F_0309, F_0367) because the SDK's own choice carried advisories
 *  (Jackson 3.1.4, then 3.2.1) or a release candidate (`reactor-core` 3.9.0-RC1).  An override is silent when it
 *  goes stale: F_0309's 3.1.8 would have held Jackson below the 3.2 line the SDK 2.2.0 is built against, and
 *  removing an override lets the SDK's own version back in.  This suite reads what is actually on the classpath. */
class RuntimeVersionsTest extends AnyFlatSpec:

  "the server's runtime" should "carry Jackson 3.2.4 or later, the release of the SDK's line the build overrides to" in {
    val v = tools.jackson.databind.cfg.PackageVersion.VERSION
    assert(v.getMajorVersion == 3, s"$v")
    assert(v.getMinorVersion > 2 || (v.getMinorVersion == 2 && v.getPatchLevel >= 4), s"Jackson $v")
  }

  it should "carry a released Reactor, not a milestone or a release candidate" in {
    val version = Option(classOf[reactor.core.publisher.Flux[?]].getPackage).flatMap(p => Option(p.getImplementationVersion))
    version match
      case None    => fail("reactor-core carries no Implementation-Version")
      case Some(v) => assert(v.matches("""\d+\.\d+\.\d+"""), s"reactor-core $v")
  }
