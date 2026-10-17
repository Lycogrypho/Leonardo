package it.grypho.scala.leonardo
package tools

import java.nio.file.Paths
import org.scalatest.flatspec.AnyFlatSpec

/** F_0097: the architecture diagram read as a graph, written as GraphML for Gephi, and the
 *  refusals that make the reader a standing check. */
class PumlGraphTest extends AnyFlatSpec:

  private val sample =
    """@startuml
      |!define W 100
      |skinparam shadowing false
      |skinparam class {
      |    BackgroundColor<<case>> #E6F4EA
      |}
      |hide empty members
      |' a comment
      |package "core" {
      |    interface "_Expression" <<trait>> [[expressions.html]] {
      |        + eval(env: Environment): Either[_Expression, _Value]
      |        __ traversal __
      |        + children: List
      |    }
      |    class "_Number" <<value>>
      |    enum "Kind" <<data>> {
      |        A | B
      |    }
      |}
      |package "scalar" <<Rectangle>> {
      |    abstract class "Sum" <<case>> <<node>> [[calculus.html]] {
      |        a & b < c
      |    }
      |}
      |"_Number"     ..|> "_Expression"
      |"Sum"         --|> "_Expression"
      |"Sum"         ..> "_Number" : reads
      |"Sum"         --> "Kind" : holds
      |legend right
      ||= a |= b |
      |endlegend
      |@enduml
      |""".stripMargin

  private def diagram(text: String): PumlGraph.Diagram =
    PumlGraph.read(text).fold(errs => fail(errs.mkString("\n")), identity)

  private def refusals(text: String): Vector[String] =
    PumlGraph.read(text).fold(identity, d => fail(s"read: $d"))

  "the diagram reader" should "read every construct the architecture diagram uses" in {
    val d = diagram(sample)
    assert(d.nodes.map(_.name) == Vector("_Expression", "_Number", "Kind", "Sum"))
    val sum = d.nodes.last
    assert(sum.pkg == "scalar" && sum.kind == "abstract class" && sum.stereotype == "case, node" &&
           sum.url == "calculus.html" && sum.members == Vector("a & b < c"), sum)
    val expr = d.nodes.head
    assert(expr.kind == "interface" && expr.members.size == 3 && expr.members(1) == "__ traversal __", expr)
    assert(d.nodes(1).members.isEmpty && d.nodes(1).url == "", d.nodes(1))
    assert(d.edges.map(e => (e.source, e.relation, e.target, e.label)) == Vector(
      ("_Number", PumlGraph.Relation.Realisation, "_Expression", ""),
      ("Sum", PumlGraph.Relation.Inheritance, "_Expression", ""),
      ("Sum", PumlGraph.Relation.Dependency, "_Number", "reads"),
      ("Sum", PumlGraph.Relation.Association, "Kind", "holds")))
  }

  it should "refuse a duplicate declaration, naming both lines" in {
    val r = refusals("package \"p\" {\nclass \"A\"\nclass \"A\"\n}\n")
    assert(r.size == 1 && r.head.contains("line 3") && r.head.contains("line 2"), r)
  }

  it should "refuse an edge to an undeclared name" in {
    val r = refusals("class \"A\"\n\"A\" ..> \"B\"\n")
    assert(r.size == 1 && r.head.contains("line 2") && r.head.contains("\"B\""), r)
  }

  it should "refuse an arrow, a keyword and a line it does not recognise" in {
    assert(refusals("class \"A\"\nclass \"B\"\n\"A\" <|-- \"B\"\n").exists(_.contains("arrow")))
    assert(refusals("object \"A\"\n").exists(_.contains("line 1")))
    assert(refusals("note right of A\n").exists(_.contains("line 1")))
  }

  it should "refuse an unclosed body, package or block, and an unmatched brace" in {
    assert(refusals("class \"A\" {\n+ x\n").exists(_.contains("not closed")))
    assert(refusals("package \"p\" {\nclass \"A\"\n").exists(_.contains("not closed")))
    assert(refusals("legend\n|x|\n").exists(_.contains("not closed")))
    assert(refusals("class \"A\"\n}\n").exists(_.contains("line 2")))
  }

  "the GraphML writer" should "emit well-formed XML with one node and one edge per declaration and relation" in {
    val xml = PumlGraph.toGraphML(diagram(sample))
    val doc = javax.xml.parsers.DocumentBuilderFactory.newInstance().newDocumentBuilder()
      .parse(new java.io.ByteArrayInputStream(xml.getBytes("UTF-8")))
    assert(doc.getElementsByTagName("node").getLength == 4)
    assert(doc.getElementsByTagName("edge").getLength == 4)
    assert(xml.contains("a &amp; b &lt; c"), "members escaped")
    assert(xml.contains("\"realisation\"") || xml.contains(">realisation<"))
    assert(PumlGraph.toGraphML(diagram(sample)) == xml, "deterministic")
  }

  "the repository's diagram" should "read without a refusal, and match its committed GraphML" in {
    val d = diagram(Files.read(Paths.get("docs", "structure.puml")))
    assert(d.nodes.size >= 200 && d.edges.size >= 300, (d.nodes.size, d.edges.size))
    assert(PumlGraph.offences(PumlGraph.DefaultSource, PumlGraph.DefaultTarget).isEmpty,
           "docs/structure.graphml is stale: run `sbt graphml`")
  }

  it should "report a committed GraphML that drifted from the diagram" in {
    val dir    = java.nio.file.Files.createTempDirectory("leonardo-graphml-")
    val source = dir.resolve("s.puml")
    val target = dir.resolve("s.graphml")
    Files.write(source, sample)
    Files.write(target, PumlGraph.toGraphML(diagram(sample)))
    assert(PumlGraph.offences(source, target).isEmpty)
    Files.write(source, sample.replace("\"Sum\"         --> \"Kind\" : holds\n", ""))
    assert(PumlGraph.offences(source, target).exists(_.contains("stale")))
  }
