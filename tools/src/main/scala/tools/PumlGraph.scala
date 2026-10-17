package it.grypho.scala.leonardo
package tools

import java.nio.file.{Files as JFiles, Path, Paths}

/** Reads the architecture diagram, `docs/structure.puml`, as a graph and writes it as GraphML
 *  for Gephi (F_0097).
 *
 *  PlantUML has no GraphML exporter, and converting its SVG would be fragile, so the source is
 *  read directly. **The reader knows exactly the subset the diagram uses**, a line-based one:
 *  `@startuml`/`@enduml`, `!define`, `skinparam` (one line or a block), `hide`, comments,
 *  `legend … endlegend`, nested `package "p" [<<s>>] {`, declarations
 *  `(abstract class|class|interface|enum) "Name" <<s>>… [[url]] [{ members }]`, and relations
 *  `"A" arrow "B" [: label]` with the four arrows the diagram draws.
 *
 *  **Anything else is refused, naming the line, never dropped**: an unknown arrow or keyword, a
 *  duplicate declaration, an edge to an undeclared name, an unclosed or unmatched block. That
 *  is what makes the reader a standing check (Decision B): PlantUML accepts all of those
 *  silently, which is how D_0096's four defects decayed unseen.
 *
 *  Packages become a node **attribute**, not group nodes: Gephi flattens nested graphs, and an
 *  attribute is what its partition colouring reads. The output carries no colour: colour by
 *  partition on `package` or `stereotype` in Gephi.
 */
object PumlGraph:

  /** The diagram, and its committed conversion (Decision A: committed beside it). */
  val DefaultSource: Path = Paths.get("docs", "structure.puml")
  val DefaultTarget: Path = Paths.get("docs", "structure.graphml")

  /** What a relation means, by its arrow. */
  enum Relation(val arrow: String, val word: String):
    case Dependency  extends Relation("..>", "dependency")
    case Inheritance extends Relation("--|>", "inheritance")
    case Realisation extends Relation("..|>", "realisation")
    case Association extends Relation("-->", "association")

  /** One declaration.
   *  @param name       the quoted name, unique in the diagram
   *  @param pkg        the enclosing packages, outermost first, joined by `.`; empty at top level
   *  @param kind       `class`, `abstract class`, `interface` or `enum`
   *  @param stereotype the stereotypes, joined by `, `
   *  @param url        the `[[…]]` link, or empty
   *  @param members    the body's lines, trimmed, separators included
   *  @param line       where it is declared, from 1
   */
  final case class Node(name: String, pkg: String, kind: String, stereotype: String, url: String,
                        members: Vector[String], line: Int)

  /** One relation, `source arrow target`.
   *  @param line where it is written, from 1 */
  final case class Edge(source: String, target: String, relation: Relation, label: String, line: Int)

  /** The diagram as a graph, in source order. */
  final case class Diagram(nodes: Vector[Node], edges: Vector[Edge])

  private val Declaration =
    """^(abstract class|class|interface|enum)\s+"([^"]+)"((?:\s+<<[^>]+>>)*)(?:\s+\[\[([^\]]+)\]\])?\s*(\{)?$""".r
  private val Stereotype = """<<([^>]+)>>""".r
  private val Package    = """^package\s+"([^"]+)"(?:\s+<<[^>]+>>)?\s*\{$""".r
  private val Link       = """^"([^"]+)"\s+(\S+)\s+"([^"]+)"(?:\s*:\s*(.*))?$""".r
  private val Directive  = """^(@startuml|@enduml|!define\s.*|hide\s.*|skinparam\s+[^{]*)$""".r
  private val BlockOpen  = """^skinparam\s+\w+\s*\{$""".r

  /** Reads `text` as the diagram subset, or answers every refusal, each naming its line.
   *
   *  @param text the `.puml` source
   *  @return the diagram, or the refusals
   */
  def read(text: String): Either[Vector[String], Diagram] =
    val lines  = text.stripPrefix("﻿").linesIterator.toVector
    val errors = Vector.newBuilder[String]
    val nodes  = Vector.newBuilder[Node]
    val edges  = Vector.newBuilder[Edge]
    var packages = List.empty[(String, Int)]            // innermost first, with the opening line
    var body     = Option.empty[(Node, Vector[String])] // a declaration whose `{` is open
    var block    = Option.empty[(String, Int)]          // the closing word and the opening line

    for (raw, i) <- lines.zipWithIndex do
      val n = i + 1
      val t = raw.trim
      (body, block) match
        case (Some((node, members)), _) =>
          if t == "}" then { nodes += node.copy(members = members); body = None }
          else if t.nonEmpty then body = Some((node, members :+ t))
        case (None, Some((close, _))) =>
          if t == close then block = None
        case _ =>
          t match
            case "" =>
            case c if c.startsWith("'") =>
            case BlockOpen()    => block = Some(("}", n))
            case l if l == "legend" || l.startsWith("legend ") => block = Some(("endlegend", n))
            case Directive(_)   =>
            case Package(name)  => packages = (name, n) :: packages
            case "}"            =>
              if packages.isEmpty then errors += s"line $n: a closing brace with no open package"
              else packages = packages.tail
            case Declaration(kind, name, stereo, url, open) =>
              val node = Node(name, packages.reverse.map(_._1).mkString("."), kind,
                              Stereotype.findAllMatchIn(Option(stereo).getOrElse("")).map(_.group(1).trim).mkString(", "),
                              Option(url).getOrElse(""), Vector.empty, n)
              if Option(open).isDefined then body = Some((node, Vector.empty)) else nodes += node
            case Link(source, arrow, target, label) =>
              Relation.values.find(_.arrow == arrow) match
                case Some(r) => edges += Edge(source, target, r, Option(label).getOrElse("").trim, n)
                case None    => errors += s"line $n: unknown arrow '$arrow' in: $t"
            case other => errors += s"line $n: not part of the diagram subset this reader knows: $other"

    body.foreach((node, _) => errors += s"line ${node.line}: the body of \"${node.name}\" is not closed")
    block.foreach((close, at) => errors += s"line $at: a block is not closed (expected '$close')")
    packages.foreach((name, at) => errors += s"line $at: package \"$name\" is not closed")

    val ns = nodes.result()
    val es = edges.result()
    val first = ns.groupBy(_.name).view.mapValues(_.minBy(_.line)).toMap
    ns.foreach { node =>
      first.get(node.name).filter(_.line != node.line).foreach { f =>
        errors += s"line ${node.line}: \"${node.name}\" is already declared at line ${f.line}"
      }
    }
    es.foreach { e =>
      List(e.source, e.target).filterNot(first.contains).foreach { name =>
        errors += s"line ${e.line}: the relation names \"$name\", which is not declared"
      }
    }
    val all = errors.result()
    if all.isEmpty then Right(Diagram(ns, es)) else Left(all)

  /** Escapes text for an XML element or attribute. */
  private def escape(s: String): String =
    s.flatMap {
      case '&' => "&amp;"
      case '<' => "&lt;"
      case '>' => "&gt;"
      case '"' => "&quot;"
      case c   => c.toString
    }

  private val NodeKeys = Vector("label", "package", "kind", "stereotype", "url", "members")
  private val EdgeKeys = Vector("relation", "label")

  /** The diagram as GraphML: one node per declaration and one directed edge per relation, in
   *  source order, with typed string attributes. Deterministic, so a committed copy can be
   *  compared against a fresh conversion. Empty attributes are omitted.
   *
   *  @param d the diagram
   *  @return the GraphML document, `\n`-terminated lines
   */
  def toGraphML(d: Diagram): String =
    val ids = d.nodes.zipWithIndex.map((node, i) => node.name -> s"n$i").toMap
    def data(key: String, value: String): Option[String] =
      Option.when(value.nonEmpty)(s"""      <data key="$key">${escape(value)}</data>""")
    val keys =
      NodeKeys.map(k => s"""  <key id="n_$k" for="node" attr.name="$k" attr.type="string"/>""") ++
      EdgeKeys.map(k => s"""  <key id="e_$k" for="edge" attr.name="$k" attr.type="string"/>""")
    val nodes = d.nodes.flatMap { node =>
      val values = Vector(node.name, node.pkg, node.kind, node.stereotype, node.url, node.members.mkString("\n"))
      Vector(s"""    <node id="${ids(node.name)}">""") ++
        NodeKeys.zip(values).flatMap((k, v) => data(s"n_$k", v).map("  " + _)) :+ "    </node>"
    }
    val edges = d.edges.zipWithIndex.flatMap { (e, i) =>
      Vector(s"""    <edge id="e$i" source="${ids(e.source)}" target="${ids(e.target)}">""") ++
        EdgeKeys.zip(Vector(e.relation.word, e.label)).flatMap((k, v) => data(s"e_$k", v).map("  " + _)) :+
        "    </edge>"
    }
    (Vector(
      """<?xml version="1.0" encoding="UTF-8"?>""",
      "<!-- Generated from docs/structure.puml by `sbt graphml` (F_0097). Do not edit:",
      "     `sbt checks` compares this file against a fresh conversion. -->",
      """<graphml xmlns="http://graphml.graphdrawing.org/xmlns">""") ++ keys ++
      Vector("""  <graph id="leonardo" edgedefault="directed">""") ++ nodes ++ edges ++
      Vector("  </graph>", "</graphml>")).mkString("", "\n", "\n")

  /** Every refusal of `source`, and whether `target` still matches its conversion.
   *
   *  @param source the `.puml` diagram
   *  @param target the committed GraphML
   *  @return one finding per problem, empty when the diagram reads and the copy is current
   */
  def offences(source: Path, target: Path): Vector[String] =
    read(Files.read(source)) match
      case Left(errors) => errors.map(e => s"$source: $e")
      case Right(d) =>
        val fresh = toGraphML(d)
        if !JFiles.exists(target) then Vector(s"$target: missing; run `sbt graphml`")
        else if Files.read(target).replace("\r\n", "\n") != fresh then
          Vector(s"$target: stale, it no longer matches $source; run `sbt graphml`")
        else Vector.empty

  /** `--check`: report the refusals and a stale copy. Otherwise write the GraphML.
   *
   *  @param args `--check` and/or the source and target paths
   *  @return 1 on any refusal or a stale copy, 0 otherwise
   */
  def run(args: Seq[String]): Int =
    val check = args.contains("--check")
    val paths = args.filterNot(_ == "--check").map(Paths.get(_))
    val (source, target) = (paths.headOption.getOrElse(DefaultSource), paths.lift(1).getOrElse(DefaultTarget))
    if check then
      Files.report(offences(source, target), n => s"\n$source: $n diagram problem(s)",
        "The diagram reader refuses what it does not know (F_0097); after editing the diagram, run `sbt graphml`.")
    else
      read(Files.read(source)) match
        case Left(errors) =>
          Files.report(errors.map(e => s"$source: $e"), n => s"\n$source: $n diagram problem(s), nothing written")
        case Right(d) =>
          Files.write(target, toGraphML(d))
          println(s"$target: ${d.nodes.size} nodes, ${d.edges.size} edges")
          0
