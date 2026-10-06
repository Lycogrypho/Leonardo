---
title: "Lessons: in your program"
nav_order: 17
has_children: true
---

<img src="../logo_bw.svg" alt="" style="height:80px;width:auto;float:right;margin:0 0 8px 16px"/>

# Lessons: in your program
<div style="clear:both"></div>

Ten short lessons on using Leonardo as a **library** — a dependency in your own Scala code,
called from your own types. Each one builds something small and ends with an exercise, and the
last one assembles the rest into a working program.

**[Start with L1 — Reading an expression](l1.md).**

If you would rather type at a prompt than write Scala, the other branch is
[Lessons: at the prompt](repl.md) — it needs no installation at all, and it is a quick way to get
a feel for the expression language before you embed it.

## The lessons

| | | |
|---|---|---|
| **[L1](l1.md)** | Reading an expression | `Parser.parse`, its result type, and the failure that is not one |
| **[L2](l2.md)** | The dual evaluation model | `Either[_Expression, _Value]`, and why `Left` is not an error |
| **[L3](l3.md)** | Environment | Binding, scoped evaluation, and why rounding is display-only |
| **[L4](l4.md)** | Building trees without the parser | The `_`-prefixed nodes, and `Syntax` sugar |
| **[L5](l5.md)** | Traversal | `children` / `rebuild`, `freeVars`, `dependsOn` |
| **[L6](l6.md)** | The calculus API | `derive`, `integrate`, `simplify`, `normalize` |
| **[L7](l7.md)** | Numbers | The widening extractor and the exact tier |
| **[L8](l8.md)** | Embedding a `Session` | String in, string out — the bridge to the REPL |
| **[L9](l9.md)** | Refusals | `Left`, `None`, and what "stays symbolic" obliges you to handle |
| **[L10](l10.md)** | A small application, end to end | The payoff |

*All ten are written. The [reference pages](../expressions.md) cover the domains this branch
does not open — matrices, equations, logic, transforms, differential equations, statistics,
vector calculus and control theory — in the same language these lessons teach.*

## What this branch assumes

That you can add a dependency to a Scala 3 build and read a `match`. It does **not** assume you
know what a computer algebra system is, and it does not teach the mathematics — it teaches how to
express a problem to the library and how to read what comes back.

## The arc

The first three lessons are the model: parsing, the dual evaluation result, and the environment
that binds names. The middle four are the working surface: building trees, traversing them, the
calculus functions, and the number tiers. The last three are what separates a demonstration from
a program: embedding a session, reading the library's refusals, and one small application end to
end.

Every example on these pages is **compiled and executed when the site is built**, so the code
cannot drift from the library and the results are the library's own.

## Before you start

[Getting Started](../getting-started.md) has the artifact coordinates, the two-artifact split and
the import conventions. These lessons assume it only for the dependency line, which L1 repeats.
