---
title: "Lessons: at the prompt"
nav_order: 16
has_children: true
---

<img src="../logo_bw.svg" alt="" style="height:80px;width:auto;float:right;margin:0 0 8px 16px"/>

# Lessons: at the prompt
<div style="clear:both"></div>

Ten short lessons on using Leonardo as a **calculator you talk to** — no Scala, no build, no
installation. Everything here runs in the
[browser REPL](https://lycogrypho.github.io/Leonardo/app/): open it in another tab and type
along.

**[Start with R1 — Five minutes](r1.md)**, or pick a lesson below.

If you would rather call Leonardo from your own Scala code, the other branch is
[Lessons: in your program](library.md).

## The lessons

| | | |
|---|---|---|
| **[R1](r1.md)** | Five minutes | What it is for, before any vocabulary |
| **[R2](r2.md)** | Expressions and free variables | Why an unknown name is an answer, and two notation traps |
| **[R3](r3.md)** | Naming things | `:=` versus `=`, definitions that track their dependencies |
| **[R4](r4.md)** | Calculus | `derive`, `integral`, `limit`, `taylor` — and what a refusal looks like |
| **[R5](r5.md)** | Equations | `solve`, inequalities, systems — and the answer it stores for you |
| **[R6](r6.md)** | Matrices | Literals, `det`/`inv`, decompositions, stacked display |
| **[R7](r7.md)** | Drawing and sampling | `plot` in the browser, `samples` everywhere, and dropped points |
| **[R8](r8.md)** | Exact arithmetic | Fractions instead of decimals; the two precisions |
| **[R9](r9.md)** | Your session | Saving, loading, sharing, and the settings worth knowing |
| **[R10](r10.md)** | The terminal REPL | The same language offline, and the three differences |

## What this branch assumes

Ordinary mathematics — that you know what a derivative is. It does **not** assume you have used
a computer algebra system before, and it teaches the *command language*, not the library.

Each lesson ends with an exercise, and every transcript on these pages is **executed when the
site is built**, so what you read is what the library actually answers.

## A note on the two REPLs

They are the same program. `Session` — the whole command language — is shared, so a command that
works in the browser works in the terminal, with two exceptions called out where they arise: the
terminal cannot draw, and the browser has no file system. [R10](r10.md) covers the differences.
