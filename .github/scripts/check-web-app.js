// Proves the LINKED bundle starts. That is all it does, and it is the one thing a Scala test
// cannot do (issue F_0027, Decision C).
//
// Everything else this script used to check -- every element id App.scala reaches for, the key
// handler, a line round-tripping through Session.step, the plot documents, the latex toggle,
// the settings panel -- is now `web/src/test/scala/web/AppTest.scala`, driven against a fake
// DOM written in Scala.js. Those are properties of the CLASSES, and a suite is a better home
// for them than a hand-rolled `check()` function.
//
// What a suite cannot reach is the artifact a browser actually downloads: `sbt web/test` links
// a *test* bundle and runs it, which says nothing about whether `leonardo-web-opt/main.js`
// loads and exposes its entry point. That matters here more than it usually would, because a
// NoModule export is a lexical `let` rather than a property of globalThis -- so
// `globalThis.leonardoStart` is `undefined` while the bare identifier works, `require()` cannot
// load the file at all, and a page whose feature detection got that wrong would report the
// bundle as missing. Hence `vm.runInThisContext`, which is what a <script> tag does.
//
// The remaining JavaScript in the repository is exactly this file, and the rule is that it
// stays that way: a program here is Scala unless it strictly cannot be.
const fs = require('fs');
const vm = require('vm');

// The Scala version is NOT spelled here (F_0023): the build output directory is named after
// it, so a literal path puts the version in a file Scala Steward's bump branch would have to
// edit -- harmless in a script, refused outright in a workflow, and one rule is cheaper to
// keep than a per-file exception. CheckScalaVersion enforces it.
//
// Several scala-* directories means a stale target beside a fresh one, and silently driving
// the wrong bundle is worse than saying so -- `sbt clean` is the remedy the message names.
function bundlePath() {
  const root = 'web/target';
  const found = fs.readdirSync(root)
    .filter(d => d.startsWith('scala-'))
    .map(d => `${root}/${d}/leonardo-web-opt/main.js`)
    .filter(p => fs.existsSync(p));
  if (found.length === 1) return found[0];
  if (found.length === 0) throw new Error('no linked bundle under ' + root + '; run `sbt app`');
  throw new Error('several linked bundles, so the current one would be a guess; run `sbt clean`:\n  ' + found.join('\n  '));
}

const path = bundlePath();
vm.runInThisContext(fs.readFileSync(path, 'utf8'));

// A bare identifier, never globalThis.leonardoStart -- see the note above. `typeof` is the one
// operator that does not throw on an undeclared name, so this reports rather than crashes.
const kind = vm.runInThisContext('typeof leonardoStart');
if (kind !== 'function') {
  console.log(`FAIL ${path}\n  leonardoStart is ${kind}, not a function`);
  process.exit(1);
}

const bytes = fs.statSync(path).size;
console.log(`ok   linked bundle starts   -> ${path} (${bytes} bytes), leonardoStart is a function`);
