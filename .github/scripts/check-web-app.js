// Drives the browser REPL's entry point against a minimal fake DOM.
//
// There is no browser here, so this cannot prove the page LOOKS right -- but it proves the
// part that actually breaks: that every element id App.scala reaches for exists in
// index.html, that the key handler runs, that a line round-trips through Session.step, and
// that `plot` produces a document Plotly could read. A typo in an id is invisible until the
// page is opened; this makes it visible from a terminal.
const fs = require('fs');
const vm = require('vm');

// --- the fake DOM -------------------------------------------------------------------------
// These ids MUST be exactly the ones index.html defines; that agreement is what is under test.
const ids = ['input', 'transcript', 'figure', 'share', 'clear'];
const elements = {};

function makeElement(id) {
  return {
    id, value: '', className: '', textContent: '', innerHTML: '',
    scrollTop: 0, scrollHeight: 0, children: [], listeners: {},
    addEventListener(type, fn) { (this.listeners[type] = this.listeners[type] || []).push(fn); },
    appendChild(c) { this.children.push(c); },
    focus() {}
  };
}
for (const id of ids) elements[id] = makeElement(id);

globalThis.document = {
  getElementById(id) {
    if (!elements[id]) throw new Error('App reached for an element index.html does not define: #' + id);
    return elements[id];
  },
  createElement() { return makeElement('div'); }
};
globalThis.location = { href: 'https://example.invalid/app/', hash: '' };
globalThis.navigator = {};

// Plotly is stubbed rather than loaded: the real one needs layout and measurement a fake DOM
// cannot provide, and what matters here is the document handed to it.
let drawn = null;
globalThis.Plotly = { react(_el, data, layout) { drawn = { data, layout }; }, purge() {} };

// --- run ------------------------------------------------------------------------------------
vm.runInThisContext(fs.readFileSync('web/target/scala-3.3.6/leonardo-web-opt/main.js', 'utf8'));
vm.runInThisContext('leonardoStart()');

const input = elements.input;
const transcript = elements.transcript;

function type(line) {
  input.value = line;
  for (const fn of input.listeners.keydown) fn({ key: 'Enter', preventDefault() {} });
}
function press(key) {
  for (const fn of input.listeners.keydown) fn({ key, preventDefault() {} });
}
function lastOutput() {
  const out = transcript.children.filter(c => c.className === 'out');
  return out.length ? out[out.length - 1].textContent : '(nothing)';
}

let failures = 0;
function check(label, actual, expected) {
  const ok = expected instanceof RegExp ? expected.test(actual) : actual === expected;
  if (!ok) { failures++; console.log('FAIL ' + label + '\n  expected ' + expected + '\n  actual   ' + actual); }
  else console.log('ok   ' + label.padEnd(34) + ' -> ' + String(actual).split('\n')[0]);
}

// Evaluation, through the same Session.step the terminal REPL uses.
type('2+3*4');                 check('arithmetic', lastOutput(), '14.0');
type('derive(sin(x)^2, x)');   check('derivative', lastOutput(), /cos/);
type('solve(x^2 - 4 > 0, x)'); check('inequality', lastOutput(), /or/);

// State persists across lines: one Session per page, not one per line.
type('a := 7');
type('a * 6');                 check('binding persists', lastOutput(), '42.0');

// Browser-only persistence, reusing :save / :load unchanged.
type(':save work');            check('save', lastOutput(), /saved to work|storage is not available/);

// History: Up recalls the previous line, Down past the newest clears the box.
press('ArrowUp');              check('history up', input.value, ':save work');
press('ArrowUp');              check('history up twice', input.value, 'a * 6');
press('ArrowDown'); press('ArrowDown');
check('history down to empty', input.value, '');

// Plotting: a line, then coordinates with the aspect lock.
type('plot x^2 x 0 2 5');      check('plot', lastOutput(), /plotted 5 points/);
check('plot trace mode', drawn.data[0].mode, 'lines');
check('plot y values', JSON.stringify(drawn.data[0].y), '[0,0.25,1,2.25,4]');
check('plot titled by expr', drawn.layout.yaxis.title, 'x^2');
check('function plot unlocked', drawn.layout.yaxis.scaleanchor, undefined);

type('points x x -1 1 3');     check('points', lastOutput(), /plotted 3 points/);
check('coordinates locked', drawn.layout.yaxis.scaleanchor, 'x');

// Failures must read as messages, not as a dead page.
type('plot');                  check('plot with no args', lastOutput(), /usage: plot/);
type('plot x x 2 1');          check('plot with lo >= hi', lastOutput(), /plot:/);
type('sin(');                  check('parse error', lastOutput(), /parse error/);
type('a * 6');                 check('still alive after errors', lastOutput(), '42.0');

console.log(failures === 0 ? '\nall checks passed' : '\n' + failures + ' CHECK(S) FAILED');
process.exit(failures === 0 ? 0 : 1);
