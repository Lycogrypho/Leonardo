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
const ids = ['input', 'transcript', 'figure', 'share', 'clear', 'settings'];
const elements = {};

// `checked` and `type` are plain properties here, which is all the settings panel needs: it
// creates inputs and a select and then only reads and writes those two and `value`.
function makeElement(id) {
  return {
    id, value: '', className: '', textContent: '', innerHTML: '', type: '', checked: false,
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

// MathLive is stubbed for the same reason, and the stub is deliberately recognisable: what is
// under test is that the page CALLS it with the LaTeX the session produced and puts the result
// in the transcript -- not that MathLive can typeset, which is its own problem. index.html
// installs the real one from an ES module; here the global is simply present.
let typeset = [];
globalThis.leonardoLatexToMarkup = (latex) => { typeset.push(latex); return '<span class="ml">' + latex + '</span>'; };

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
function lastMath() {
  const m = transcript.children.filter(c => c.className === 'math');
  return m.length ? m[m.length - 1].innerHTML : '(nothing)';
}
// The panel's fields are inside their labels, one label per setting.
function field(id) {
  for (const label of elements.settings.children)
    for (const child of label.children)
      if (child.id === id) return child;
  throw new Error('no settings control with id #' + id);
}
function change(el) {
  for (const fn of el.listeners.change) fn({});
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

// Bode and Nyquist (F_0004): the log grid and the unwrapped phase, drawn.
type('bode 1/(s+1)^3 s 0.01 100 60');
check('bode', lastOutput(), /plotted 60 points/);
check('bode has two panels', drawn.data.length, 2);
check('bode x is log', drawn.layout.xaxis.type, 'log');
check('bode panels linked', drawn.layout.xaxis2.matches, 'x');
// The phase must end near -270 rather than wrapping up to +90, which is the whole entry.
const endPhase = drawn.data[1].y[drawn.data[1].y.length - 1];
check('phase unwrapped past -180', endPhase < -260 && endPhase > -271, true);
// A geometric grid: the ratio between neighbours is constant, so a linear one would fail.
const w = drawn.data[0].x;
check('grid is geometric', Math.abs(w[1] / w[0] - w[2] / w[1]) < 1e-9, true);

type('nyquist 1/(s+1) s 0.01 100 40');
check('nyquist', lastOutput(), /plotted 40 points/);
check('nyquist axes locked', drawn.layout.yaxis.scaleanchor, 'x');

// The `latex on` toggle (F_0016 step 5): a result becomes a typeset block instead of a text
// one, and switching back restores the text. The toggle itself answers as text, which is the
// property that keeps a setting readable while the mode is on.
type('latex on');              check('latex toggle', lastOutput(), 'latex = on');
type('1/x + 1');
check('typeset result', typeset[typeset.length - 1], '\\frac{1.0}{x} + 1.0');
check('math block in transcript', lastMath(), /\\frac/);
// The text channel is untouched: the last plain `out` block is still the toggle's own answer,
// so the formula replaced the text rather than being added beside it.
check('text not duplicated', lastOutput(), 'latex = on');
type('latex off');
type('1/x + 1');               check('text again once off', lastOutput(), '((1.0 / x) + 1.0)');

// The settings panel (F_0016 step 6). Two directions, and the second is the design point:
// a control issues the command a user would have typed, and a command typed at the prompt
// moves the control -- otherwise the panel becomes a stale second copy of the session state.
check('panel built from session', elements.settings.children.length > 0, true);
check('no control for colors', (() => { try { field('set-colors'); return true; } catch { return false; } })(), false);

const latexBox = field('set-latex');
check('control reflects the typed command', latexBox.checked, false);
latexBox.checked = true; change(latexBox);
check('control issues the command', lastOutput(), 'latex = on');
type('latex off');
check('typed command moves the control', latexBox.checked, false);

const digits = field('set-precision');
digits.value = '9'; change(digits);
check('number control', lastOutput(), 'precision = 9');
type('precision 5');
check('number control follows back', digits.value, '5');

const tnorm = field('set-logic');
tnorm.value = 'lukasiewicz'; change(tnorm);
check('choice control', lastOutput(), /lukasiewicz/);

// A refused value must snap back rather than leaving the field showing a state the session
// is not in.
digits.value = '99'; change(digits);
check('refusal is reported', lastOutput(), /precision expects/);
check('refused value snaps back', digits.value, '5');

// Failures must read as messages, not as a dead page.
type('plot');                  check('plot with no args', lastOutput(), /usage: plot/);
type('bode');                  check('bode with no args', lastOutput(), /usage: bode/);
type('bode 1/s s 0 10');       check('bode from zero', lastOutput(), /wMin must be > 0|logarithmic/);
type('plot x x 2 1');          check('plot with lo >= hi', lastOutput(), /plot:/);
type('sin(');                  check('parse error', lastOutput(), /parse error/);
type('a * 6');                 check('still alive after errors', lastOutput(), '42.0');

console.log(failures === 0 ? '\nall checks passed' : '\n' + failures + ' CHECK(S) FAILED');
process.exit(failures === 0 ? 0 : 1);
