"""Repair UTF-8-read-as-cp1252 mojibake (issue 2.8).

The damage is `original.encode('utf-8').decode('cp1252')`, so the repair is that round-trip
inverted.  Three complications make a naive `.encode('cp1252').decode('utf-8')` insufficient,
and each is handled explicitly below.

1.  **cp1252 has five undefined slots** (0x81, 0x8D, 0x8F, 0x90, 0x9D).  The tool that did the
    original damage passed those bytes through as raw C1 code points, but Python's `cp1252`
    codec *raises* on them, so the single most common sequence here — `⁻¹`, whose UTF-8 is
    E2 **81** BB C2 B9 — fails to encode and survives untouched.  `_to_bytes` therefore maps
    U+0081 and friends straight back to their byte value.

2.  **Some text was damaged twice.**  `Ã¢â‚¬â€` is an em dash that went through the bad decode
    two times over.  `repair` iterates to a fixpoint rather than passing once.

3.  **Some damage is lossy and cannot be reversed arithmetically.**  Where a later tool
    stripped a C1 control outright, or normalised the smart quote U+201D to an ASCII `"`, the
    byte is simply gone.  `LOSSY` maps those few sequences by hand; every entry was confirmed
    against the surrounding prose, not guessed from the byte pattern alone.

Correct text is safe because repair works on maximal runs and substitutes only when the whole
run round-trips: a genuine `⁻¹` (U+207B) is not cp1252-encodable, so its run fails and is left
exactly as it was.

Usage:  python fix-mojibake.py --check <paths...>   (report only; non-zero exit if damaged)
        python fix-mojibake.py --write <paths...>   (repair in place)
"""

import io
import os
import sys

# cp1252 byte -> character, inverted.  Built from the codec so it cannot drift from reality.
_CHAR_TO_BYTE = {}
for _b in range(0x80, 0x100):
    try:
        _CHAR_TO_BYTE[bytes([_b]).decode("cp1252")] = _b
    except UnicodeDecodeError:
        pass  # an undefined slot; handled by the C1 pass-through below

# The five slots cp1252 leaves undefined, which the damaging tool emitted as raw C1 controls.
for _b in (0x81, 0x8D, 0x8F, 0x90, 0x9D):
    _CHAR_TO_BYTE[chr(_b)] = _b

# Damage that lost a byte outright and so cannot be inverted; each confirmed against context.
LOSSY = {
    # E2 80 [94] -> em dash; the 0x94 smart quote was later normalised to an ASCII quote.
    'â€"': "—",
    # E2 [94] 80 -> box-drawing horizontal, used in this codebase's separator comments.
    'â"€': "─",
    # E2 [81] B4 / B5 -> superscript four / five; the C1 control was deleted outright.
    "â´": "⁴",
    "âµ": "⁵",
}


def _to_bytes(run: str):
    """The cp1252 encode the damaging tool's decode implies, or None if this run is clean."""
    out = bytearray()
    for ch in run:
        b = _CHAR_TO_BYTE.get(ch)
        if b is None:
            return None
        out.append(b)
    return bytes(out)


# Scripts this project never uses.  A "repair" landing in one of them is a coincidence, not a
# repair -- see `_plausible`.  Kept in step with .github/scripts/check-charset.py.
_FORBIDDEN = (
    (0x0400, 0x04FF), (0x0530, 0x058F), (0x0590, 0x05FF), (0x0600, 0x06FF),
    (0x0700, 0x074F), (0x0900, 0x097F), (0x0E00, 0x0E7F), (0x3040, 0x30FF),
    (0x4E00, 0x9FFF), (0xAC00, 0xD7AF), (0xFFFD, 0xFFFD),
)


def _plausible(decoded: str) -> bool:
    """Reject a round-trip whose output lands in a script this codebase never uses.

    The round-trip is not injective over *correct* text: `×–` (U+00D7 U+2013) encodes to
    bytes D7 96, which is valid UTF-8 for Hebrew ZAYIN, so a genuine "100×–4 700×" in
    `docs/src/developer.md` would be silently corrupted into "100ז4 700×".  Real mojibake
    here always decodes back to Latin, Greek, punctuation or mathematical symbols, so a
    result outside those is evidence the input was never damaged in the first place.
    """
    return not any(lo <= ord(c) <= hi for c in decoded for lo, hi in _FORBIDDEN)


def _pass(text: str) -> str:
    """One repair pass over maximal non-ASCII runs."""
    out, buf = [], []

    def flush() -> None:
        if not buf:
            return
        run = "".join(buf)
        buf.clear()
        raw = _to_bytes(run)
        if raw is None:
            out.append(run)
            return
        try:
            decoded = raw.decode("utf-8")
        except UnicodeDecodeError:
            out.append(run)
            return
        out.append(decoded if decoded != run and _plausible(decoded) else run)

    for ch in text:
        if ord(ch) >= 0x80:
            buf.append(ch)
        else:
            flush()
            out.append(ch)
    flush()
    return "".join(out)


def repair(text: str) -> str:
    """Apply the lossy table, then iterate the round-trip to a fixpoint (double encodings)."""
    for damaged, original in LOSSY.items():
        text = text.replace(damaged, original)
    for _ in range(5):
        nxt = _pass(text)
        if nxt == text:
            return text
        text = nxt
    return text


def scan(paths):
    """Yield (path, original, repaired) for every file this would change."""
    for root in paths:
        walk = (
            [root]
            if os.path.isfile(root)
            else [
                os.path.join(d, f)
                for d, _, fs in os.walk(root)
                for f in fs
                if f.endswith((".scala", ".md", ".txt", ".puml", ".sbt"))
            ]
        )
        for path in walk:
            original = io.open(path, encoding="utf-8").read()
            fixed = repair(original)
            if fixed != original:
                yield path, original, fixed


def main() -> int:
    mode, paths = sys.argv[1], sys.argv[2:]
    changed = list(scan(paths))

    for path, original, fixed in changed:
        diffs = sum(a != b for a, b in zip(original.split("\n"), fixed.split("\n")))
        print(f"{'FIX ' if mode == '--write' else 'HAS '}{path}  ({diffs} lines)")
        if mode == "--write":
            io.open(path, "w", encoding="utf-8", newline="\n").write(fixed)

    print(f"\n{len(changed)} file(s) {'repaired' if mode == '--write' else 'affected'}")
    return 0 if (mode == "--write" or not changed) else 1


if __name__ == "__main__":
    sys.exit(main())
