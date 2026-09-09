"""Detect characters from scripts this codebase cannot legitimately contain.

A safety net for the issue-2.8 mojibake repair.  The repair reverses a cp1252 round-trip, and
in rare cases a *correct* sequence reverses into valid UTF-8 by coincidence: `×–`
(U+00D7 U+2013) becomes bytes D7 96, which is valid UTF-8 for Hebrew ZAYIN.  A bad repair
therefore shows up as a character from an unrelated script.

This is a **blacklist, not a whitelist**, and deliberately so.  The first attempt enumerated
the allowed blocks and produced 222 false positives, because the legitimate mathematical
notation here is far wider than it looks: modifier letters (`Aᵀ`, `aᵛ`, `eˣ`, `logₖ`),
combining marks (`r̂`), long arrows (⟹), box drawing in separator comments, subscript j
(U+2C7C) — each a separate Unicode block.  Enumerating what mathematics is allowed to look
like is a losing game; enumerating the handful of scripts it will never use is not.
"""

import io
import os
import sys
import unicodedata

# Scripts that cannot appear in this project.  A hit is either a bad mojibake repair or
# something else that wants a human's attention.
FORBIDDEN = (
    (0x0400, 0x04FF, "Cyrillic"),
    (0x0530, 0x058F, "Armenian"),
    (0x0590, 0x05FF, "Hebrew"),
    (0x0600, 0x06FF, "Arabic"),
    (0x0700, 0x074F, "Syriac"),
    (0x0900, 0x097F, "Devanagari"),
    (0x0E00, 0x0E7F, "Thai"),
    (0x3040, 0x30FF, "Kana"),
    (0x4E00, 0x9FFF, "CJK"),
    (0xAC00, 0xD7AF, "Hangul"),
    (0xFFFD, 0xFFFD, "replacement character"),
    # C1 controls: invisible, and a reliable fingerprint of a half-decoded byte sequence.
    (0x0080, 0x009F, "C1 control"),
)

EXTS = (".scala", ".md", ".txt", ".puml", ".sbt")


def offending(ch: str):
    cp = ord(ch)
    for lo, hi, name in FORBIDDEN:
        if lo <= cp <= hi:
            return name
    return None


def main() -> int:
    found = 0
    for root in sys.argv[1:]:
        for d, _, fs in os.walk(root):
            for f in fs:
                if not f.endswith(EXTS):
                    continue
                path = os.path.join(d, f)
                for ln, line in enumerate(io.open(path, encoding="utf-8"), 1):
                    hits = {(c, offending(c)) for c in line if offending(c)}
                    if hits:
                        detail = ", ".join(
                            f"U+{ord(c):04X} ({script}, {unicodedata.name(c, '?')})"
                            for c, script in sorted(hits, key=lambda t: ord(t[0]))
                        )
                        print(f"{path}:{ln}: {detail}")
                        found += 1
    print(f"{found} suspect line(s)")
    return 1 if found else 0


if __name__ == "__main__":
    sys.exit(main())
