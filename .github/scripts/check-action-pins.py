"""Fail if any workflow references a GitHub Action by tag or branch instead of a commit SHA.

Issue 2.10 pinned every `uses:` to a SHA, because a tag is mutable and can be repointed by
whoever owns the action repository — and `ruby/setup-ruby@v1` was not even a tag but a
*branch*, whose head moves on every push.  An unpinned action therefore lets its owner change
what runs in `release.yml`, which carries the GPG signing key, and in `scala-steward.yml`,
which carries a repository-writing PAT.

**That sweep was one-time and nothing kept it true.**  This guard does: a new workflow, or a
hand-edited `uses:`, now fails CI instead of silently reopening the hole.  Written when issue
2.14 added a fifth workflow with an action the repository had not used before — the first
occasion since 2.10 where the invariant depended purely on remembering it.

Dependabot keeps the pins from becoming stale (`.github/dependabot.yml`), so pinning does not
degrade into freezing; this script only enforces that they are pins at all.

Exempt: local actions (`./.github/...`) and reusable *workflow* references, neither of which
resolves to third-party code.

Usage:  python check-action-pins.py [workflow-dir ...]     (default .github/workflows)
"""

import io
import os
import re
import sys

# `uses: owner/repo@ref` or `uses: owner/repo/path@ref`, quoted or bare.
USES = re.compile(r"""^\s*-?\s*uses:\s*["']?([^"'\s#]+)["']?""")
SHA = re.compile(r"^[0-9a-f]{40}$")


def offenders(path):
    """Yields (line number, reference) for every `uses:` that is not SHA-pinned."""
    with io.open(path, encoding="utf-8") as handle:
        for number, line in enumerate(handle, 1):
            match = USES.match(line)
            if not match:
                continue
            ref = match.group(1)
            # A local action is this repository's own code; there is nothing to pin.
            if ref.startswith("./") or ref.startswith(".\\"):
                continue
            _, _, version = ref.partition("@")
            if not SHA.match(version):
                yield number, ref


def main(argv):
    roots = argv[1:] or [os.path.join(".github", "workflows")]
    found = 0
    checked = 0
    for root in roots:
        for directory, _, names in os.walk(root):
            for name in sorted(names):
                if not name.endswith((".yml", ".yaml")):
                    continue
                path = os.path.join(directory, name)
                checked += 1
                for number, ref in offenders(path):
                    found += 1
                    print(f"{path}:{number}: not SHA-pinned: {ref}")

    print(f"\n{checked} workflow(s) checked, {found} unpinned reference(s)")
    if found:
        print("Pin to the commit SHA with the version as a trailing comment (issue 2.10);\n"
              "`git ls-remote --tags https://github.com/OWNER/REPO` resolves it without an\n"
              "API token or rate limit.")
    return 1 if found else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
