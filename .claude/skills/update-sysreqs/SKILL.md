---
name: update-sysreqs
description: Update the embedded system requirements database in inst/sysreqs from the upstream r-hub/r-system-requirements repo. Use when asked to update, sync, or refresh the embedded sysreqs / sysreqs rules / sysreqs database.
---

# Update embedded sysreqs

pkgdepends ships a bundled copy of the [r-hub/r-system-requirements](https://github.com/r-hub/r-system-requirements)
database under [inst/sysreqs/](../../../inst/sysreqs/). At runtime pkgdepends
prefers a freshly-fetched cached copy, but falls back to this embedded one (see
`sysreqs2_list_rules()` in [R/sysreqs2.R](../../../R/sysreqs2.R)). This skill
refreshes the embedded copy to the current upstream HEAD.

Layout of `inst/sysreqs/`:
- `HEAD` — the 40-char upstream commit hash the embedded copy was taken from, plus a trailing newline.
- `VERSION` — the schema version, matching `sysreqs_db_version` in `R/sysreqs2.R`.
- `rules/*.json` — a **full mirror** of the upstream `rules/` directory (nothing is filtered out).

## Steps

1. Clone the upstream repo (shallow) into a scratch/temp dir and read its HEAD:
   ```bash
   git clone --depth 1 https://github.com/r-hub/r-system-requirements.git "$TMPDIR/r-system-requirements"
   git -C "$TMPDIR/r-system-requirements" rev-parse HEAD
   ```
   (A `failed to store: ...` keychain warning during clone is harmless in the sandbox.)

2. If that hash already matches `inst/sysreqs/HEAD`, there is nothing to do — stop and report that it is up to date.

3. Replace the rules and update HEAD (run from the repo root):
   ```bash
   rm -rf inst/sysreqs/rules
   cp -R "$TMPDIR/r-system-requirements/rules" inst/sysreqs/rules
   printf '%s\n' "$(git -C "$TMPDIR/r-system-requirements" rev-parse HEAD)" > inst/sysreqs/HEAD
   ```

4. Do **not** touch `VERSION` unless the upstream JSON *schema* changed in a way
   that breaks parsing in `sysreqs2_match()` / needs different files. If it did,
   bump both `inst/sysreqs/VERSION` and `sysreqs_db_version` in `R/sysreqs2.R` to
   the same new value.

5. Sanity-check the result:
   ```bash
   ls inst/sysreqs/rules | grep -v '[.]json$'   # expect no output (json only)
   git status --short inst/sysreqs               # review added/modified/removed rules
   ```

6. Report the HEAD change (old → new) and a summary of added / modified / removed
   rules. Commit only if the user asks; the conventional message is
   `Update embedded sysreqs`.
