# descriptr 0.6.1 fix round 2 — `dfx2` report

Branch: `task/descriptr-fix-061` (worktree `F:/R/Packages/statistics/descriptr-wt/fix-061`),
head `89339c4` at the start of this round (9 `dfx` commits on `b2063b4`).
Audit: `F:/pm/notes/2026-11-01-pkg-audits/dfxx-reaudit.md` (verdict: wrong
results left = yes).
Rules applied: `common-pkgfix.md` (wrong results first; a test that fails
before the fix; value assertions; one commit per finding/tight group; clean
`git archive` build + `R CMD check --as-cran`).

All 10 remaining findings are fixed. The three wrong-result examples named in
the brief — dotted group labels merging, duplicate-name errors and ordered
factors losing their class — are reproduced by value-asserting tests first
and were confirmed failing at `89339c4` before the fix (run output: 40 failed,
3 errors in the four targeted files).

## Findings fixed

| Finding | Test added (fails before the fix) | Fix (file:line) | Verified how | Status |
| --- | --- | --- | --- | --- |
| **RH1** dotted interaction labels merge (H2 regression) | `test-group-summary-interact.R`: "keeps distinct dotted label tuples separate (RH1)" — the audit's exact `("a.b","c")/("a","b.c")` case, key-to-mean mapping against `aggregate()`, plus a repeated-tuple/third-column case; the H2 test now asserts the whole key-to-statistic mapping instead of sorted values | `R/ds-group-summary-interact.R:64-66` — `dplyr::group_by()` + `group_rows()`/`group_keys()` on the selected columns; no `interaction()` and no string serialization | Audit probe returns 2 rows with `a.b`/`c` → mean `1` and `a`/`b.c` → mean `100`, identical to `aggregate()`; the `\r` H2 case still returns `1`/`100` and `1`/`60`; ordinary `mtcarz` output unchanged | Fixed |
| **RH2** group boxplots include excluded observations | `test-group-summary.R`: "group summary boxplots exclude missing grouping labels (RH2)" | `R/ds-group-summary.R:91-97` — `plot_data` is built after the missing-grouping-label filter, so the plot and summary use the same rows | Audit probe: summary has only level `a` (Mean 2); `ggplot_build()` returns one box with `middle = 2`, `ymin = 1`, `ymax = 3`, no `NA` category | Fixed |
| **RM1** duplicate names in tidy results | `test-group-summary.R`: "returns unique column names (RM1)" (`length` grouping column); `test-group-summary-interact.R`: "returns unique column names (RM1)" (`mean` grouping column) | `R/ds-group-summary.R:163-167` + `R/ds-utils.R:138-143` (`unique_group_name()`); `R/ds-group-summary-interact.R:88-95` | `names()` has no duplicates; colliding grouping columns become `length_group` / `mean_group`; `tidy_stats$length` stays the statistic (2, 1) and `k$mean` stays the numeric means (2, 5); `dplyr::mutate()`, `dplyr::filter()` and `.data[["max"]]` work | Fixed |
| **RM2** ordered grouping keys lose their class | `test-group-summary.R`: "keeps ordered grouping keys ordered (RM2)" | `R/ds-group-summary.R:164` using `group_key_vector()` (`R/ds-utils.R:132-135`) — keys are selected from the original grouping vector instead of rebuilt from strings | `class()` is `c("ordered","factor")`, observed levels stay `c("c","b","a")` with unused dropped, means `10/6/2`, `<` is `TRUE`; an unordered factor is still `"factor"` | Fixed |
| **M4** vector/frame NA semantics inconsistent | `test-describe.R`: "vector and data-frame NA handling is consistent (M4)" | `R/ds-describe.R:150-158` (variation), `:244-247` (symmetry), `:343-357` (percentiles); `R/ds-utils.R:165-171` (`ds_std_error()` now drops NAs before `sd()` and `n`) | `c(1,2,NA)` gives `n = 2`, range `1`, IQR `0.5`, variance `0.5`, sd `0.71`, CV `47.14`, std error `0.5`; `c(1,2,NA,5)` gives symmetry `n = 3`; percentiles no longer raise a base error and equal the data-frame path column for column; all-missing vectors give `n = 0` and `NA`; `ds_std_error()` matches `sd(x)/sqrt(n)` | Fixed |
| **M5** one Percent heading, two denominators | `test-freq-table.R`: "frequency table percentages use a single denominator (M5)" (categorical, continuous and automatic tables) | `R/ds-freq-factor.R:28-33`, `R/ds-freq-numeric.R:34-37`, `R/ds-freq-mult.R:20-24` — all percentages use the total observation count; documented in `R/ds-freq-table.R` | Audit's `a,a,NA` case now prints/stores level `66.67` and Missing `33.33` (sum 100); `c(1,2,3,NA)` bins store `25/50` with Missing `25`; `freq_table2()` agrees; `test-output.R`/`test-auto-summary.R` snapshots updated to the new numbers | Fixed |
| **L1** stale README | — (generated output) | `README.md` rebuilt from `README.Rmd` with `devtools::build_readme()` | Diff now shows cross-table percentages `3.12/25.00/6.25`, title `mpg by cyl`, the `Missing` row, and high observation indices `28` before `19` (tie order) | Fixed |
| **L2** submission comments wrong/incomplete | — (metadata) | `cran-comments.md` | Records the Windows 11 / R 4.6.1 check, explains the reproduced NOTE (new maintainer address, not V8), removes the unrun platform TODOs and states how/when revdeps were run | Fixed |
| **L4** inaccurate public documentation | — (documentation) | `R/ds-cross-table.R:10-11` (stacked/proportional), `R/ds-group-summary.R:4` (`plot.ds_group_summary()`), `R/ds-describe.R:74` ("variation"), `R/ds-mult-table.R:29` (`\link{}`), `R/ds-multistats.R:6` (`data`); `man/`, `NAMESPACE`, `DESCRIPTION` regenerated with roxygen2 8.1.1 | R CMD check: Rd files, cross-references and code/documentation mismatches all `OK`; new details document the NA policy, percent denominator, `_group` naming and unbinnable-data errors | Fixed |
| **L5** regression coverage gaps | `test-describe.R` H5 test now asserts scaled ratios and positivity (`1e-308`-scaled result equals `1`, not just `0`); new `test-deprecated.R` covers `ds_multi_stats`, `ds_oway_tables`, `ds_tway_tables` and a mocked `ds_launch_shiny_app`; `ds_std_error` value assertions added in the M4 test; dotted/RM1/RM2/RH2 assertions above | — (tests only, plus the `all_of()` change in `R/ds-freq-mult.R:9` that the new direct-call test exposed as a tidyselect deprecation warning) | `devtools::test()`: 801 passed / 0 failed / 0 warnings / 0 skipped (was 687). Installed check: 753 passed / 28 skipped ("On CRAN") / 0 failed. The H5 ratio assertions fail against the original `0` result | Fixed (see residual notes) |

## Re-audit "diff review" items addressed

- NEWS bullet for bin boundaries reworded: the implementation does compute
  `max(x) - min(x)` to choose the fallback, but never uses an overflowing
  difference for the boundaries.
- NEWS now states the non-finite harmonic-mean rejection and the returned-data
  API notes (`length` is a double; interaction quartiles no longer carry
  `25%`/`75%` names).
- The old `dfx-report.md` H2 row and the duplicate-name API note are marked as
  corrected by this round.
- Duplicate NEWS bullet for the old H2 fix removed; one bullet now states that
  `.`, `\r` and any other separator are safe.

## Deferred / not changed

- **NEWS heading.** The new bullets stay under `# descriptr 0.6.1`, which is
  the version in `DESCRIPTION` being prepared for release (not an `x.y.z.9000`
  development version); the AGENTS.md development-heading rule applies to
  `.9000` versions. This is an organization preference, not a wrong result.
- **RH1 test of the launcher itself.** `ds_launch_shiny_app()` is tested with
  `check_suggests()` mocked at the tenth dependency, so all ten suggested
  packages are asserted but the app is never started (it would block the test
  run). The `xplorerr::app_descriptive()` call itself remains untested.
- **Plot-count assertions** cover cross tables and group-summary boxplots only;
  other plot types still rely on vdiffr snapshots.
- **Reverse-dependency check** was not re-run (see `cran-comments.md`); the
  0.6.0→0.6.1 results in `revdep/` are reported, and no exported signature
  changed.
- `docs/` (pkgdown site) was not rebuilt; it is excluded from the source
  tarball. `docs-dev/dfxx-reaudit.md` (the auditor's file) is left uncommitted.
- Platform coverage is Windows 11 / R 4.6.1 only; no win-builder, macOS or
  R-hub run.

## Checks

Clean source tree: `git archive HEAD` → `source/` under
`C:/Users/DELL/AppData/Local/Temp/opencode/dfx2-check`, with `TMPDIR/TMP/TEMP`
inside that directory and `LANG`/`LC_ALL` = `en_US.UTF-8`.

```
R CMD build --no-manual source          # OK, vignettes built
R CMD check --as-cran --no-manual descriptr_0.6.1.tar.gz
```

Result: **Status: 1 NOTE**, 0 errors, 0 warnings. The NOTE is the expected
CRAN incoming-feasibility maintainer change:

```text
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Aravind Hebbali <aravindhebbali@rsquaredacademy.com>'

New maintainer:
  Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s):
  Aravind Hebbali <hebbali.aravind@gmail.com>
```

Tarball: **202 entries** (one more than before: `test-deprecated.R`); no
`docs-dev`, `revdep`, `docs`, `README.Rmd`, `AGENTS.md` or `cran-comments.md`.
Examples OK, installed tests OK, vignettes rebuilt OK.

Development suite: `devtools::test()` — **801 passed / 0 failed / 0 warnings /
0 skipped**. The incidental `NULL` prints reported by the audit remain.

## Commands run that changed something

```text
Rscript -e 'devtools::test(...)'            # baseline, targeted files, final run
Rscript -e 'devtools::document()'           # regenerated man/, NAMESPACE, DESCRIPTION
Rscript -e 'devtools::build_readme()'       # regenerated README.md
testthat::snapshot_accept(...) / cp *.new.md  # reviewed and accepted intended snapshot changes
rm -rf revdep/.dfxx-audit-tmp               # audit leftovers (requested)
git add ...; git commit ...                 # commits below, each ending (dfx2)
git archive --format=tar --output=.../source.tar HEAD
R CMD build --no-manual source
R CMD check --as-cran --no-manual descriptr_0.6.1.tar.gz
```

Commits:

```text
fix: group summary keys, names and plots (RH1, RH2, RM1, RM2) (dfx2)
fix: consistent NA handling for vector summary statistics (M4) (dfx2)
fix: use one denominator for frequency table percentages (M5) (dfx2)
test: cover deprecated wrappers and the shiny launcher (L5) (dfx2)
docs: correct public documentation and regenerate (L4) (dfx2)
docs: refresh README, NEWS and submission comments (L1, L2) (dfx2)
docs: correct H2 claims and add API notes (dfx2)      # NEWS/dfx-report corrections
docs: dfx2 fix report (dfx2)                          # this file
```

## Files changed

- Code: `R/ds-group-summary.R`, `R/ds-group-summary-interact.R`,
  `R/ds-utils.R`, `R/ds-describe.R`, `R/ds-freq-factor.R`,
  `R/ds-freq-numeric.R`, `R/ds-freq-mult.R`, `R/ds-freq-table.R` (docs only),
  `R/ds-cross-table.R` (docs only), `R/ds-mult-table.R` (docs only),
  `R/ds-multistats.R` (docs only).
- Tests: `tests/testthat/test-group-summary.R`,
  `test-group-summary-interact.R`, `test-describe.R`, `test-freq-table.R`,
  new `test-deprecated.R`, and the intended snapshot updates in
  `_snaps/auto-summary.md` and `_snaps/output.md`.
- Generated/metadata: `man/` (11 files), `NAMESPACE`, `DESCRIPTION`,
  `README.md`, `NEWS.md`, `cran-comments.md`,
  `docs-dev/dfx-report.md` correction notes.

## Not done

- No push, no GitHub/gh operations, no branch switch, no other worktrees, no
  new dependencies, no exported function removed or renamed.
- `docs/` (pkgdown) was not rebuilt; `docs-dev/dfxx-reaudit.md` was not
  committed.
- Windows-only check environment (R 4.6.1 ucrt, dplyr 1.2.1, tidyr 1.3.2,
  ggplot2 4.0.3, testthat 3.3.2).
