# descriptr 0.6.1 fix round — `dfx` report

Branch: `task/descriptr-fix-061` (worktree `F:/R/Packages/statistics/descriptr-wt/fix-061`),
based on `b2063b4` (0.6.1 release prep + revdep results).
Audit: `F:/pm/notes/2026-11-01-pkg-audits/cx-descriptr-audit.md`.
Rules applied: `common-pkgfix.md` (wrong results first, one commit per finding, NEWS bullet per
user-visible fix, clean `git archive` build + `R CMD check --as-cran`).

## Findings fixed

| Finding | Test added (fails before the fix) | Fix (file:line) | Verified how | Status |
| --- | --- | --- | --- | --- |
| **H1** summary names overwrite source columns | `test-group-summary.R` "does not overwrite source columns named like statistics (H1)": `missing`, `min`, `mean` source columns and a grouping column named `length`; also `test-group-summary-interact.R` "does not overwrite source columns (H1)" for `min`/`Levels`/group named `min` | `R/ds-group-summary.R:101-170` — one grouped helper computes all statistics from the original vector; `stats` and `tidy_stats` are built from the same matrix; grouping keys kept separately. `R/ds-group-summary-interact.R:63-87` — same for interaction summaries | Audit's exact probes return min/max/mean `1/3/2` and `5/5/5`, variance `2/NA`; `tidy_stats` matches the printed table; grouping labels preserved for a `length` grouping column | Fixed |
| **H2** delimiter grouping merges distinct groups | `test-group-summary-interact.R` "keeps colliding group labels separate (H2)": `a\rb`/`a` × `c`/`b\rc`, plus a three-row nearby case | `R/ds-group-summary-interact.R:48-87` — group by the selected columns with `interaction(..., drop = TRUE)`; no label join/split | Two rows with means `1` and `100` (and `1`/`60`); labels preserved; `aggregate()` agrees. Dot labels and character/factor results still pass the existing tests | Fixed |
| **H3** cross-table plots count excluded observations | `test-cross-table.R` "cross table plots use only the observations in the table (H3)": audits' 3-row probe and asymmetric `mtcarz` missingness (obs 30) | `R/ds-cross-table.R:137-168` — complete cases of the two selected variables are selected once and used by all three plot branches | `ggplot_build()$data[[1]]$count` sums to `utility$obs` for default, stacked and proportional plots (1 and 30) | Fixed |
| **H4** all-missing input yields `Inf`/`NaN` extrema/bins | `test-summary-stats.R` "an all-missing variable returns missing metrics (H4)" (`NA` vector and `numeric(0)`, printing incl. data frame); `test-freq-table.R` "continuous frequency tables reject data with no usable observations (H4)" | `R/ds-summary-stats.R:95,109-140` — metrics are `NA` when there are no usable values (lazy helper); `R/ds-output.R:8-11` — extreme-value widths no longer call `max()` on empty vectors; `R/ds-freq-numeric.R:15-17` — clear error instead of `Inf`/`NaN` bins | Summary prints `NA`, no warnings, no `Inf`/`NaN`; frequency table stops with `Data has no non-missing observations; cannot construct continuous bins.` | Fixed |
| **H5** harmonic mean returns 0 for small positive data | `test-describe.R` "harmonic mean is stable for extreme magnitudes (H5)" | `R/ds-describe.R:563-576` — scaled reciprocals `s / mean(s / z)`, `s = min(z)`; non-finite input warns and returns `NA` | `ds_hmean(rep(1e-308, 10)) == 1e-308`, `ds_hmean(c(1e-310, 1e-310)) == 1e-310`, mixed `c(1e-300, 1e300) == 2e-300`, `c(2,4) == 4/3` | Fixed |
| **H6** interval range overflows | `test-freq-table.R` "bin boundaries stay finite and increasing for extreme ranges (H6)" | `R/ds-utils.R:48-79` — boundaries from `lo + (hi - lo) * t`, falling back to the convex combination `lo*(1-t) + hi*t` when `hi - lo` is `Inf`; finiteness/monotonicity checked (`bin_size()` removed, unused) | `c(-1e308, 0, 1e308)` gives finite increasing breaks and frequencies `1, 0, 1, 0, 1`; `c(-1e308, 1e308)` gives `1, 1`; audit's case reproduced exactly | Fixed |
| **M1** join message suppressed by using `deparse()` keys | `test-cross-table.R` "ds_twoway_table supports programmatic column selection (M1)" | `R/ds-cross-table.R:194-200, 242` — join keys from the resolved columns (`select(...) %>% names()`); `deparse()` kept only for messages | `!!rlang::sym()` and `{{ }}` calls both equal the direct call (8 rows, counts `1,8,2,2,4,1,12,2`, sum 32) | Fixed |
| **M2** empty grouping labels give a low-level error | `test-group-summary.R` "reports when there are no usable groups (M2)" (all-NA labels and empty data) | `R/ds-group-summary.R:81-88` — zero usable groups stop with `No data to summarise. ...` | Both audit probes raise the documented message instead of `non-numeric argument to mathematical function` | Fixed |
| **M3** maintainer email | — (metadata; verified by build/check) | `DESCRIPTION:5`, `man/descriptr.Rd:22` (`aravindhebbali@rsquaredacademy.com`) | `R CMD check` reports `Maintainer: 'Aravind Hebbali <aravindhebbali@rsquaredacademy.com>'`; Rd matches DESCRIPTION | Fixed |
| **M6** tail-observation count not validated (wrong row for `n = 0`) | `test-describe.R` "ds_tailobs validates n (M6)" | `R/ds-describe.R:445-468` — `n` must be one finite whole number; `n = 0` returns `numeric(0)`; `seq_len()` | `ds_tailobs(1:3, 0)` is `numeric(0)`; `1.5`, `-1`, `NA_real_`, `Inf`, `c(1,2)` are rejected; `n` above the usable count still errors | Fixed |
| **L3** `docs-dev/` not excluded from source builds | — (build rule) | `.Rbuildignore:11` (`^docs-dev$`) | Rebuilt tarball from `git archive HEAD` after adding this report; `tar -tzf` contains no `docs-dev` | Fixed |

M6 is listed as "nice to have" in the audit but returns a wrong row (`n = 0` returns one observation),
so it was fixed under the wrong-results rule.

## Deferred (not fixed in this round)

- **M4** vector vs data-frame NA policy: `ds_measures_*`/`ds_percentiles` still error or count NAs on
  bare vectors while data-frame input drops them. This needs a policy decision (`na.rm` argument or
  documented "n = total" semantics) rather than a mechanical fix; NEWS wording is already limited to
  data-frame input.
- **M5** frequency Percent mixes valid-observation and total denominators when data are missing
  (`a = 100`, `Missing = 33.33`). Each number is internally correct; choosing "valid percent" vs
  "total percent" (or printing both) is a display/API decision that would change existing output.
- **L1** `README.md` is stale relative to `README.Rmd` (proportions, blank `by` title, missing row,
  tie order). It is generated output; rebuilding it is a release-doc step, not a code fix.
- **L2** `cran-comments.md` still has `TODO`/R-hub/macOS lines and the old V8-based note. It must be
  updated before submission; note the required email change now produces CRAN's "New maintainer"
  NOTE (see below), which should be explained there.
- **L4** roxygen inaccuracies: `ds_cross_table` `stacked` description reversed,
  `boxplot.group_summary()` does not exist, variation described as "location", literal
  `\code{link{...}}` in `ds-mult-table.R`, `x` vs `data` in `ds-multistats.R`. Regenerating `man/`
  with the installed roxygen2 8.1.1 would rewrite every Rd file and bump `RoxygenNote` (repo is on
  7.2.3), so it is left to the maintainer's next documentation run.
- **L5** remaining direct-call/snapshot gaps: `ds_launch_shiny_app`, deprecated wrapper forwarding
  tests, mocked launcher, and plot-count assertions beyond cross tables. The H1–H6/M1–M2 value tests
  were added; the rest is test-suite work.
- Audit leftovers under `revdep/.cx-audit-tmp/` from the previous audit were not touched.

## API/behaviour changes required by the fixes

- `ds_freq_table()` now errors for continuous data with no non-missing observations (previously printed
  `Inf`/`NaN` bins).
- `ds_tailobs()` rejects negative/fractional/`NA`/infinite `n` and returns `numeric(0)` for `0`.
- `ds_group_summary()` errors when no observation has a non-missing grouping label.
- `ds_hmean()` warns and returns `NA` for non-finite input.
- `ds_group_summary()$tidy_stats` `length` is now double and, when a grouping column is named like one
  of the statistics (e.g. `length`), the tibble keeps both columns with duplicate names (labels first).

## Build and check

Clean source tree: `git archive HEAD` → `source/` in
`C:/Users/DELL/AppData/Local/Temp/opencode/dfx-check` (outside the worktree), `TMPDIR/TMP/TEMP`
pointed inside that directory, `LANG`/`LC_ALL` = `en_US.UTF-8`.

```
R CMD build --no-manual source          # OK, vignettes built
R CMD check --as-cran --no-manual descriptr_0.6.1.tar.gz
```

Result: **Status: 1 NOTE**, 0 errors, 0 warnings. The NOTE is CRAN incoming feasibility reporting:

```
New maintainer:
  Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s):
  Aravind Hebbali <hebbali.aravind@gmail.com>
```

This is expected: it is the metadata change required by M3. It disappears once CRAN records the new
maintainer address, and should be explained in `cran-comments.md` (L2, deferred).

Tarball: 201 entries, no `docs-dev`, `revdep`, `docs`, `README.Rmd`, `AGENTS.md` or
`cran-comments.md`. Examples run (15 s), tests pass (16 s), vignettes rebuild (17 s).

The build/check was repeated from the final `git archive HEAD`, which now contains `docs-dev/` and
this report: same result, and the tarball still has 201 entries with no `docs-dev`.

Development suite: `devtools::test()` — **687 passed / 0 failed / 0 warnings / 0 skipped**
(excluding the 3 incidental `NULL` prints that already existed in the suite output). The suite grows
from 586 passes by the new value assertions.

## Commands run that changed something

```text
Rscript -e 'devtools::test(...)'                      # baseline, then after each fix
# edits: R/ds-group-summary.R, R/ds-group-summary-interact.R, R/ds-cross-table.R,
#        R/ds-summary-stats.R, R/ds-freq-numeric.R, R/ds-output.R, R/ds-utils.R,
#        R/ds-describe.R, DESCRIPTION, man/descriptr.Rd, .Rbuildignore, NEWS.md,
#        tests/testthat/{test-group-summary,test-group-summary-interact,test-cross-table,
#        test-summary-stats,test-freq-table,test-describe}.R
git add ...; git commit ...                           # one commit per finding/group (see git log)
git archive --format=tar --output=.../source.tar HEAD
R CMD build --no-manual source
R CMD check --as-cran --no-manual descriptr_0.6.1.tar.gz
tar -tzf descriptr_0.6.1.tar.gz                       # file-list verification
```

## Not done

- No push, no GitHub operations, no branch switch, no changes to other worktrees.
- No new dependencies; no exported API removed or renamed.
- `README.md` and `cran-comments.md` were not regenerated (L1, L2).
- `man/` was not regenerated with roxygen; the only Rd edit is the maintainer email line that
  roxygen derives from `DESCRIPTION` (L4 deferred).
- Windows-only check environment (R 4.6.1 ucrt); no win-builder/macOS/R-hub runs were attempted.
- The committed pkgdown site (`docs/`) was not rebuilt, so `docs/reference/descriptr.html`,
  `docs/reference/descriptr.md` and `docs/search.json` still show the old maintainer address. The site
  is excluded from the source tarball and regenerating it is a separate pkgdown build.
