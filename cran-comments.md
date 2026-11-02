## Patch release

This is a patch release that fixes crashes and incorrect results in group
summaries, frequency and cross tables, and summary statistics.

## Test environments

* local Windows 11, R 4.6.1 (ucrt)

## R CMD check results

0 errors | 0 warnings | 1 note

The note is CRAN's incoming feasibility check reporting the new maintainer
address:

```
New maintainer:
  Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s):
  Aravind Hebbali <hebbali.aravind@gmail.com>
```

The address was updated in `DESCRIPTION` in this release and is the address
CRAN should use from now on. The note disappears once CRAN records it.

## Reverse dependencies

We checked 3 reverse dependencies (olsrr, StatisticTeach1, xplorerr), comparing
R CMD check results across CRAN and dev versions of this package.

* We saw 0 new problems
* We failed to check 0 packages

These checks ran on the 0.6.1 release preparation; the fixes added on top of it
do not change any exported function signature or argument. (To be re-run on the
final code before submission.)
