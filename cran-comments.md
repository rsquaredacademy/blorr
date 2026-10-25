This is a patch release that fixes several bugs in model fit statistics,
variable selection, bivariate analysis, model validation, gains tables, weight
of evidence and the Gini index (now 2 * AUC - 1). See `NEWS.md` for the full
list.

## Test environments

* local: Windows 11, R 4.6.1
* TODO: win-builder (devel, release)
* TODO: R-hub
* TODO: macOS

## R CMD check results

0 errors | 0 warnings | 1 note

The note is specific to this machine: `Skipping checking math rendering: package 'V8' unavailable`. `V8` is not installed here; the package itself is unaffected.

## Reverse dependencies

We checked 2 reverse dependencies (midoc, xplorerr), comparing R CMD check
results across CRAN and dev versions of this package.

* We saw 0 new problems
* We failed to check 0 packages
