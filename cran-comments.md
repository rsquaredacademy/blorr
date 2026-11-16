This is a patch release that fixes several bugs (wrong statistics) in model fit statistics,
variable selection, bivariate analysis, model validation, gains tables, weight
of evidence and the Gini index (now 2 * AUC - 1). See `NEWS.md` for the full
list. blorr now supports unweighted binary logistic (logit) models only;
weighted, grouped and non-logit models stop with a clear error instead of
returning statistics that do not apply to them.

## Test environments

* local: Windows 11 x64, R 4.6.1 (ucrt)
* win-builder: R-devel and R-release (4.6.2)
* R-hub: linux, windows, macos, macos-arm64 (R-devel)
* GitHub Actions: ubuntu-latest (devel, release, oldrel-1), windows-latest
  (release), macos-latest (release)
* Ubuntu 26.04, R 4.5.2

## R CMD check results

0 errors | 0 warnings | 1 note

The note is the expected maintainer-address change from
`hebbali.aravind@gmail.com` to `aravindhebbali@rsquaredacademy.com`:

```
New maintainer:
  Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s):
  Aravind Hebbali <hebbali.aravind@gmail.com>
```

## Reverse dependencies

We checked 2 reverse dependencies (midoc, xplorerr), comparing R CMD check
results with the CRAN and the new version of this package.

* We saw 0 new problems
* We failed to check 0 packages
