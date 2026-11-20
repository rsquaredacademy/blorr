# pkg-blorr-c25-fu2: binary-response checks for data-column APIs

2026-11-19. Follow-up to L1 and L2 in `docs-dev/pkg-blorr-c25-rx-audit.md`,
after reading `F:/pm/notes/package-audit-stop-rule.md`. Starting HEAD:
`3f154f81adc4ae3c11d2829c25acf38cdffc844c`, branch `task/blorr-c25`.

## Changes and supported responses

The five previously accepting APIs now call the existing
`blr_check_binary_response()` before computing or printing results:

- `blr_woe_iv()`
- `blr_woe_iv_stats()`
- `blr_segment()`
- `blr_segment_dist()`
- `blr_segment_twoway()`

The check counts observed, non-missing response levels, using `factor()` on
the validation input to discard unused factor levels. It does not change
the response passed to the calculations. `exactly = FALSE` preserves the
existing handling of single-outcome data. Numeric 0/1, two-level factors,
logical and two-value character responses retain their behavior.

A response named `y` with three observed levels now raises the existing
model-function error:

```text
The response `y` has 3 levels; blorr supports a binary response with exactly two levels.
```

This closes L2's stop-rule honesty blocker: the five APIs no longer return
the undocumented second-level-versus-rest result for a multilevel response.
The response requirement is documented in the five help pages and their
roxygen sources, and in the development NEWS section.

## L1: actual glm behavior

Executed the following against R 4.6.1; output is retained in
`glm-probe.log` in the scratch directory below:

```r
d <- hsb2
d$char3 <- as.character(d$ses)
glm(char3 ~ female + read, data = d, family = binomial)
# Error: y values must be 0 <= y <= 1

mf <- glm(ses ~ female + read, data = d, family = binomial)
table(mf$y)
#   0   1
#  47 153
table(mf$model[[1]])
#  1  2  3
# 47 95 58
```

Thus the silent recoding claim applies to the factor response in this
example. The character response errors during `glm()` initialization.
NEWS now states both behaviors accurately; the matching internal helper
comment also drops the incorrect character-response claim.

## Regression evidence

Added `tests/testthat/test-data-column-response.R`. Each API has a rejection
test covering three-level factor, character and numeric responses, and a
binary regression test comparing returned values and printed output across
numeric 0/1, labeled factor, logical, character and unused-level factor
encodings.

Ran this test file before changing package code, against starting HEAD's
implementation, then included it in the full post-fix suite:

| Run | Files | Tests | Failed expectations | Errors | Warnings | Skips | Passed expectations |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| Starting HEAD code with new regression tests | 1 | 10 | 15 | 0 | 0 | 0 | 50 |
| Fixed code, full `devtools::test()` | 27 | 256 | 0 | 0 | 0 | 0 | 2066 |

All 15 rejection expectations fail before the fix (three response types for
each of five APIs); all 50 binary assertions already pass before the fix.
The full post-fix suite passes all 65 new assertions and the existing 2001.
The full test ran from 19:01:59 to 19:05:21 IST.

A separate ordinary-response comparison ran the five APIs on ten datasets
against an archive of starting HEAD and the fixed source. The datasets cover
numeric, factor, logical, character, unused factor levels, reversed factor
levels, missing numeric/factor responses, and each single numeric outcome.
All 50 comparisons are identical at `all.equal(..., tolerance = 0)`, including
returned values, printed output, errors, warnings and messages. Both versions
have the same pre-existing `invalid 'times' argument` error when printing the
all-zero `blr_segment_twoway()` result; this follow-up does not change that
degenerate printer behavior. The other 49 calls complete without errors.

## Full package check

Ran the requested full check on a separate source copy outside the worktree:

```r
devtools::check(path, document = FALSE, args = "--no-manual",
                check_dir = check_dir, error_on = "never")
```

The changed package, help, NEWS and test files in that source copy were
verified against the worktree using SHA-256 hashes (zero mismatches).
`docs-dev/` is excluded from package builds by the existing `.Rbuildignore`.
The check retained devtools' default `--as-cran`; `error_on = "never"` only
keeps the result object available for reporting. No check stages were removed.

```text
* checking examples ... [26s] OK
* checking examples with --run-donttest ... [81s] OK
* checking tests ... [100s] OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... [36s] OK
* DONE
Status: OK
CHECK_TOTALS: errors = 0 warnings = 0 notes = 0
```

Inside R CMD check, testthat reports
`[ FAIL 0 | WARN 0 | SKIP 2 | PASS 2063 ]`. The two unchanged skips are
`test-gini-index.R:83` and `test-stepwise-selection.R:62`: installed packages
do not contain the raw `man/*.Rd` files those tests inspect. Their three
expectations pass in the full source-tree test, explaining its 2066 passed
expectations and zero skips. The full check, including the package build,
ran from 19:04:08 to 19:16:12 IST.

## Scope and artifacts

No version bump: `DESCRIPTION` remains `0.3.2.9000`. L3 is left as requested:
`docs-dev/c25-report.md` is not renamed. No branch switch, push, GitHub access
or issue creation was performed. Test-generated plot and line-ending changes
were restored and are excluded from this change.

R 4.6.1 (2026-06-24 ucrt), Windows x64; devtools 2.5.2, testthat 3.3.2.
Test/check processes use process-local `LANG`, `LC_ALL`, `LC_COLLATE`,
`LC_CTYPE`, `LC_MONETARY` and `LC_TIME` set to `C`, avoiding the inherited
Windows-invalid `C.UTF-8` locale noted in the re-check.

Scripts, complete logs, result RDS files, the original HEAD archive and the
check source/output are outside the repository at:

```text
C:/Users/DELL/AppData/Local/Temp/pkg-blorr-c25-fu2-20261119-190105/
```

Key artifacts: `runtests.R`, `head-regression.log`, `full-test.log`, their
result RDS files, `glm-probe.R`, `glm-probe.log`, `binary-grid.R`,
`grid-{head,fixed}.rds`, `grid-{head,fixed}.log`, `runcheck.R`, `check.log`,
and `check-output/`.
