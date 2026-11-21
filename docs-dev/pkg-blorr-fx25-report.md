# blorr 0.3.3 development fix round (rx25, pkg-blorr-fx25)

2026-11-21. Branch `task/blorr-fx25`, base `c848118` (develop). One commit on
top of the base contains the fix, tests, help pages, NEWS and this report.
Worktree: `F:/R/Packages/_worktrees/blorr-fx25`. The audit is
`F:/R/Packages/_worktrees/blorr-rx25/docs-dev/pkg-blorr-rx25-audit.md`; its
reprexes were rerun against the `v0.3.2` tag for the "before" column and against
this commit for the "after" column.

## Findings, changes and tests

| Finding | Change | Test(s) | Status |
| --- | --- | --- | --- |
| H1 two-way denominator | `blr_segment_twoway()` now divides the events in each level pair by the non-missing responses in that same pair, not by all non-missing responses in the sample. Empty cells are `NA` and print as `-`. Levels are taken from the non-missing response rows, so unused levels are ignored and the event and denominator tables always have the same shape. | `test-ordinary-grid.R`: "H1: two-way cells are events divided by responses in the cell"; the grid blocks "mtcars/hsb2/bank_marketing two-way rates are within-cell event rates". `test-blbl.R` and `test-bivariate-analysis.R` expectations updated. | Fixed |
| H2 rounded proportions | `blr_segment()` and `blr_segment_dist()` return unrounded `1s%` and `n%`. Printing multiplies by 100 and rounds to two decimal places; `plot.blr_segment_dist()` uses the unrounded values. M5 test expectations changed from 34.00/73.00 to 34.38/72.73. | `test-ordinary-grid.R`: "H2: proportions stay unrounded and only printing rounds (M5 tests)" and "H2: the segment distribution plot uses the unrounded values"; `test-blbl.R` row expectations; `test-bivariate-analysis.R` event-rate expectations. | Fixed |
| H3 one-level response | `blr_check_binary_response()` in `R/blr-utils.R` now applies one rule to numeric, factor, logical and character responses: fewer than two observed levels is an error, `` `y` has 1 level; blorr needs both events and non-events. `` More than two levels keeps the existing named error. The five data-column help pages now say "exactly two levels". The shared model guard also uses this rule; the McNemar no-discordance test was changed to a two-class data set. | `test-ordinary-grid.R`: "H3: one-level numeric and factor responses are both rejected"; `test-data-column-response.R` (three-level coverage); `test-model-validation.R` (no-discordance test now has two classes). | Fixed |
| H4 total IV | The total information value is computed from the full-precision IV components and rounded once by the printer. `blr_woe_iv()` stores `iv_total` (unrounded) and `digits`; `blr_woe_iv_stats()` and `blr_bivariate_analysis()` use the same total. The returned WoE/IV table components keep the documented `digits` rounding. | `test-ordinary-grid.R`: "H4: total IV is rounded once from full-precision components"; the grid WoE/IV and bivariate blocks compare every total with the base-R sum. `test-bivariate-analysis.R` bivariate IV expectation updated to the full-precision total. | Fixed |
| M1 missing predictor | `blr_segment()`, `blr_segment_dist()` and `blr_segment_twoway()` reject a missing predictor at construction with an error naming the column, instead of failing inside the printer with `invalid 'width' argument`. | `test-ordinary-grid.R`: "M1: a missing predictor is a named error, not a printer crash". | Fixed |
| M2 no-event/empty two-way tables | No-event cells in a non-empty table are zero rates. A one-level response is now rejected by H3, so the audit's all-zero reprex stops before printing. The two-way printer also returns a clear line for a manually constructed empty table instead of failing in `max()`/`rep()`. Empty input reaches the H3 zero-level error. | `test-ordinary-grid.R`: "M2: no-event cells are zero and empty tables print without a crash". | Fixed |
| L1 supported domain and precision | The five data-column help pages now state exactly two response levels, the supported response encodings (numeric 0/1, logical, two-level factor/character), the event level, unused-level and missing-value handling, the missing-predictor rule, the two-way within-cell definition and the printing precision. NEWS has a "Supported inputs and precision" section with the numeric limit and output precision. | Documentation; verified by `R CMD check` Rd/code-documentation checks. | Fixed |

The regression grid is `tests/testthat/test-ordinary-grid.R`. It covers
`blr_segment()`, `blr_segment_dist()`, `blr_segment_twoway()`, `blr_woe_iv()`,
`blr_woe_iv_stats()`, `blr_bivariate_analysis()` and the
`plot.blr_segment_dist()` data on ordinary `mtcars`, `hsb2` and
`bank_marketing` inputs. Each value is computed independently in base R from
the response and predictor columns (counts, within-cell rates, sample shares,
WoE/IV with the documented 0.5 correction, and `anova()` likelihood-ratio
statistics). There are also direct value tests for H1-H4, M1 and M2; no new
test is snapshot-only.

## Before and after: audit reprexes

### H1: two-way event rate

Before (`v0.3.2`; audit HEAD `c848118` has the same values):

```r
d <- transform(mtcars, cyl = factor(cyl), gear = factor(gear))
blr_segment_twoway(d, am, cyl, gear)$twoway_segment["4", "4"]
# [1] 0.1875        printed 0.188

blr_segment_twoway(d, am, cyl, gear)
#                      cyl
# --------------------------------
#  am       3       4       5
# --------------------------------
#   4     0.000   0.188   0.062
# --------------------------------
#   6     0.000   0.062   0.031
# --------------------------------
#   8     0.000   0.000   0.062
# --------------------------------
```

After:

```r
blr_segment_twoway(d, am, cyl, gear)$twoway_segment["4", "4"]
# [1] 0.75

blr_segment_twoway(d, am, cyl, gear)$twoway_segment["8", "4"]
# [1] NA

blr_segment_twoway(d, am, cyl, gear)
#                      gear
# --------------------------------
#  cyl      3       4       5
# --------------------------------
#   4     0.000   0.750   1.000
# --------------------------------
#   6     0.000   0.500   1.000
# --------------------------------
#   8     0.000     -     1.000
# --------------------------------
```

The cyl 4 / gear 4 cell is 6 events out of 8 observations, so 0.750, matching
`with(d, mean(am[cyl == "4" & gear == "4"]))`.

### H2: percentage output and plot data

Before (audit HEAD `c848118`; the `v0.3.2` tag printed the proportions
0.34/0.73 instead):

```r
d <- transform(mtcars, cyl = factor(cyl))
blr_segment_dist(d, am, cyl)
#        Event Segmentation
# -----------------------------------
# cyl      n     1s     n%       1s%
# -----------------------------------
#  4      11     8     34.00    73.00
#  6      7     3     22.00    43.00
#  8      14     2     44.00    14.00
# -----------------------------------

blr_segment(d, am, cyl)
# Event By Attributes
# --------------------
# pred            1s%
# --------------------
#  4             73.00
#  6             43.00
#  8             14.00
# --------------------
```

After:

```r
blr_segment_dist(d, am, cyl)$dist_table
#   variable  n 1s      n%       1s%
# 2        4 11  8 0.34375 0.7272727
# 1        6  7  3 0.21875 0.4285714
# 3        8 14  2 0.43750 0.1428571

blr_segment_dist(d, am, cyl)
#        Event Segmentation
# ---------------------------------
# cyl    n     1s     n%       1s%
# ---------------------------------
#  4     11    8     34.38    72.73
#  6     7     3     21.88    42.86
#  8     14    2     43.75    14.29
# ---------------------------------

blr_segment(d, am, cyl)
# Event By Attributes
# --------------------
# pred            1s%
# --------------------
#  4             72.73
#  6             42.86
#  8             14.29
# --------------------
```

The plot data now carry 0.34375, 0.21875 and 0.4375 for `n%` and
0.7272727, 0.4285714 and 0.1428571 for `1s%`, instead of the stored rounded
0.34/0.22/0.44 and 0.73/0.43/0.14.

### H3: one-level response

Before:

```r
d <- data.frame(y = c(1, 1, 1, 1), g = c("a", "a", "b", "b"))
blr_segment(d, y, g)$segment_data
#   pred 1s%
# 1    a   1
# 2    b   1

d$y <- factor(d$y)
blr_segment(d, y, g)$segment_data
#   pred 1s%
# 1    a   0
# 2    b   0
```

After (the same error for a numeric or factor response):

```r
blr_segment(d, y, g)
# Error: `y` has 1 level; blorr needs both events and non-events.
```

`blr_segment_dist()`, `blr_segment_twoway()`, `blr_woe_iv()` and
`blr_woe_iv_stats()` return the same error.

### H4: total information value

Before:

```r
d <- data.frame(y = c(0, 1, 0, 1, 0, 1), g = c("a", "a", "b", "b", "a", "b"))
blr_woe_iv(d, g, y, digits = 1)
#       Information Value
# -----------------------------
# Variable    Information Value
# -----------------------------
#    g               0.4
# -----------------------------
```

After:

```r
k <- blr_woe_iv(d, g, y, digits = 1)
k$iv_total
# [1] 0.4620981
sum(k$woe_iv_table$iv)   # rounded table components, kept for the table
# [1] 0.4

print(k)
#       Information Value
# -----------------------------
# Variable    Information Value
# -----------------------------
#    g               0.5
# -----------------------------
```

The exact total is `2 * (2/3 - 1/3) * log(2) = 0.4620981`, which rounds to 0.5
at one digit. For `hsb2` and `prog` the default-`digits` total changes from the
old sum of rounded components (0.433) to `round(0.4329500, 4) = 0.4329`.

### M1: missing predictor

Before:

```r
d <- data.frame(y = c(0, 1, 0, 1), g = c("a", "a", NA, "b"))
print(blr_segment(d, y, g))
# Error in ... : invalid 'width' argument
print(blr_segment_dist(d, y, g))
# Error in ... : invalid 'width' argument
```

After:

```r
blr_segment(d, y, g)
# Error: The predictor `g` has 1 missing value; blorr's segment tables need a
# value for every observation.

blr_segment_dist(d, y, g)
# Error: The predictor `g` has 1 missing value; blorr's segment tables need a
# value for every observation.
```

`blr_segment_twoway()` names whichever grouping column has the missing value.

### M2: no-event and empty two-way tables

Before:

```r
d <- data.frame(y = c(0, 0, 0, 0), g = c("a", "a", "b", "b"),
                h = c("x", "y", "x", "y"))
print(blr_segment_twoway(d, y, g, h))
# Warning: In max(nchar(k)) : no non-missing arguments to max; returning -Inf
# Error in rep("-", w) : invalid 'times' argument
```

After:

```r
print(blr_segment_twoway(d, y, g, h))
# Error: `y` has 1 level; blorr needs both events and non-events.

empty <- data.frame(y = numeric(0), g = character(0), h = character(0))
print(blr_segment_twoway(empty, y, g, h))
# Error: `y` has 0 levels; blorr needs both events and non-events.

# a manually constructed empty table also prints without a crash
x <- structure(list(twoway_segment = matrix(numeric(0), 0, 0),
                    varnames = c("y", "g", "h")),
               class = "blr_segment_twoway")
print(x)
# No non-missing observations to summarise.
```

A no-event cell in a table with both response classes is a zero rate; for
example `blr_segment_twoway(hsb2, honcomp, prog, race)` has
`twoway_segment["1", "1"] == 0` and prints `0.000`.

### L1: supported domain and precision

Before, each of `man/blr_segment.Rd`, `blr_segment_dist.Rd`,
`blr_segment_twoway.Rd`, `blr_woe_iv.Rd` and `blr_woe_iv_stats.Rd` said only:

```text
\item{response}{Response variable; column in \code{data} with at most two
observed, non-missing levels. Unused factor levels are ignored.}
```

After, the help pages say "exactly two observed, non-missing levels", name the
event encoding (second factor/character level, `TRUE`, numeric 1), state that
numeric responses must be 0/1, and describe missing responses, missing
predictors, empty input, the two-way within-cell definition and the precision.
NEWS has a "Supported inputs and precision" section with the same contract:

```text
The data-column functions require exactly two observed, non-missing response
levels with both events and non-events. Supported encodings are numeric 0/1,
logical, a two-level factor and a two-value character vector; the event is the
second level, TRUE or 1. Unused factor levels and rows with a missing response
are ignored. Numeric codes other than 0/1 are not supported. Segment and two-way
functions reject a missing predictor with an error naming the column; WoE/IV
excludes rows with a missing response or predictor. Empty input is an error
because it has no response levels.

Proportions are returned unrounded. Segment tables print percentages to two
decimal places, two-way rates to three decimal places, WoE/IV components to
digits decimal places, and the total information value is rounded once at print
time. Plots use the unrounded proportions.
```

## Check results

`devtools::test()` in the disposable archive copy
(`.pkg-blorr-fx25-tmp/clean`):

```text
files 28, tests 281, failed 0, errors 0, warnings 0, skipped 0, passed 2337
```

`R CMD build` built `blorr_0.3.2.9000.tar.gz` including vignettes. `R CMD check
--as-cran --no-manual` on that tarball:

```text
Status: 1 NOTE   (0 errors, 0 warnings)
```

The only NOTE is CRAN incoming feasibility:

```text
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Aravind Hebbali <aravindhebbali@rsquaredacademy.com>'
Version contains large components (0.3.2.9000)
```

This is the development version already in `DESCRIPTION`; no version bump was
made. Remote incoming and URL checks were disabled to honour the no-network
instruction, so those remote checks were not verified. The check's test stage
reported:

```text
[ FAIL 0 | WARN 0 | SKIP 50 | PASS 2278 ]
```

The 50 skips are the "On CRAN" skips (vdiffr and source-Rd-path checks) that do
not run in `R CMD check` without `NOT_CRAN`; the same suite ran with no skips in
`devtools::test()` above. There were no errors and no failures in either run.

## Commands run

The working shell in this environment is bash; the commands are shown as run.
PowerShell 5.1 equivalents use `$env:NAME = "value"` instead of the inline
`NAME=value` prefix.

```text
cd F:/R/Packages/_worktrees/blorr-fx25
git archive HEAD | tar -x -C .pkg-blorr-fx25-tmp/clean
cd .pkg-blorr-fx25-tmp/clean
Rscript -e 'devtools::test(".", reporter = "summary")'
Rscript -e 'res <- devtools::test(".", reporter = "silent"); df <- as.data.frame(res); ...'
cd ..
R CMD build clean
_R_CHECK_FORCE_SUGGESTS_=false _R_CHECK_CRAN_INCOMING_REMOTE_=false _R_CHECK_URLS_=false R CMD check --as-cran --no-manual blorr_0.3.2.9000.tar.gz
Rscript .pkg-blorr-fx25-tmp/before.R    # v0.3.2 tag, load_all
Rscript .pkg-blorr-fx25-tmp/after.R     # this commit, load_all
```

The before/after scripts were run with `devtools::load_all()` against the
`v0.3.2` archive and this commit respectively. No package was installed into a
permanent library. The disposable directory `.pkg-blorr-fx25-tmp/` and the
built tarball were deleted after the report was written; `tests/testthat/Rplots.pdf`
and snapshot line-ending changes produced by test runs were restored to the
committed versions.

## Follow-ups and out-of-domain backlog

These were not fixed in this round; they are listed for GitHub issues as
requested.

1. **Numeric responses other than 0/1.** `d <- data.frame(y = c(2, 3, 2, 3),
   g = c("a", "a", "b", "b"))`; `blr_segment_dist(d, y, g)` reports zero 1s in
   both groups, while converting `y` to a factor reports one event out of two
   per group. The help now states that numeric responses must be 0/1, so this
   is outside the documented domain, but the functions still return a number
   instead of a clear error. A follow-up should either define the event mapping
   for numeric codes or reject non-0/1 numeric responses with an error naming
   the argument and the supported values.
2. **Empty WoE/IV tables.** A zero-level response now stops in the shared
   binary-response guard with a clear error, so the public `blr_woe_iv()` path
   no longer prints IV 0. A manually constructed empty WoE object can still
   reach the printer; total IV is undefined on no observations. A follow-up
   should add an explicit printer guard or document empty input as unsupported
   for that object.
3. **Extreme predictor values.** The audit grid included predictors around
   ±1e300. No numeric limit is stated for predictors, and no guard was added;
   these remain exotic/out-of-domain inputs for a future backlog item.
4. **Real-render visual gate.** The changed `plot.blr_segment_dist()` data were
   checked by value (`ggplot_build()` in `test-ordinary-grid.R`) and the vdiffr
   snapshot was updated, but no headless-Chrome/contact-sheet review was
   performed in this round. The separate final real-render gate remains
   outstanding, as the stop rule requires for visual output.

Not done or deliberately unchanged:

- `DESCRIPTION` remains `Version: 0.3.2.9000`; no bump was requested.
- The package's roxygen note is 7.3.2 while the installed roxygen2 is 8.1.1.
  To avoid unrelated generated-file churn, the five help pages were edited by
  hand in both the roxygen comments and `man/*.Rd`; `R CMD check` finds no
  code/documentation mismatch.
- No GitHub issues were created and nothing was pushed (per instructions).
- The vdiffr snapshot for the segment distribution plot and the text snapshots
  for `blr_segment()`, `blr_segment_dist()`, `blr_segment_twoway()` and
  `blr_woe_iv()` were updated for the new values; these are existing
  snapshots, not the new regression evidence.
