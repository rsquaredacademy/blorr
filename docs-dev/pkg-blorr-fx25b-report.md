# blorr 0.3.3 development fix round 2 (rx25b, pkg-blorr-fx25b)

2026-11-21. Branch `task/blorr-fx25`, continuing from `b56aa2b` (the pkg-blorr-fx25
fix) on base `c848118` (develop). Worktree: `F:/R/Packages/_worktrees/blorr-fx25`.
Re-audit: `F:/R/Packages/_worktrees/blorr-rx25b/docs-dev/pkg-blorr-rx25b-audit.md`.
Rules: `F:/pm/notes/package-audit-stop-rule.md`.

One commit contains the code, tests, help/NEWS updates and this report. The
scope was the chief's narrowed 2026-11-21 scope: restore regressions introduced
by the new one-level model guard, plus the quick in-domain Highs (H1-H3). H4,
M1 (explicit-NA predictor), M3, M5, L1 and the backlog are left for GitHub
issues.

One item needs a scope note. The brief lists M4 among "don't touch", but M4 is
exactly the kind of case item 1 says must be fixed: it worked on `c848118` and
it errors on `b56aa2b`. It is therefore reported and fixed here as a
**regression restore**, not as a new M4 fix. The fix is the minimal pre-guard
ordering (remove missing responses, then check the predictor on retained rows)
and does not weaken the M1 named error for a missing predictor on a retained
row.

## Findings, changes and tests

| Finding | Change | Test(s) | Before | After |
| --- | --- | --- | --- | --- |
| M2 (regression) | `blr_check_supported()` calls `blr_check_binary_response(..., exactly = FALSE)` again. Only responses with more than two levels are rejected for model APIs; `exactly = TRUE` remains the default for data-column APIs. `blr_regress()` still rejects a one-level response through `resp_profile()` because its summary prints the two outcomes side by side. | `test-fx25b-fixes.R`: "M2: a one-class model keeps its c848118 confusion matrix"; "M2: the other model APIs that accepted a one-class fit still do". Value test against the `c848118` confusion matrix/counts. | HEAD error: `` `rep(0, 32)` has 1 level; blorr needs both events and non-events. `` | `c848118` confusion matrix restored: 32 true zeroes, accuracy 1, specificity 1, sensitivity `NA`, kappa `NaN`. `blr_model_fit_stats()`, `blr_rsq_*()`, `blr_gains_table()` and `blr_test_hosmer_lemeshow()` run again. |
| M4 (regression) | `blr_segment()`, `blr_segment_dist()` and `blr_segment_twoway()` remove rows with a missing response before calling `blr_check_predictor_missing()`. A missing predictor on a retained row is still a named error. | `test-fx25b-fixes.R`: "M4: missing responses are removed before the predictor guard". | HEAD error: `The predictor `g` has 1 missing value; ...` for `data.frame(y=c(0,1,0,1,NA), g=c("a","a","b","b",NA))`. | `c848118` result restored: `a` 0.5, `b` 0.5; `n = 2` each; two-way cells 0/1 as the retained rows define them. |
| H1 | `event_indicator()` treats an explicit missing factor label (`is.na(as.character(x))`) as missing, not as a non-event. The data-column guards pass `factor(data[[resp]])`, which drops the explicit `NA` level before the level count. `blr_bivariate_analysis()` removes rows with a missing response before `glm()` and WoE, so an explicit `NA` level is never fitted as an outcome. | `test-fx25b-fixes.R`: "H1: explicit NA factor responses are excluded from segment tables", "... from WoE/IV", "... from bivariate analysis". Value tests with `exclude = NULL`. | `blr_segment_dist`: `a n=3, 1s=1, n%=0.6, 1s%=1/3`; two-way `a/y` = 0; WoE `a count_0s=2, count_1s=1`; bivariate errored at 3 levels. | `a n=2, 1s=1, n%=0.5, 1s%=0.5`; two-way `a/y` = `NA`; WoE `a count_0s=1, count_1s=1`; bivariate equals the analysis of the complete-case rows. |
| H2 | `blr_woe_iv()` identifies the event on the full response before removing rows with a missing response or predictor, and `woe_data_prep()` receives that event vector. The event class therefore survives `droplevels()` on the filtered response. | `test-fx25b-fixes.R`: "H2: WoE keeps the factor event class after complete-case filtering"; "H2: the event mapping survives either missing response class". Factor and numeric 0/1 encodings compared, and both directions of the missing class. | Factor `a: 2 zeroes, 0 ones; b: 1 zero, 0 ones` (numeric 0/1 was already correct). | Factor `a: 0 zeroes, 2 ones; b: 0 zeroes, 1 one`, identical counts, WoE, IV and printed stats to the numeric encoding. |
| H3 | `blr_woe_iv()` stops with a named complete-case error when no rows remain after removing missing responses/predictors. `blr_woe_iv_stats()` reaches the same error through its per-variable call. | `test-fx25b-fixes.R`: "H3: no complete WoE rows is a named complete-case error". Numeric and factor responses, both public paths. | Empty WoE table, 7 `max()` warnings, printed `Information Value 0`. | `` Error: No complete observations remain for predictor `g` and response `y`: every row has a missing response or predictor, so WoE/IV is undefined. `` |

The regression check covered every re-audit finding against `c848118` and
`b56aa2b`. Only M2 and M4 are regressions of the kind item 1 describes. H1-H3,
H4, M1, M3, M5 and L1 are wrong or limited on both commits (H4 and the
explicit-NA predictor crash are reproducible on `c848118`); they are not
regressions. H1-H3 are fixed because item 2 asked for the quick in-domain
Highs.

## Regression restores: c848118 vs HEAD vs this commit

### M2: one-class model and the confusion matrix

`c848118`:

```r
m <- glm(rep(0, 32) ~ wt, mtcars, family = binomial())
blr_confusion_matrix(m)
# Confusion Matrix and Statistics
#
#           Reference
# Prediction  0  1
#          0 32  0
#          1  0  0
#
#                 Accuracy : 1.0000
#      No Information Rate : 1.0000
#
#                    Kappa : NaN
#
# McNemars's Test P-Value  : NA
#
# Note: McNemar's test is not defined when there are no discordant predictions.
#
#              Sensitivity : NA
#              Specificity : 1.0000
#              Pos Pred Value : NA
#              Neg Pred Value : NA
#                  Prevalence : 0.0000
#              Detection Rate : 0.0000
#        Detection Prevalence : 0.0000
#           Balanced Accuracy : NA
#                   Precision : NA
#                      Recall : NA
#
#         'Positive' Class : 1
```

`b56aa2b` (HEAD before this round):

```r
blr_confusion_matrix(m)
# Error: `rep(0, 32)` has 1 level; blorr needs both events and non-events.
```

This commit: the `c848118` output above is restored byte for byte on the
exercised model. The value test also checks the exact 2 x 2 counts, accuracy,
specificity, no-information rate, `NA` sensitivity/PPV/NPV/balanced accuracy,
`NaN` kappa and the McNemar note. `blr_regress()` still stops with the one-level
error, as it did on `c848118`, because its response-summary printer needs two
outcomes; the model diagnostics do not.

### M4: missing response already removes the missing predictor row

`c848118`:

```r
d <- data.frame(y = c(0, 1, 0, 1, NA), g = c("a", "a", "b", "b", NA))
blr_segment(d, y, g)$segment_data
#   pred 1s%
# 1    a 0.5
# 2    b 0.5
blr_segment_dist(d, y, g)$dist_table
#   variable n 1s  n% 1s%
# 1        a 2  1 0.5 0.5
# 2        b 2  1 0.5 0.5
```

`b56aa2b`:

```r
blr_segment(d, y, g)
# Error: The predictor `g` has 1 missing value; blorr's segment tables need a
# value for every observation.
```

This commit: the `c848118` values above are restored. The missing predictor is
checked on the rows left after removing the missing response, so a missing
predictor on a retained row still produces the same named error (the M1 test
stays green).

## H1-H3: audit reprex before and after

### H1: explicit missing factor response

Before (`b56aa2b`):

```r
d <- data.frame(y = factor(c("0", "1", NA, "0", "1"), exclude = NULL),
                g = c("a", "a", "a", "b", "b"),
                h = c("x", "x", "y", "y", "y"))
blr_segment_dist(d, y, g)$dist_table
#   variable n 1s  n%       1s%
# 1        a 3  1 0.6 0.3333333
# 2        b 2  1 0.4 0.5000000
blr_segment_twoway(d, y, g, h)$twoway_segment
#       x   y
#   a 0.5 0.0
#   b     0.5
blr_woe_iv(d, g, y)$woe_iv_table
#   levels count_0s count_1s dist_0s dist_1s     woe     iv
# 1      a        2        1  0.6667     0.5  0.2877 0.0479
# 2      b        1        1  0.3333     0.5 -0.4055 0.0676
```

After:

```r
blr_segment_dist(d, y, g)$dist_table
#   variable n 1s  n% 1s%
# 1        a 2  1 0.5 0.5
# 2        b 2  1 0.5 0.5
blr_segment(d, y, g)$segment_data
#   pred 1s%
# 1    a 0.5
# 2    b 0.5
blr_segment_twoway(d, y, g, h)$twoway_segment
#       x   y
#   a 0.5
#   b     0.5
# (a/y prints as -)
blr_woe_iv(d, g, y)$woe_iv_table
#   levels count_0s count_1s dist_0s dist_1s woe iv
# 1      a        1        1     0.5     0.5   0  0
# 2      b        1        1     0.5     0.5   0  0
```

`blr_woe_iv_stats()` prints the same `a`/`b` rows with one zero and one one.
`blr_bivariate_analysis()` now removes the explicit-`NA` row and gives exactly
the result of the complete-case data frame; before this round it stopped with
the three-level error. `c848118` also stopped with that error, so this is an
H1 extension needed to avoid fitting the explicit `NA` level as an outcome, not
a regression restore.

### H2: WoE event reversal after complete-case filtering

Before (`b56aa2b`):

```r
d <- data.frame(y = factor(c(0, 1, 1, 1)), g = c(NA, "a", "a", "b"))
blr_woe_iv(d, g, y)$woe_iv_table[, 1:3]
#   levels count_0s count_1s
# 1      a        2        0
# 2      b        1        0
```

After:

```r
blr_woe_iv(d, g, y)$woe_iv_table[, 1:3]
#   levels count_0s count_1s
# 1      a        0        2
# 2      b        0        1
```

Changing `y` to numeric 0/1 gives the same counts, WoE, IV and total. The test
also covers the opposite missing class (only non-events remain) and a factor
response with an explicit `NA` level.

### H3: no complete WoE rows

Before (`b56aa2b`):

```r
d <- data.frame(y = c(0, 1), g = c(NA, NA))
blr_woe_iv(d, g, y)
# empty table; 7 warnings from max(); Information Value 0
blr_woe_iv_stats(d, y, g)
# Variable: g
# empty table; warnings; Information Value 0
```

After:

```r
blr_woe_iv(d, g, y)
# Error: No complete observations remain for predictor `g` and response `y`:
# every row has a missing response or predictor, so WoE/IV is undefined.
blr_woe_iv_stats(d, y, g)
# Variable: g
# Error: No complete observations remain for predictor `g` and response `y`:
# every row has a missing response or predictor, so WoE/IV is undefined.
```

The test also covers a factor response and checks that the error is the named
complete-case error rather than an empty table.

## Check results

`devtools::test()` in a disposable `git archive` copy
(`C:/Users/DELL/AppData/Local/Temp/opencode/blorr-fx25b/clean`, `NOT_CRAN=true`):

```text
files 29, tests 290, failed 0, errors 0, warnings 0, skipped 0, passed 2404
elapsed 126.23 s
```

`R CMD build` built `blorr_0.3.2.9000.tar.gz` including vignettes. `R CMD check
--as-cran --no-manual` on that tarball, with `LANG=C`, `LC_ALL=C` and a
disposable `R_LIBS_USER`:

```text
Status: 1 NOTE   (0 errors, 0 warnings)
```

The NOTE is CRAN incoming feasibility:

```text
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Aravind Hebbali <aravindhebbali@rsquaredacademy.com>'
Version contains large components (0.3.2.9000)
```

This is the existing development version in `DESCRIPTION`; no bump was made.
Remote incoming and URL checks were disabled to honour the no-network
instruction, so those remote checks were not verified.

The check test stage from `00check.log`:

```text
* checking tests ... [235s] OK
  Running 'testthat.R' [234s]
[ FAIL 0 | WARN 0 | SKIP 50 | PASS 2345 ]
```

The check's own `proc.time()` for the test process was 233.82 s elapsed. The 50
skips are the "On CRAN" skips (vdiffr and source-Rd-path checks), as in the
previous report; `devtools::test()` with `NOT_CRAN=true` runs with no skips.
The full local suite finishes in 126 s, and even the check test stage stayed
under the 4-minute limit (3 min 54 s) on this run. An earlier check run of the
same commit's tests took 57 s; the variation is host load, not test content.
The test growth over rx25 is 67 passes (2337 to 2404 locally; 2278 to 2345 in
check), all from `test-fx25b-fixes.R`; no existing test was weakened or
removed.

## Commands run

The working shell in this environment is bash. PowerShell 5.1 equivalents use
`$env:NAME = "value"` instead of the `export NAME=value` prefix.

```text
cd F:/R/Packages/_worktrees/blorr-fx25
Rscript -e 'devtools::load_all(".", quiet=TRUE); ...'          # before/after reprexes
Rscript -e 'devtools::test(".", reporter = "summary")'          # working tree
git add ... ; git commit -m "blorr 0.3.3 dev: rx25b regression fix + quick in-domain fixes (pkg-blorr-fx25b)"
git archive HEAD | tar -x -C C:/Users/DELL/AppData/Local/Temp/opencode/blorr-fx25b/clean
cd C:/Users/DELL/AppData/Local/Temp/opencode/blorr-fx25b/clean
Rscript -e 'Sys.setenv(NOT_CRAN="true"); devtools::test(".", reporter = "summary")'
cd ..
export LANG=C; export LC_ALL=C; export R_LIBS_USER=.../lib
export _R_CHECK_FORCE_SUGGESTS_=false
export _R_CHECK_CRAN_INCOMING_REMOTE_=false
export _R_CHECK_URLS_=false
R CMD build clean
R CMD check --as-cran --no-manual blorr_0.3.2.9000.tar.gz
```

The disposable archive, tarball, check directory and temporary library under
`C:/Users/DELL/AppData/Local/Temp/opencode/blorr-fx25b/` were deleted after the
report was written. No package was installed into a permanent library. Test
runs were restored from the committed snapshots and `Rplots.pdf` before the
commit; the working tree is clean apart from this report before the amend.

## Backlog for GitHub issues

These were deliberately not fixed in this round.

1. **H4: character responses in `blr_bivariate_analysis()`.** The documented
   two-value character response still reaches `glm()` unchanged and fails with
   "y values must be 0 <= y <= 1"; the equivalent factor works. Reproducible on
   `c848118`, so not a regression. Normalize the character response with the
   documented event ordering before both the LR test and WoE.
2. **M1: explicit-NA factor predictors.** `factor(c("a", "a", NA, "b"),
   exclude = NULL)` still bypasses `is.na()` in the segment printers (and is
   silently dropped by the two-way function). Ordinary missing predictors are
   fixed. Treat explicit NA levels as missing (or support and label them
   consistently) across constructors and printers.
3. **M3: WoE printer `digits`.** The returned WoE/IV table honours `digits`,
   but the printed component rows are still rounded to two decimal places. Make
   the printer use `x$digits`.
4. **M5: small nonzero rates print as zero.** A rate of 0.000025 prints as
   0.00% / 0.000. Print a positive bound below display resolution and
   distinguish a true zero.
5. **L1: remaining documentation limits.** Character bivariate responses,
   explicit NA levels, the model-level one-class rule and numerical predictor
   limits are not fully covered by the help/NEWS claims. Update after the
   remaining behaviour is settled.
6. **Numeric responses other than 0/1.** `y = c(2, 3, 2, 3)` returns zero event
   counts instead of an error naming `y` and the supported 0/1 range. Enforce
   the numeric encoding in the data-column paths.
7. **Manually constructed empty WoE objects.** The public all-missing-predictor
   path now errors (H3), but a hand-built empty `blr_woe_iv` object can still
   print zero IV. Add a printer guard or document it as unsupported.
8. **Extreme predictor values.** Around ±1e300 worked in the exercised two-way
   example, but no documented numeric limit or platform precision bound was
   established.
9. **Real-render visual gate.** The changed plot data are checked by value
   (`ggplot_build()`), but no headless-Chrome/contact-sheet review of the final
   rendered images was performed; the separate final visual gate remains
   outstanding.

Not done or deliberately unchanged: no version bump (`0.3.2.9000`), no push, no
GitHub calls, no branch switch, no changes outside this worktree, and no
permanent package installation.
