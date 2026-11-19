pkg-blorr-c25-rx2: independent re-check of follow-up 2 (DeepSeek)

2026-11-19. Audited HEAD `2cf3907854b969ce54c2fdf6973a96910be064d7`
("blorr: binary-response check for data-column APIs (pkg-blorr-c25-fu2)")
on branch `task/blorr-c25`. Parent `HEAD^` =
`3f154f81adc4ae3c11d2829c25acf38cdffc844c` (the follow-up's starting
HEAD), pre-C25 develop baseline `bc76c35`. Read
`F:/pm/notes/package-audit-stop-rule.md`,
`docs-dev/pkg-blorr-c25-rx-audit.md` (L1, L2) and
`docs-dev/pkg-blorr-c25-fu2-report.md`; nothing in the follow-up report was
taken on trust. Only this report is added to the repository. All archives,
R scripts, RDS files and check output are outside the worktree at
`C:/Users/DELL/AppData/Local/Temp/pkg-blorr-c25-rx2-20261119-204229/`.
No package code, test, help, NEWS or other file was changed.

The supported domain remains unweighted Bernoulli binomial/logit models and
their stored model frame/response. A response column with three observed
levels is outside that domain; per the stop rule it must be rejected rather
than silently summarized. Ordinary binary responses (numeric 0/1, two-level
factor, logical, two-value character, unused factor levels, missing values)
must keep their previous results.

| Item | Verdict | Summary |
| --- | --- | --- |
| L2, data-column APIs silently choose level 2 | **FIXED** | All six exported functions with a `response` formal, including the five fixed APIs, now stop with the package's binary-response message on `hsb2$ses`. The five silently computed on the parent. |
| Binary responses unaffected | **CONFIRMED** | 240 calls (10 datasets x 4 encodings x 6 APIs) are identical to the parent commit at `all.equal(..., tolerance = 0)`, including errors, warnings, messages, printed output and returned values. |
| L1, NEWS accuracy | **FIXED** | The revised bullet matches executed `glm()` and pre-C25 `blr_regress()` behavior, including the factor recode, the character error and the 47/95 response summary. |
| Full tests and check | **PASS** | `devtools::test()`: 27 files, 256 test blocks, 2066 expectations, 0 failed/error/skipped/warning. `devtools::check()`: 0 errors, 0 warnings, 0 notes, Status OK. |

## 1. L2: three-level response rejection

The follow-up added `blr_check_binary_response(factor(data[[resp]]), resp,
exactly = FALSE)` to `blr_woe_iv()`, `blr_woe_iv_stats()`, `blr_segment()`,
`blr_segment_dist()` and `blr_segment_twoway()`. It also updated the five
help pages and added `tests/testthat/test-data-column-response.R`.

I enumerated the namespace rather than relying on the report. Exactly six
exported functions have a formal named `response`:

```text
blr_bivariate_analysis(data, response, ...)
blr_segment(data, response, predictor)
blr_segment_dist(data, response, predictor)
blr_segment_twoway(data, response, variable_1, variable_2)
blr_woe_iv(data, predictor, response, digits, ...)
blr_woe_iv_stats(data, response, ...)
```

With `d <- hsb2; d$ses3 <- d$ses` (200 observations, `table(d$ses3)` =
47/95/58), every one of the six stops on HEAD with the package message and
the correct response name:

```text
blr_bivariate_analysis : The response `ses3` has 3 levels; blorr supports a binary response with exactly two levels.
blr_woe_iv             : The response `ses3` has 3 levels; blorr supports a binary response with exactly two levels.
blr_woe_iv_stats       : The response `ses3` has 3 levels; blorr supports a binary response with exactly two levels.
blr_segment            : The response `ses3` has 3 levels; blorr supports a binary response with exactly two levels.
blr_segment_dist       : The response `ses3` has 3 levels; blorr supports a binary response with exactly two levels.
blr_segment_twoway     : The response `ses3` has 3 levels; blorr supports a binary response with exactly two levels.
```

The same five calls directly on `hsb2$ses` name `ses` and give the same
message. This is exactly the error text of
`blr_check_binary_response()` (`R/blr-utils.R:91`).

The parent tree gives the expected before/after contrast. There,
`blr_woe_iv(d, female, ses3)` returns WoE/IV 0.0232, `blr_segment()` returns
the 52%/44% one-event rates and `blr_segment_dist()` the 91/109 split, all
without warning; `blr_woe_iv_stats()` and `blr_segment_twoway()` likewise
return the level-2-versus-rest table. `blr_bivariate_analysis()` alone
already errored on the parent through its model-side LR calculation. On HEAD
all six error before returning any result.

Model-based entry points were swept as well, using
`m <- glm(ses ~ female + read, data = hsb2, family = binomial())`. All 27
probed APIs reject the three-level fit with the same binary-response
message, including `blr_regress()`, `blr_model_fit_stats()`, all eight
`blr_rsq_*()` helpers, all six stepwise functions, `blr_gains_table()`,
`blr_confusion_matrix()`, `blr_test_hosmer_lemeshow()`, `blr_test_lr()`,
`blr_pairs()`, `blr_coll_diag()`, `blr_linktest()`, `blr_gini_index()`,
`blr_lorenz_curve()`, `blr_residual_diagnostics()` and
`blr_prep_lorenz_data()`.

Behavior on the documented edge is preserved: an unused third factor level
is ignored (no error), and a single-outcome response is still accepted by
these data-column APIs because they call with `exactly = FALSE`. Both were
executed on HEAD; the single-outcome calls complete without error.

L2 is closed. The honesty exception in the stop rule no longer applies.

## 2. Binary responses unchanged versus the parent

I archived `HEAD` and `HEAD^` with `git archive` into separate trees and ran
the same grid in separate Rscript processes. The grid has ten ordinary
datasets: `hsb2` with `honcomp`; `hsb2` with `ses == 3`; `mtcars` with `am`;
`mtcars` with `gear > 3`; the first 1500 rows of `bank_marketing`; and five
deterministic synthetic frames covering balanced, unbalanced, missing
numeric response, missing factor response and a small 24-row sample.

Each dataset was run under four response encodings - numeric 0/1, two-level
factor, logical and two-value character - through all six exported
response-column APIs. Each call captured the error, warnings, messages,
output produced by the call, the printed representation, and the returned
object. Comparison used exact identity for text and
`all.equal(..., tolerance = 0, check.environment = FALSE)` for values.

```text
CALL COUNT: 240  IDENTICAL: 240  DIFFERING: 0
BY ENCODING: numeric01 60/60, factor2 60/60, logical 60/60, char2 60/60
BY API: bivariate 40/40, woe_iv 40/40, woe_iv_stats 40/40,
        segment 40/40, segment_dist 40/40, segment_twoway 40/40
ERRORS LEFT: 10  ERRORS RIGHT: 10
```

The five fixed data-column APIs produce no errors across all 200 of their
calls and are identical for every encoding. The ten errors are all
`blr_bivariate_analysis()` on a character response (`y values must be
0 <= y <= 1`), a pre-existing `glm()` limitation that occurs identically on
the parent and on HEAD; it is not a new rejection introduced by the
follow-up. No ordinary binary response changed in any captured result,
message or warning.

## 3. L1: NEWS versus executed `glm()` behavior

The revised NEWS bullet (`NEWS.md:5-20`) states that a three-level factor is
silently recoded by `glm()` as first level versus everything else, that a
character response fails instead, and that the old `blr_regress()` summary
printed 47 and 95 of 200 observations while dropping the third level (58).
Executed on both HEAD and the parent:

```r
d <- hsb2
d$char3 <- as.character(d$ses)
glm(char3 ~ female + read, data = d, family = binomial())
# Error: y values must be 0 <= y <= 1

mf <- glm(ses ~ female + read, data = d, family = binomial())
table(mf$y)
#   0   1
#  47 153
table(mf$model[[1]])
#  1  2  3
# 47 95 58
identical(as.integer(mf$y), as.integer(d$ses != "1"))
# TRUE
```

The recode is therefore exactly "level 1" versus "levels 2 and 3", and the
character claim is correct. The 47/95 historical claim was checked on the
pre-C25 tree `bc76c35`, where `blr_regress(ses ~ female + read, data = d)`
builds and its printed Response Summary is:

```text
Outcome        Frequency        Outcome        Frequency
   1              47               2              95
Obs. 200
```

On HEAD the same formula now stops with
``The response `ses` has 3 levels; ...``. The single-outcome sentence was
also reproduced: on `bc76c35`, `d$zero <- 0;
blr_regress(zero ~ read, data = d)` builds the object and printing fails
with `subscript out of bounds`; on HEAD it stops with
``The response `zero` has 1 level; blorr supports a binary response with
exactly two levels.`` The NEWS bullet matches the executed behavior; L1 is
closed.

## 4. Full test suite and package check

Both were run on a fresh `git archive` of HEAD outside the worktree, with
`LANG`/`LC_*` set to `C` in the R process. No package file was edited.

`devtools::test(path, reporter = "summary")`:

```text
TOTALS: files = 27 tests (test_that blocks) = 256 expectations = 2066
        failed = 0 error = 0 skipped = 0 warning = 0
        passed expectations = 2066
```

The fu2 report's "Tests 256" is the number of `test_that()` blocks; its 2066
passed expectations agrees with this independent run.

`devtools::check(path, document = FALSE, args = "--no-manual",
check_dir = ..., error_on = "never")` retained devtools' default
`--as-cran`; no check stage was removed. It completed in 6m 35s:

```text
* checking examples ... [18s] OK
* checking examples with --run-donttest ... [51s] OK
* checking tests ... [93s] OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... [37s] OK
* DONE
Status: OK
CHECK_TOTALS: errors = 0 warnings = 0 notes = 0
```

Inside `R CMD check`, testthat reports
`[ FAIL 0 | WARN 0 | SKIP 2 | PASS 2063 ]`. The two unchanged skips are
`test-gini-index.R:83` and `test-stepwise-selection.R:62`: installed packages
do not contain the raw `man/*.Rd` paths those tests inspect. Their three
expectations pass in the source-tree run, which explains 2066 versus 2063.

Environment: R 4.6.1 (2026-06-24 ucrt), Windows 11 x64, devtools 2.5.2,
testthat 3.3.2, pkgload 1.5.3.

## Reproduction

All R invocations used script files, not `Rscript -e`. Representative
PowerShell 5.1 commands (paths abbreviated with `$S`):

```powershell
$S = "C:/Users/DELL/AppData/Local/Temp/pkg-blorr-c25-rx2-20261119-204229"
git archive --format=tar -o "$S/head.tar" HEAD
git archive --format=tar -o "$S/parent.tar" HEAD^
git archive --format=tar -o "$S/base.tar" bc76c35
tar --force-local -xf "$S/head.tar" -C "$S/head"
tar --force-local -xf "$S/parent.tar" -C "$S/parent"
tar --force-local -xf "$S/base.tar" -C "$S/base"
Rscript "$S/probe_l2.R" "$S/head" "$S/probe-l2-head.log"
Rscript "$S/probe_l2.R" "$S/parent" "$S/probe-l2-parent.log"
Rscript "$S/model_sweep.R" "$S/head" "$S/model-sweep-head.log"
Rscript "$S/grid.R" "$S/head" "$S/grid-head.rds"
Rscript "$S/grid.R" "$S/parent" "$S/grid-parent.rds"
Rscript "$S/compare.R" "$S/grid-parent.rds" "$S/grid-head.rds" "$S/compare-parent-head.log"
Rscript "$S/news_probe.R" "$S/head" "$S/news-head.log"
Rscript "$S/news_probe.R" "$S/base" "$S/news-base.log"
Rscript "$S/runtests.R" "$S/head" "$S/full-test.log"
Rscript "$S/runcheck.R" "$S/head" "$S/check.log" "$S/check-out"
```

Key artifacts in `$S`: `probe-l2-{head,parent}.log`, `model-sweep-head.log`,
`grid-{head,parent}.rds`, `compare-parent-head.log`, `news-{head,base}.log`,
`full-test.log`/`full-test.rds`, `check.log`/`check.rds`, and
`check-out/blorr.Rcheck/`.

No package code was changed, no branch was switched, nothing was pushed, and
GitHub/`gh` was not used.

Ready to merge into develop: yes
