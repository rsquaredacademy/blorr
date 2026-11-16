# bfx7: blorr fix round 7 (BFXX6-1, BFXX6-2, BFXX6-3)

Date: 2026-11-02. Worktree `F:/R/Packages/statistics/blorr-wt/fix-032`,
branch `task/blorr-fix-032`, base `567e246`. Audit fixed:
`docs-dev/bfxx6-reaudit.md` (left untracked with `.bfxx6/` and `build.log`,
untouched). Environment: Windows, R 4.6.1, testthat 3.3.2.

## Files changed

- `R/blr-utils.R`
  - BFXX6-1, restore check: the restored frame is now checked against the
    refit's **effective prior weights**. That is the frame's case weights
    (1 if none) times the binomial trials for a grouped response. Before,
    it was checked against the case weights rebuilt by
    `selection_weights()`, which are 0 for a zero-trial group, so valid
    zero-trial fits failed restoration.
  - BFXX6-1, fallback: `selection_restore()` no longer returns the
    stored-frame refit when restoration fails. That object names stored
    columns in its formula and carries the training offset and weight
    vectors in its call, so `predict(newdata)` could fail or silently
    reuse training offsets. Selection now stops with a clear error
    ("variable selection could not rebuild the selected model from the
    terms of the original model (...)"). So every returned model either
    passed the exact design/offset/prior-weight checks or was not
    returned. I found no input that reaches this error (see the
    weighted/grouped check below).
  - BFXX6-2: new `fitted_rows()` (positive prior weight) and
    `fitted_design()` (model-matrix columns with a non-aliased coefficient,
    on those rows). `design_contains(full, reduced)` now projects onto the
    whole column space of that estimable design, using SVD after
    column normalization. It no longer truncates an unweighted all-rows
    SVD to the fitted rank. The tolerance is unchanged.
  - BFXX6-3: `odds_effect()`, `odds_point()` and `odds_conf_limit()` now
    select the non-intercept coefficients by name. An intercept-only model
    gets an empty 0 x 2 interval table. A one-coefficient vector from
    `confint()` is reshaped to a matrix. No `2:n` indexing remains.
    Side effect found while fixing: a model without an intercept used to
    drop its first predictor's odds ratio (`[-1]`). It now reports all
    of them.
- `R/blr-lrtest.R`: compares both designs on the full fit's informative
  rows, with the full design cut to its estimable columns. Prior-weight
  equality between the models was already required.
- `R/blr-model-fit-stats.R`: `null_model_nested()` uses the same policy.
- Tests: `test-forward-selection.R` (2 blocks), `test-blr-lrtest.R`
  (1 block).
- `NEWS.md`: two new bullets (zero-trial selection with the error
  fallback; intercept-only and no-intercept odds tables). The LR nesting
  bullet is extended.

## Reproduction before (567e246) / after

BFXX6-1 (seed 6, grouped `cbind(s, trials-s) ~ x + noise + offset(off)`,
`trials[1] = 0`, `weights = w`). New data has `off + 2`. All six procedures:

| | before | after |
| --- | --- | --- |
| returned formula | `` `cbind(s, trials - s)` ~ x `` | `cbind(s, trials - s) ~ x + offset(off)` |
| newdata prediction error vs glm | 0.4621147674 | 0 |
| `length(predict(k$model, nd[2:6, ]))` | 200 | 5 |
| first three predictions on nd | .7593, .3198, .9367 | .9588533716, .7764947178, .9909307024 (= glm) |
| coef / logLik / profile CI error | 0 / 0 / 0 | 0 / 0 / 0 |

BFXX6-2 (`am ~ wt + hp + z` vs `am ~ wt`, `w[1:8] = 0`, `z` aliased on the
informative rows; wt scaled by 1, 1e155, 1e-200):
- before: "must be nested" at every scale, and `blr_model_fit_stats()` lr
  fields NA with the "not nested" warning.
- after: LR 8.912477, df 1, p 0.002832288 (= `anova()`) at every scale.
  Fit stats give LR 26.49604, df 2, p 1.763832e-06.
- The zero-trial `cbind(s, fail)` variant was rejected before. After, it
  is accepted with LR 8.912477, df 1 (= `anova()`).

BFXX6-3: `blr_regress(glm(am ~ 1), odd_conf_limit = TRUE)` failed before
with "attempt to set 'colnames' on an object with less than two
dimensions". After, the summary prints, with 0 effects and a 0 x 2 interval
table. The seed-11 noise fixture's intercept-only result works in all six
procedures.

## Weighted / grouped review (same class of mistake)

The scratch script compares all six procedures with an independent `glm()`
using the same call. It checks coefficients, logLik, `confint()`,
`blr_regress()` odds limits, NA padding, and predictions on new data with
**changed offsets**, including that predicting 5 rows gives 5 values.
Cases: formula offset; argument offset; both; two formula offsets; weights
+ `poly()` + offset + `na.exclude`; grouped + offset + `na.exclude`; grouped
+ weights + argument offset; zero-trial groups + formula offset; zero-trial
groups + weights + `poly()` + formula and argument offsets + `na.exclude`;
intercept-only with a formula offset. Every case: error 0. In the last
case, two procedures keep `poly(z, 2)` while the reference is refit on
the 197 fitted rows. Their coefficients differ only by the poly basis, and
logLik and predictions agree (2e-16). No case reached the new error. LR
boundary and invalid-control results are unchanged from bfx6. The
eps 1e-8 at 1e-200 case is still rejected with the "numerical precision"
error. Weighted invalid controls (reversed wt at each scale) still error.

## Tests

- Fail before: the new test files were run against a `git archive HEAD`
  (567e246) scratch copy (no `git stash`). The result was 28 failures or
  errors, all in the 3 new blocks. All pass after.
- Full suite (`NOT_CRAN=true`, `devtools::test()`): 19 files, 205 test
  blocks, 905 expectations passed, 0 failed, 0 errors, 0 warnings, 0
  skipped. testthat's deletion of the unused `_snaps/forward-p.md` and its
  snapshot line-ending rewrites were reverted with `git checkout`.

## R CMD check

`R CMD check --as-cran blorr_0.3.2.tar.gz`. The tarball was built with
`R CMD build --no-manual` from a `git archive HEAD` copy with the changed
files overlaid; `diff -rq --strip-trailing-cr` found no differences from the
worktree. The check built the PDF manual ("checking PDF version of manual
... OK"). `_R_CHECK_FORCE_SUGGESTS_=false` because V8 is not installed.
**Status: 1 NOTE, 0 warnings, 0 errors.** The NOTE is the expected
maintainer change (hebbali.aravind@gmail.com ->
aravindhebbali@rsquaredacademy.com).

## Commands run

- `Rscript r7.R <dir>` (exact audit reproductions, on the 567e246 archive
  copy and the worktree), `Rscript r6b.R` (extended weighted/grouped/offset
  hole check).
- Filtered `devtools::test()` on the archive copy with the new tests, then
  in the worktree. Full `devtools::test()`.
- `git archive HEAD | tar -x` with the changed files overlaid, then
  `diff -rq --strip-trailing-cr` against the worktree, `R CMD build
  --no-manual`, and `_R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran`.

## Not fixed / judgment calls

- Rows with zero prior weight are left out of the nesting check, so two
  designs that differ only on those rows count as nested. This matches the
  likelihood, where those rows contribute nothing, and `anova()`.
- The LR rounding-boundary rejection from bfx6 is unchanged.
- Carried over: selection always fits an intercept; term hierarchy is not
  enforced.

## Follow-ups

1. Codex re-audit of this round (bfxx7) before any CRAN submission.
   Focus: the restore error path (try to find an input that reaches it),
   effective-weight check, informative-row nesting, odds tables for
   no-intercept and aliased models.
2. Aravind: no-intercept selection policy (open from bfx5).
3. Aravind: marginality for interaction terms (open from bfx5).
