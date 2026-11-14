# bfx6: blorr fix round 6 (BFXX5-1, BFXX5-2, BFXX5-3)

Date: 2026-11-02. Worktree `F:/R/Packages/statistics/blorr-wt/fix-032`,
branch `task/blorr-fix-032`, base `666057d`. Audit fixed:
`docs-dev/bfxx5-reaudit.md` (left untracked, with `.bfxx5/`, untouched).
Environment: Windows, R 4.6.1, testthat 3.3.2.

## Files changed

- `R/blr-utils.R`
  - `selection_restore_impl()` (BFXX5-1): the restored model frame now takes
    its extra columns (`(weights)`, an argument-supplied `(offset)`) from the
    original fit's model frame, not from the stored-frame refit whose
    `(offset)` holds the total offset. Formula offsets are therefore counted
    once. Two new consistency checks: the restored frame's
    `model.offset()` must equal the refit's offset exactly and its
    `model.weights()` must equal the weights used for the refit; otherwise
    the unrestored refit is returned (the same estimates, and its own frame
    is self-consistent).
  - `selection_restore()` (BFXX5-3): the original fit's `na.action` is put
    on the returned model and on its model frame (both on the restored
    path and the fallback), so `na.exclude` fits pad residuals, fitted
    values and predictions as an ordinary `glm()` fit does.
  - `design_contains(full, reduced, rank = NULL)` (BFXX5-2): the full
    column space is the span of the leading `rank` left singular vectors of
    the column-normalized design, where `rank` is the fitted full model's
    rank. The default `qr()` tolerance no longer drops a nearly collinear
    direction the GLM estimated. Without `rank`, singular values below
    `1e-12 * max` count as zero. The residual tolerance is unchanged
    (`sqrt(.Machine$double.eps) * sqrt(n)`). The largest residual is
    returned as an attribute.
- `R/blr-lrtest.R`: passes `rank = full_model$rank`. When nesting fails with
  a residual below 1e-4, the error now says nesting "could not be confirmed
  to numerical precision, possibly because the full design is nearly
  collinear" (it still starts "reduced_model must be nested within
  full_model").
- `R/blr-model-fit-stats.R`: `null_model_nested()` passes `rank = model$rank`
  and drops the attribute.
- `R/blr-pairs.R`: `blr_pairs()` uses `model$fitted.values` (unpadded,
  aligned with `model$y`) instead of `fitted(model)`. Found while fixing
  BFXX5-3: `blr_pairs()`/`blr_regress()` failed with "arguments imply
  differing number of rows: 29, 32" on any `na.exclude` glm, which the
  selection procedures hit once padding was restored. Pre-existing bug, it
  errored (was not silently wrong).
- Tests: `test-blr-lrtest.R` (1 block), `test-forward-selection.R`
  (2 blocks), `test-blr-pairs.R` (1 block).
- `NEWS.md`: one new selection bullet (offsets once, `na.exclude` padding),
  one `blr_pairs()` bullet, LR nesting bullet extended.

## Reproduction before (666057d) / after

BFXX5-1 (seed 6, `y ~ x + z + noise + offset(off)`), all six procedures:

| | before | after |
| --- | --- | --- |
| `max |model.offset(frame) - offset|` | 2.11887726874466 | 0 |
| AIC-forward `confint()["x", ]` | 1.735160780, 1.916003932 | 1.338478645, 2.377497411 (= independent glm) |
| p-forward `confint()["x", ]` | 1.711444034, 1.891859711 | 1.314581434, 2.354635745 (= independent glm) |
| max profile-CI error vs independent glm | .4628 (p-fwd/back), .4615 (others) | 0 in all six |
| `blr_regress(odd_conf_limit = TRUE)` limits vs glm | differ | equal (error 0) |

BFXX5-2 (dyadic `b = a + 2^-27 v`, `am ~ a + b + hp` vs `am ~ v`):
before `Error: reduced_model must be nested within full_model.`; after
LR 9.760188457, df 2, p 0.007596298, equal to `anova()`.

BFXX5-3 (`am ~ log(wt) + hp + noise`, `na.exclude`, hp missing in rows
2, 9, 30): before `predict`/`residuals` length 29 in all six, `na.action`
NULL; after length 32, NA at 2, 9, 30, names equal to the reference, coef
error 0, in all six.

## Hole check of the other bfx5 changes (scratch script, all six procedures)

Compared with an independent `glm()` with the same call on the fitted rows:
coefficients, logLik, `confint()`, `blr_regress()` odds-ratio limits,
predictions on new data, and NA padding/names. Cases: formula offset;
argument offset; formula + argument offset; two formula offsets; weights +
`poly()` + formula offset + `na.exclude`; grouped `cbind(s, f)` + formula
offset + `na.exclude`; grouped + weights + argument offset; intercept-only
selection with a formula offset. Every comparison: error 0 and padding
identical.

LR boundary (seed-12 `b = wt + rnorm(sd = eps)`, `v = (b - wt)/eps`, `a`,
`b` scaled by s): accepted with LR equal to `anova()` (32.8082917, df 2) for
eps 1e-4 and 1e-6 at s = 1, 1e155, 1e-200, and eps 1e-8 at s = 1 and 1e155.
eps 1e-8 at s = 1e-200 is rejected with the new "could not be confirmed to
numerical precision" error: scaling by 1e-200 is not exact, so the stored
`v` is off the span of the stored `a`, `b` by about 1e-8 relative (the largest
residual is 8.8e-8 to 9.2e-8, against a tolerance of 8.4e-8). This is an
explicit error, not a wrong result. Loosening the tolerance would accept
more non-nested designs, so I left it. Invalid controls (reversed
predictor at s = 1, 1e155, 1e-200; reversed same-name wt) still error.

## Tests

- Fail before: the new test files were run against a `git archive HEAD`
  (666057d) copy in the scratchpad (no `git stash`). The result was 88
  failures or errors across all 4 new blocks: LR 1, pairs 1, offsets 38,
  na.exclude 48. All pass after.
- Full suite (`NOT_CRAN=true`, `devtools::test()`): 19 files, 202 test
  blocks, 762 expectations passed, 0 failed, 0 errors, 0 warnings, 0
  skipped. As before, testthat deleted the unused `_snaps/forward-p.md` and
  rewrote snapshot line endings; reverted with `git checkout`.

## R CMD check

`R CMD check --as-cran blorr_0.3.2.tar.gz`: tarball built with `R CMD build
--no-manual` from a `git archive HEAD` copy with the changed files overlaid
(verified identical to the worktree with `diff -rq --strip-trailing-cr`).
The check built the PDF manual ("checking PDF version of manual ... OK").
`_R_CHECK_FORCE_SUGGESTS_=false` because V8 is not installed.
**Status: 1 NOTE, 0 warnings, 0 errors.** The NOTE is the expected
maintainer change (hebbali.aravind@gmail.com ->
aravindhebbali@rsquaredacademy.com).

## Commands run

- `Rscript r6.R <pkgdir>` (audit reproductions, run on the HEAD archive
  copy and the worktree), `Rscript r6b.R` (offset/weights/na/LR-boundary
  hole check), `Rscript r6c.R` (LR residual vs tolerance).
- Filtered `devtools::test()` on the HEAD archive copy with the new tests
  (fail before) and in the worktree. Full `devtools::test()`.
- `git archive HEAD | tar -x`, overlay of `git diff --name-only` files,
  `diff -rq --strip-trailing-cr` against the worktree, `R CMD build
  --no-manual`, `_R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran`.

## Not fixed / judgment calls

- LR nesting at the rounding boundary (above): rejected with a clear error.
- Carried over from bfx5: selection always fits an intercept; term
  hierarchy is not enforced.

## Follow-ups

1. Codex re-audit of this round (bfxx6) before any CRAN submission,
   focusing on the restore path (offsets, weights, `na.exclude`) and the
   rank-based LR nesting check.
2. Aravind: whether selection on a no-intercept model should stay without
   an intercept (open from bfx5).
3. Aravind: whether selection should enforce marginality for interactions
   (open from bfx5).
