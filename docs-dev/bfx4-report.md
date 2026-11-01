# bfx4: blorr open findings after three flash rounds

Round-4 fix round (DeepSeek Pro) in worktree `task/blorr-fix-032`, branch
`task/blorr-fix-032`. Base for the diff: `a55c15c` (round-3 HEAD). Fixes the
three findings the round-3 re-audit left open and, through them, closes the
seven findings the re-audit had marked "partly".

Scope: only BFXX3-1, BFXX3-2 and BFXX3-3 (2 high, 1 medium). Every exact
reproduction from `bfxx3-reaudit.md` was re-run and corrected, each with a
value-asserting test added before the fix.

## Fixes

| Finding | Test added | Fix (file:line) | Verified how | Status |
| --- | --- | --- | --- | --- |
| BFXX3-1 (High) predictor scale changes non-nested LR inference | `test-model-fit-stats.R:309` (rank-one no-intercept at scales 1/1e6/1e8), `test-blr-lrtest.R:163` (reversed-`wt` vs `wt+big` at scales 1/1e6/1e8) | `R/blr-utils.R:259` `column_normalize`, `R/blr-utils.R:272` `design_contains`; `R/blr-model-fit-stats.R:410` `null_model_nested`; `R/blr-lrtest.R:120` `lr_check_comparable` | Both designs column-normalized to unit 2-norm, each reduced column projected and its residual measured against its own (unit) scale. Exact reproductions: `am ~ 0 + wt*1e8` now warns and returns `NA/NA/NA` LR fields (was `0/0/1`); `wt + hp*1e6` vs reversed-`wt` now errors "nested" (was accepted with LR 32.83). Valid nested cases (factor contrasts, `I(wt)`, intercept) still pass | Fixed |
| BFXX3-2 (High) selection mixes changed source predictors with the stored response | `test-forward-selection.R:131` (all six procedures after `wt` doubled and reversed; coefficient/term/prediction assertions) | `R/blr-utils.R:335` `selection_data` returns `model$model`; `R/blr-utils.R:294` `selection_formula` quotes each predictor | Exact reproduction: after `d$wt <- 2*d$wt`, forward p returns `wt` coefficient `-8.08347518244463` equal to the original-data reference (was `-4.04173759122231`) and prediction differs by `0` (was `0.999`) | Fixed |
| BFXX3-3 (Medium) candidate formulas lose local transformations | `test-forward-selection.R:167` (locally scoped `log2wt`), `test-forward-selection.R:186` (no-data locally scoped `log(x_local)`) | `R/blr-utils.R:294` `selection_formula` quotes each predictor; `R/blr-utils.R:335` `selection_data` uses stored evaluated columns | Exact reproduction: `log2wt(wt)+hp+noise` now returns coefficients `19.76650040, -22.66901764, 0.03310056` equal to the independent glm (was `could not find function "log2wt"`). The no-data local case now also succeeds instead of `object 'x_local' not found` | Fixed |

The round-3 report's claim that every fit with a recorded data argument is
handled is corrected: selection no longer depends on a recorded data argument
or on re-evaluating predictor expressions at all, so the same behaviour holds
with or without a `data` argument.

## Partly-fixed findings now closed

The re-audit marked seven findings "partly" only because their exact
reproductions passed while a related counterexample failed in BFXX3-1/-2/-3:

| Finding | Resolved by |
| --- | --- |
| H2 weighted mtcars LR/R2/regression | BFXX3-2/-3 (stored model frame) |
| H3 missing-gear/nesting | BFXX3-1 (scale-invariant containment) |
| BF2 weighted forward p | BFXX3-2 (stored model frame) |
| BF4 missing intercept / reversed predictor | BFXX3-1 (scale-invariant containment) |
| BFXX2-1 rank-one/two-predictor no-intercept | BFXX3-1 (scale-invariant containment) |
| BFXX2-3 `log(wt)` selection | BFXX3-2/-3 (stored model frame) |
| BFXX2-4 factor contrasts / `I(wt)` | BFXX3-1 (scale-invariant containment) |

Their committed value assertions remain in the suite and pass unchanged.

## Deferred

None. The no-data locally scoped transformation, which the round-3 report had
declared as an explicit limitation, is now handled by the same fix and is
covered by a test; no other finding from the re-audit was touched.

## Check output

- `devtools::test()` (working tree, after reverting snapshot line-ending
  churn and restoring the pre-existing unused `forward-p.md` snapshot):
  all green. The clean check-mode suite below is authoritative.
- `R CMD build --no-manual` from a clean `git archive HEAD` copy succeeded
  (vignettes rebuilt, `docs-dev` excluded by `.Rbuildignore`).
- `R CMD check --as-cran --no-manual blorr_0.3.2.tar.gz`: **0 errors /
  0 warnings / 1 NOTE**. The single NOTE is CRAN incoming feasibility's
  maintainer change (`aravindhebbali@rsquaredacademy.com` replaces
  `hebbali.aravind@gmail.com`), unchanged from the round-3 report.
  Check-mode suite: `FAIL 0 | WARN 0 | SKIP 49 | PASS 448` (round-3 was
  `PASS 379`; the difference is the new value assertions). Examples and
  run-donttest examples OK.

## Commands run that changed something

- `R/blr-utils.R`: added `column_normalize` / `design_contains`; changed
  `selection_formula` to quote predictors and `selection_data` to return the
  stored model frame.
- `R/blr-model-fit-stats.R`: `null_model_nested` now delegates to
  `design_contains`.
- `R/blr-lrtest.R`: `lr_check_comparable` now delegates to `design_contains`.
- `tests/testthat/test-model-fit-stats.R`, `test-blr-lrtest.R`,
  `test-forward-selection.R`: new value-asserting tests; transformed-term
  label assertions updated for the quoted column names.
- `NEWS.md`: two new bullets under 0.3.2 Bug Fixes.

## Commits

- `b11dbee` fix: scale-invariant LR nesting checks (BFXX3-1) (bfx4)
- `4230e7f` fix: refit selection on the stored model frame (BFXX3-2, BFXX3-3) (bfx4)

## Anything not done

- The pre-existing untracked re-audit reports `docs-dev/bfxx2-reaudit.md` and
  `docs-dev/bfxx3-reaudit.md` were left as-is (they are audit reports, not the
  "audit temp folders" this job was asked to delete; no temp folders were
  present). This report is committed as `docs-dev/bfx4-report.md`.
- `Rplots.pdf` and any snapshot line-ending churn produced by local
  `devtools::test()` runs were reverted; the committed tree contains only the
  source, test, NEWS and report changes above.
