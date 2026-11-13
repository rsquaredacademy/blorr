# bfx9 report: blorr 0.3.2 fix round 9

Branch task/blorr-fix-032, base 0acd374. Convergence rule applied: no more tolerance tuning.

| Finding | Test added | Fix | Verified how | Status |
|---|---|---|---|---|
| BFXX8-1 (High) nesting check accepts non-nested models | test-blr-lrtest.R: "BFXX8-1 reproductions stop; nested controls are accepted (bfx9)" (both exact dyadic cases pow 27/33, stable parameterization, orthogonal fixture at scales 1/1e155/1e-200); "LR nesting is structural for near-collinear designs"; "stops when nesting is not structural" | R/blr-utils.R `design_nested()` replaces `design_contains()`/`column_normalize()` (condition-scaled tolerance and `reliable` attribute removed); R/blr-lrtest.R `lr_check_comparable()`; R/blr-model-fit-stats.R `null_model_nested()` now requires an estimated intercept | Nested controls compared to `anova()`; rejections assert the error | Fixed |
| BFXX8-2 (High) backward AIC mixed baseline | test-backward-aic.R "baseline row comes from one refitted full model (bfx9)" (failed before: no-intercept input removed v, mixed row) | R/blr-backward-elimination.R: `aic_f <- model_aic(mi)` from the refit `mi` (also supplies BIC/deviance) | AIC/BIC/deviance of row 1 equal AIC/BIC/deviance of manual `glm(y ~ x+z+v)` (187.4759410925487); zero removals for both inputs | Fixed |

## Rule now in force (BFXX8-1)
Reduced model is nested only if same family/link, same y, prior weights, offset, rows, and every estimated
(non-aliased) reduced column name is an estimated full-model column name with equal values on informative
rows (prior weight > 0), and the residual df differ by at least 1. Otherwise error. User-visible consequence:
equivalent parameterizations under different names (treatment vs sum contrasts, `I(wt)` vs `wt`, a column
that is a linear combination of full columns) now error instead of being tested. Tests for those were changed
to expect the error (an API behaviour change; documented in NEWS).
The intercept-only null in `blr_model_fit_stats()` is available only when the model has an estimated
`(Intercept)`; a no-intercept model whose columns span the constant now gives "not nested" / NA (previously available).

## Deferred (not fixed; wrong-number items outside the brief's two High items)
- BFXX8-3 (Medium), AIC detail tables only: candidate rows still hard-code DF 1 (zero-rank / factor / matrix terms). The overview `Df. Model` part was fixed (see below).
- BFXX8-4 (Medium): grouped-binomial response summary, association table and count R2 treat counts as binary.
- BFXX8-5 (Medium): blr_regress on zero-parameter no-intercept glm errors.
- BFXX8-6 (Medium): p-value selection crashes on aliased candidate terms.
BFXX8-3 (tables), 4, 5 and 6 are larger changes (grouped-binomial handling, alias handling in three p procedures,
rank-based DF) and were not done this round; Claude/Aravind to decide on a bfx10 round for them.

## Also fixed (cheap Medium/Low items)
| Finding | Test | Fix | Status |
|---|---|---|---|
| BFXX8-3 (overview) Df. Model was df.null | test-blr-regress.R "overview Df. Model ..." (am~wt+hp: 2, no-intercept: 2, aliased 0+wt+I(2*wt): 1, am~1: 0; resid df 29); test-utils.R model_df; blr-regress snapshot updated (199 -> 3) | R/blr-utils.R `model_df()`: non-aliased coefficients minus intercept | Fixed |
| BFXX8-7 p-both column labelled C(p) | test-forward-selection.R "p-both summary labels ... Deviance" | R/blr-output.R:641 label "Deviance" | Fixed |

## Checks
See the final section appended below after the R CMD check run.

## Check results (clean git archive of HEAD 3795a8e, R CMD build, R CMD check --as-cran --no-manual)
0 errors, 0 warnings, 1 NOTE (CRAN incoming feasibility: maintainer change, expected per cran-comments.md). Examples (incl. donttest), tests (60 s) and vignette rebuild OK.
devtools::test() with NOT_CRAN=true: 212 tests, 0 failures, ~1051 expectations.
Commands that changed something: edits to R/, tests/, NEWS.md, docs-dev; blr-regress snapshot updated (Df. Model 199 -> 3).
