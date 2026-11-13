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
- BFXX8-3 (Medium): printed Df. Model is df.null; AIC detail tables hard-code DF 1.
- BFXX8-4 (Medium): grouped-binomial response summary, association table and count R2 treat counts as binary.
- BFXX8-5 (Medium): blr_regress on zero-parameter no-intercept glm errors.
- BFXX8-6 (Medium): p-value selection crashes on aliased candidate terms.
- BFXX8-7 (Low): p-both prints deviance under "C(p)".
BFXX8-3, 4 and 7 give wrong numbers or labels and are within the common rule; they were left because the
brief limited this round to BFXX8-1/2 plus "any Medium/Low" only if time allows: Aravind/Claude to decide
whether to run a bfx10 round for them.

## Checks
See the final section appended below after the R CMD check run.
