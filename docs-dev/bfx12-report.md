# bfx12 report: blorr 0.3.2 fix round 12

Branch task/blorr-fix-032, base 28ef12c (bfx11). Decision (Aravind, 2026-11-13): weighted models are rejected too.

| Finding | Test added | Fix | Status |
|---|---|---|---|
| BFXX11-1/2 weighted models wrong in fit measures, pairs, response table, gains, HL, linktest | test-bfx10.R "every entry point rejects weighted and grouped models (bfx12)" | one shared guard `blr_check_supported()` (R/blr-utils.R) called from `blr_check_model()`: any prior weight != 1 (zero weights included; `model$prior.weights` and `weights(model, "prior")`) stops with "blorr supports unweighted 0/1 (Bernoulli) models; weighted and grouped binomial models are not supported." | Resolved by rejection |
| BFXX11-3 guard depends on optional storage | same test file: "a fit without its model frame or response is declined (bfx12)"; zero-weight interior response fixture; both storage flags | the guard needs `model$model` and `model$y` (else "refit with glm(..., model = TRUE, y = TRUE)"), inspects the stored original response (model frame column 1: matrix, or numeric values other than 0/1; factor and logical responses keep their 0/1 mapping) as well as `y`; `blr_test_lr()` validates the supplied model before the automatic null-model reconstruction | Fixed |
| BFXX11-4 warning in nesting test | test-blr-lrtest.R "ordinary nested comparisons ..." now on non-separating simulated data (numeric, factor, interactions, formula offset) | no glm warnings; plus an argument-offset mismatch error assertion | Fixed |

Entry points tested: every exported function whose first argument is `model`, `full_model` or `object`
(computed from the namespace, at least 50; the auditor's 51) times six rejected fixtures: interior cbind, interior
proportions with trials as weights, all-boundary cbind, case weights, a zero weight, and a zero-weight row with an interior
response (the audit's case 1). Plus both positions in `blr_test_lr` and `blr_multi_model_fit_stats`. Fits with
`model = FALSE` or `y = FALSE` (grouped and plain Bernoulli) give the refit message at every entry point.
Accepted and tested: unweighted Bernoulli with numeric, factor and logical responses, explicit unit weights, na.exclude.

Removed tests: the weighted-case tests from earlier rounds (weighted selection printing, weighted LR/pseudo R2/regress,
bfx7 informative-rows nesting, weighted na.exclude selection). The weights comparison inside `lr_check_comparable()` stays
(it still compares prior weights exactly; with the guard it only sees unit weights).
Documentation: the `model`/`full_model` argument text of 48 Rd files and roxygen sources was updated by hand (no
`devtools::document()`); NEWS has one bullet.

## Checks
See below.
