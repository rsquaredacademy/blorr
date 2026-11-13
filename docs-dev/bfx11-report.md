# bfx11 report: blorr 0.3.2 fix round 11

Branch task/blorr-fix-032, base 3296992 (bfx10 report).

| Finding | Test added | Fix | Verified how | Status |
|---|---|---|---|---|
| BFXX10-1 nesting check used `all.equal(tolerance = 1e-8)` | test-blr-lrtest.R "same-named but unequal columns are rejected, with no tolerance (bfx11)": the small-units case (wt*1e-200 reversed) and scales 1/1e-10/1e-100/1e150, the exact dyadic power-27 case (was LR -184.2), and a one-value 1e-15 relative change all error; "ordinary nested comparisons still run and match anova (bfx11)": numeric, factor, interaction, factor*numeric interaction, case weights and formula offset all equal `anova(..., test = "Chisq")` LR/df/p | R/blr-utils.R `design_nested()` compares shared columns with `identical()`; R/blr-lrtest.R `lr_check_comparable()` compares response, prior weights and offset exactly (names dropped, NULL offset = zeros). Comments and NEWS no longer claim anything but exact equality | tests; package examples and vignette run in the clean check | Fixed |
| BFXX10-2/3/4 (grouped likelihood, offsets, boundary proportions) | test-bfx10.R "grouped binomial models are rejected everywhere (bfx11)" | Aravind's decision: reject grouped binomial. `blr_check_grouped()` is called from `blr_check_model()` (every exported function taking a glm, plus `blr_prep_lorenz_data`) | cbind with interior and all-boundary proportions, proportions with weights, all error; 33 functions asserted | Resolved by rejection |

## Grouped rejection
Detection: a matrix response (any `cbind(...)`, including all-boundary), or any model `y` value other than 0 or 1
(proportions). Message: "blorr supports 0/1 (Bernoulli) responses; grouped binomial models (cbind(successes, failures)
or proportions with weights) are not supported."
Limitation to decide: a numeric 0/1 response with weights (e.g. `glm(am ~ wt, weights = w)`), which is also how an
all-boundary proportion response with trials as weights looks, cannot be told apart from Bernoulli with case weights.
It is read as case weights, as before (the weighted-fit tests from earlier rounds rely on this), so BFXX10-4's
"proportions with weights, all 0/1" encoding is not rejected; only its cbind encoding is. Rejecting every non-unit weight
would break the case-weight support audited in earlier rounds.
Removed: the bfx10 per-trial code (pairs, R2 and fit-stats branches, `grouped_loglik`, response profile branch) and their
tests; the grouped zero-trial tests in test-forward-selection.R, test-blr-lrtest.R and test-model-fit-stats.R. The
internal selection restore code that handles cbind responses (selection_weights etc.) is left in place; it is unreachable
from exported functions. Kept from bfx10: term-df in AIC tables, empty-model summary, aliased p-selection rejection,
one-string response name.
Documentation: the `model`, `full_model` arguments of 48 Rd files (and their roxygen sources) state the restriction;
edited by hand, `devtools::document()` was not run.
Plain Bernoulli results: bfx9 and earlier tests (all examples, snapshots) unchanged; test "Bernoulli models, with and
without case weights, are unaffected".

## Checks
See below.

Clean `git archive` of commit 68d0599: `R CMD build` + `R CMD check --as-cran --no-manual`: 0 errors, 0 warnings, 1 NOTE
(CRAN incoming: maintainer change, expected). Examples, donttest examples, tests and vignette rebuild OK.
`devtools::test()` (NOT_CRAN=true): 215 tests, 0 failures, 1104 expectations.
