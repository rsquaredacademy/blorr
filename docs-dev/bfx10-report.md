# bfx10 report: blorr 0.3.2 fix round 10

Branch task/blorr-fix-032, base 4cb051e (bfx9). Closes the items deferred in bfx9.

| Finding | Test added | Fix | Verified how | Status |
|---|---|---|---|---|
| BFXX8-4 grouped binomial (cbind or proportion + weights as trials) | tests/testthat/test-bfx10.R: pairs, count/adjusted count, Efron, McKelvey-Zavoina, McFadden (+adj), Cox-Snell, Nagelkerke and every field of `blr_model_fit_stats()` equal the expanded Bernoulli fit (equal and unequal trials, cbind and weights forms); `blr_regress` response summary 170/150, one-string response name; errors for the unsupported functions | R/blr-utils.R (`is_grouped_binomial`, `grouped_counts`, `blr_check_ungrouped`, `resp_profile`, `response_var`), R/blr-pairs.R, R/blr-model-fit-stats.R (`grouped_loglik`, R2 functions, fit stats table) | values asserted against `glm()` on the expanded data; audit numbers c .595098039216, count R2 .5625, adjusted .0666666666667 | Fixed |
| BFXX8-3 AIC detail tables DF | test-bfx10.R: DF column of backward/forward/both details equals `drop1()`/`add1()` Df (factor term = 3) | R/blr-forward-selection.R, R/blr-backward-elimination.R, R/blr-stepwise-selection.R: DF = rank difference of candidate and current model | drop1/add1 | Fixed |
| BFXX8-5 zero-parameter no-intercept glm | test-bfx10.R: `blr_regress(glm(am ~ 0))` with and without odds limits, empty tables, residual df 32 | R/blr-utils.R `predictor_mine()` returns empty vectors | residual df equals `df.residual()` | Fixed |
| BFXX8-6 aliased candidate terms in p procedures | test-bfx10.R: `blr_step_p_forward/both/backward` on aliased fits with and without intercept stop with an "aliased" message; AIC procedures still run | R/blr-error-messages.R `blr_check_aliased()`, called in the three p procedures (documented in Rd) | error text asserted | Fixed (rejected, documented) |

## Grouped binomial: every function checked and which way it went
Grouped means a `cbind(successes, failures)` response, or proportions with trials as weights.
Computed per trial (equal to the expanded Bernoulli fit, tested): `blr_pairs`, the association and response-summary
tables in `blr_regress`, `blr_model_fit_stats`, `blr_multi_model_fit_stats` (uses the previous), `blr_rsq_mcfadden`,
`blr_rsq_mcfadden_adj`, `blr_rsq_cox_snell`, `blr_rsq_nagelkerke`, `blr_rsq_effron`, `blr_rsq_mckelvey_zavoina`,
`blr_rsq_count`, `blr_rsq_adj_count`. Non-integer trial counts stop with a message.
Stop with "does not support grouped binomial data": `blr_gains_table` (and the ks/decile/ROC charts, which take its
output), `blr_gini_index`, `blr_lorenz_curve`, `blr_prep_lorenz_data`, `blr_confusion_matrix`,
`blr_test_hosmer_lemeshow`, `blr_linktest`.
Unchanged because they use the glm fit, design or supplied data, not 0/1 rows: `blr_test_lr`, `blr_coll_diag`,
`blr_vif_tol`, `blr_eigen_cindex`, `blr_residual_diagnostics` and the `blr_plot_*` diagnostics, the six selection
procedures (they call the fit stats above), `blr_bivariate_analysis`, `blr_segment*`, `blr_woe_iv*` (data-frame
functions). I did not add value tests for the glm-based residual plots on grouped data.
Note: `blr_model_fit_stats()` and the R2 values for grouped models now follow the one-row-per-trial convention, so
Cox-Snell etc. on grouped models changed from the earlier glm-grouped values (the old bfx test was updated to compare
with the expanded fit). Plain Bernoulli models are unchanged. Rd note: man/blr_model_fit_stats.Rd (hand edited, no roxygen
re-run because it rewrote every Rd).

## Other changes
`response_var()` returns one deparsed string (test-utils.R updated from a call to a string).
`base_rank` extra fits in selection loops add one small glm per step. NEWS.md updated.

## Checks
See the final section below.
