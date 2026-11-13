# blorr 0.3.2

This is a patch release that fixes several bugs in model fit statistics,
variable selection, bivariate analysis, model validation, gains tables,
weight of evidence, and the Gini index.

## Bug Fixes

- Cox-Snell and Nagelkerke R2 now use the number of observations the model was
  fitted on, so they are correct when rows are dropped for missing values or
  when the model is fitted without a `data` argument. The model fit statistics
  table is also assembled by name, so values no longer shift between labels.
- `blr_vif_tol()` now identifies the intercept from the fitted terms instead of
  assuming the first design column, so a model without an intercept reports
  tolerance and VIF for every predictor instead of silently dropping one.
- `blr_model_fit_stats()` no longer reports a zero likelihood ratio test for
  a model without an intercept. The intercept-only model is only used as the
  null when it is actually nested in the fitted design; otherwise the test is
  reported as unavailable with a warning.
- The likelihood ratio test now checks nesting structurally: the reduced
  model's estimated terms must be a subset, by name and values, of the full
  model's, with the same response, rows, weights and offset. The columns must be
  identical (compared exactly, with no tolerance), so a comparison that cannot be established as nested stops
  with an error instead of returning chi-square inference. Equivalent
  parameterizations under different names (other factor contrasts, `I(wt)` for
  `wt`) are not recognised as nested.
- The model overview now prints `Df. Model` as the number of estimated coefficients excluding the intercept, instead of the null residual df.
- The stepwise p-value (both directions) summary now labels its last column `Deviance`, which is what it reports, instead of `C(p)`.
- Weighted and grouped binomial models are not supported: every function that takes a model now stops with "blorr supports unweighted 0/1 (Bernoulli) models; weighted and grouped binomial models are not supported." This covers any prior weight other than 1 (zero weights included), a `cbind(successes, failures)` response and proportion responses. Fits stored without their model frame or response (`model = FALSE`, `y = FALSE`) are declined with a request to refit with `model = TRUE, y = TRUE`. The response name in the overview is printed as one expression.
- The AIC selection detail tables print the real degrees of freedom of each term (for example 3 for a four-level factor) instead of 1.
- `blr_regress()` works for a model with no coefficients, and the p-value selection procedures stop with a clear message for a model with aliased terms instead of failing with an obscure error.
- `blr_step_aic_backward()` now takes the first row (AIC, BIC and deviance)
  from the same refitted full model that every removal is compared with, so a
  model fitted without an intercept no longer produces a mixed baseline row or
  a wrong first removal.
- The likelihood ratio test now uses the difference in residual degrees of
  freedom between the reduced and full models, giving the correct degrees of
  freedom and p-value for nested models.
- Multi-model fit statistics are now labeled by name, so each measure is
  shown against the correct model statistic.
- The event rate reported by `blr_segment()` and `blr_segment_dist()` is now
  the share of events within each level rather than the share of all events,
  and no longer fails when a level has no events.
- `blr_regress()` now reports the number of observations and the response
  profile from the data used to fit the model when rows are dropped for
  missing values, including for factor responses whose levels are not 0 and 1.
- Forward selection now stores the BIC and deviance of each step instead of
  the AIC after the first step.
- Variance decomposition proportions in `blr_eigen_cindex()` now sum to one
  for each coefficient. Eigenvalues and condition indices are now computed
  from the singular values of the scaled design rather than from its
  cross-product, so a nearly collinear full-rank design keeps a finite index
  and a design without an intercept keeps its predictor names.
- Somers' D and Goodman-Kruskal gamma in `blr_pairs()` now use the correct
  denominators.
- Gains tables now assign every observation to exactly one of ten deciles,
  count events using the event level of the response, and no longer fail for
  small samples or when a decile contains only one outcome.
- Gains tables and Lorenz data now assign deciles after dropping rows with
  missing outcomes or predictors, so those rows no longer cause an error or
  misaligned deciles.
- Confusion matrices now show both outcome classes even when the model
  predicts only one class, McNemar's test is reported as unavailable when it
  cannot be computed, and the no information rate uses the larger class.
- The Gini index and Lorenz curve now use the observations the model was
  fitted on, so rows dropped for missing values are no longer misaligned, and
  the Gini index is reported as 2 * AUC - 1, the credit-scoring definition
  based on the area under the ROC curve (values differ from earlier
  versions, which reported the concentration index of the predicted
  probabilities). The index is computed from the
  Mann-Whitney rank statistic, so it remains fast on large models, and `data`
  without the response column is now an error.
- The default `blr_gini_index()` call now uses the fitted response labels, so
  labelled factor and logical responses give the same index as an explicit
  `data` argument instead of `NaN`.
- Variable selection procedures now return an intercept-only model when no
  predictor qualifies, and p-value based procedures apply the entry criterion
  to the first predictor as well.
- Variable selection procedures now refit candidate models on the stored model
  frame, so predictors supplied as expressions such as `log(wt)` or built from
  locally defined functions are handled correctly, and editing or reassigning
  the source data after fitting no longer changes the selected terms. The
  returned model keeps the original term expressions, so `predict()` on raw
  new data recomputes terms such as `log(wt)` or `poly(x, 2)` as for an
  ordinary `glm()` fit, and interaction terms such as `x:z` and non-syntactic
  names such as `` `body weight` `` are refit correctly.
- Models returned by variable selection keep formula and argument offsets
  exactly once in their model frame, so `confint()` and the odds ratio
  limits of `blr_regress()` match an ordinary `glm()` fit, and models fitted
  with `na.action = na.exclude` keep their padded residuals, fitted values
  and predictions.
- `blr_pairs()`, and therefore `blr_regress()`, now works for models fitted
  with `na.action = na.exclude`.
- Variable selection on grouped binomial fits with zero-trial groups now
  returns a model with the original formula and offsets, so `predict()` on
  new data uses the new offset values. If a selected model cannot be rebuilt
  exactly from the original terms, selection now stops with an error instead
  of returning a model whose predictions could differ from `glm()`.
- `blr_regress()` now matches standard errors, z values and p-values to the
  coefficients by name, so an aliased coefficient no longer shifts them onto
  other predictors; aliased rows show 0 degrees of freedom and missing
  statistics, and odds ratio tables with aliased coefficients now print.
- Variable selection rebuilds selected terms from the fitted model's own
  expressions, so terms with high-precision constants such as
  `I(wt / 3.141592653589793)` keep their exact values.
- `blr_regress(odd_conf_limit = TRUE)` now works for intercept-only models,
  reporting an empty odds ratio table, and reports the odds ratio of every
  predictor of a model without an intercept.
- Forward, backward and stepwise selection now keep the fitted model's weights,
  offset, link and contrasts in every candidate model, and print the same
  fitted model they return instead of an unweighted formula refit.
- `blr_woe_iv()` now accepts character and numeric predictors, drops unused
  factor levels, applies a 0.5 continuity correction to levels with no
  events or no non-events, and drops rows with missing values so they are not
  counted as non-events.
- `blr_plot_dfbetas_panel()` now works for models with one or more predictors.
- Odds ratio confidence limits in `blr_regress()` are now labelled as
  profile-likelihood intervals.
- The forward selection documentation now describes selection by AIC.
- Deviance residual plots now use and are labelled as standardized deviance
  residuals.
- Forward AIC selection now compares the best first candidate with the
  intercept-only model and returns the intercept-only model when no predictor
  improves AIC.
- Weighted fits keep their weights, offset, link and contrasts in likelihood
  ratio tests, pseudo R-squared measures and `blr_regress()`, and
  `blr_regress()` can summarise a fit whose response is a transformed
  expression.
- Grouped binomial fits no longer apply the trial counts twice when the null
  model is reconstructed for pseudo R-squared measures, likelihood ratio tests
  or variable selection, so those statistics match base R.
- Likelihood ratio tests reject models fitted to different observations,
  weights or offsets, or non-nested models, and bivariate analysis fits its
  null model on the predictor model's complete rows. Nesting is checked
  against the fitted design matrices, including the intercept and the actual
  predictor values, and comparisons with no additional parameters are
  rejected.
- Efron, count, adjusted count and McKelvey-Zavoina R-squared align fitted
  values with the fitted response when `na.action = na.exclude`.
- Missing responses are no longer counted as non-events in segment, gains,
  confusion matrix and validation summaries, and unused response levels no
  longer change which class is treated as the event.
- Validation functions now reject held-out response values that are not one of
  the two response classes the model was fitted on, instead of silently
  counting them as non-events.
- The Hosmer-Lemeshow test accepts numeric and labelled factor responses and
  explicit `data`, uses degrees of freedom based on the observed number of
  groups, and reports the test as unavailable when too few groups remain.
- Weight of evidence and information value are computed at full precision
  before being rounded, avoiding infinite values from small proportions.
- `blr_pairs()` uses wide counters, so pair counts above the 32-bit integer
  limit no longer overflow.
- Residual degrees of freedom are taken from the fitted rank, so aliased
  coefficients are not counted as estimated parameters.
- Constant predictors now get infinite variance inflation factors and
  unavailable variance proportions, and `blr_vif_tol()` works for a single
  predictor.
- Condition indices stay finite for full-rank designs with a small positive
  eigenvalue, and zero-variance predictor columns are handled without an error,
  reporting an infinite index and unavailable variance proportions.
- The maintainer email address is now `aravindhebbali@rsquaredacademy.com`.
- `blr_prep_lorenz_data()` defaults to the model frame and consistently uses
  the fitted rows for training data. Its example now calls the function with
  the default data argument instead of the non-existent `model$data`.
- Executable selection and dfbetas examples now use the binomial family and
  the correct model class.
- Likelihood ratio nesting checks now normalize the design columns before
  comparing column spaces, so rescaling a predictor no longer changes whether
  a no-intercept or non-nested comparison is accepted. Each column is scaled
  by its largest absolute value before its norm is taken, so very large or
  very small predictor values (for example of order `1e155` or `1e-200`) no
  longer overflow or underflow and let a non-nested comparison through.
  The full column space now has the fitted model's rank, so a nearly
  collinear direction the fit estimates is no longer dropped and a valid
  nested comparison is no longer rejected. When nesting cannot be confirmed
  to numerical precision the error says so. Nesting, and whether the
  intercept-only model is nested for `blr_model_fit_stats()`, is checked on
  the observations with positive prior weight using the fitted model's
  non-aliased columns, so zero weights or zero-trial groups no longer cause
  valid nested comparisons to be rejected. The check compares the column
  spaces themselves, through orthonormal bases with a rounding tolerance
  based on the designs' condition numbers, so adding a large constant to a
  predictor no longer lets a non-nested comparison through.

# blorr 0.3.1

This is a patch release to fix minor bugs.

# blorr 0.3.0

This is a minor release to reduce package dependencies and fix bugs.

# blorr 0.2.2

This is a patch release to fix CRAN errors.

# blorr 0.2.1

This is a patch release to fix bugs in the app.

# blorr 0.2.0

This is a minor release to fix bugs and for other enhancements.

## Enhancements

- variable selection procedures now return the final model as an object of 
class `glm` ([#58](https://github.com/rsquaredacademy/blorr/issues/58))
- data preparation functions of selected plots are now exported to enable end 
users to create customized plots and to use plotting library of their 
choice ([#64](https://github.com/rsquaredacademy/blorr/issues/64))
- informs users about new versions ([#1](https://github.com/rsquaredacademy/blorr/issues/1))

## Bug Fixes

- tibble package breaking changes ([#74](https://github.com/rsquaredacademy/blorr/issues/74)) 


# blorr 0.1.0

Initial release
