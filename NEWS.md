# blorr (development version)

## Supported inputs and precision

- The data-column functions (`blr_segment()`, `blr_segment_dist()`,
  `blr_segment_twoway()`, `blr_woe_iv()`, `blr_woe_iv_stats()` and
  `blr_bivariate_analysis()`) support one or two observed, non-missing
  response levels. Supported encodings are numeric 0/1, logical, factor and
  character; the event is the second level, `TRUE` or 1. A single observed
  level returns the defined rate (all 0 or all 1), and a level labelled `"1"`
  or `TRUE` is the event, so numeric and factor encodings agree. Unused factor
  levels and rows with a missing response are ignored. Numeric codes other
  than 0/1 are not supported. Segment and two-way functions keep a missing
  predictor as its own `NA` level; WoE/IV excludes rows with a missing response
  or predictor. Empty input is an error because it has no response levels.
- Proportions are returned unrounded. Segment tables print percentages to two
  decimal places, two-way rates to three decimal places, WoE/IV components to
  `digits` decimal places, and the total information value is rounded once at
  print time. Plots use the unrounded proportions.

## Behaviour changes

- `blr_segment_twoway()` now reports the event rate within each pair of
  levels: events in a cyl x gear cell divided by the non-missing responses in
  that cell. Versions up to 0.3.2 divided the event counts of each cell by all
  non-missing responses in the sample (the share of all observations), which
  understated every rate. For `am`, `cyl` and `gear` in `mtcars`, the cyl 4 /
  gear 4 cell is now 0.750 (6 events out of 8 observations) instead of 0.188
  (6 out of 32). A cell with no non-missing responses is `NA` and prints as
  `-`.

- `blr_segment()` and `blr_segment_dist()` now return unrounded proportions in
  `1s%` and `n%` and round only when printing. Versions up to 0.3.2 rounded the
  proportions to two decimal places before multiplying by 100, so the printed
  percentages and the plot used false precision: for `am` by `cyl` in
  `mtcars`, cyl 4 is now 34.38% of the sample and has a 72.73% event rate,
  instead of printing 34.00% and 73.00% from the stored 0.34 and 0.73. The plot
  of `blr_segment_dist()` now uses the unrounded proportions.

- A response with one observed level is accepted again by the data-column
  functions and returns the defined empirical rates: 0 when every response is
  a non-event and 1 when every response is an event. `blr_woe_iv()` and
  `blr_woe_iv_stats()` apply the documented 0.5 continuity correction to the
  zero cells, so their corrected within-class distributions are equal and WoE
  and IV are 0. A factor whose only level is `"1"` or `TRUE` is now all events,
  the same as numeric 1, so the earlier numeric/factor disagreement is fixed.
  A response with no observed level remains an error, and the model APIs that
  need two classes still reject a one-level response.

## Bug fixes

- `blr_test_hosmer_lemeshow()` now returns `NA` with a message for a one-class
  response on every platform; before, Windows returned a near-zero statistic
  (#162).

- A response with more than two levels is now rejected with an error naming
  the response and the number of levels found. `glm()` silently recodes a
  factor response with three or more levels as "first level"
  versus "everything else", so a three-level response was fitted without
  complaint. A character response such as `as.character(hsb2$ses)` instead
  fails in `glm()` with "y values must be 0 <= y <= 1".
  For `glm(ses ~ female + read, hsb2, family = binomial())` the
  three-level `ses` response was fitted and the summary printed by
  `blr_regress()` reported 47 and 95 of 200 observations, silently dropping
  the third level (58). Every model entry point now raises this error:
  `blr_regress()`, `blr_model_fit_stats()`, the `blr_rsq_*()` functions, the
  stepwise functions, `blr_gains_table()`, `blr_confusion_matrix()` and
  `blr_test_hosmer_lemeshow()`. Binary responses are unchanged.

- A response with a single outcome is rejected by `blr_regress()` with
  `` `y` has 1 level; blorr needs both events and non-events. `` because its
  response summary prints the two outcomes side by side. The model diagnostics
  accept a degenerate single-outcome fit again, as they did before the
  one-level check was added: `blr_confusion_matrix()` reports the defined
  counts (for `glm(rep(0, 32) ~ wt, mtcars, family = binomial())`, 32 true
  zeroes and accuracy 1) and marks the undefined statistics `NA`, and
  `blr_model_fit_stats()`, the `blr_rsq_*()` functions, `blr_gains_table()` and
  `blr_test_hosmer_lemeshow()` run on it again.

- `blr_woe_iv()`, `blr_woe_iv_stats()`, `blr_segment()`,
  `blr_segment_dist()` and `blr_segment_twoway()` now use the same error for
  response columns with more than two observed, non-missing levels, instead
  of silently treating the second level as the event. Numeric 0/1, two-level
  factor, logical and two-value character responses are unchanged; unused
  factor levels are ignored.

- An explicit missing factor response (`factor(..., exclude = NULL)`) is now
  excluded from the data-column functions like any other missing response.
  Previously `is.na()` did not detect the explicit missing level, so the row
  was counted as a non-event: for `y = factor(c("0", "1", NA, "0", "1"),
  exclude = NULL)` grouped by `g`, the `a` group had `n = 3` and one event
  instead of `n = 2` and one event, and WoE/IV counted two non-events instead
  of one. The event is also the second non-missing level when a factor carries
  an explicit missing level.

- `blr_woe_iv()` and `blr_woe_iv_stats()` keep the event class of the original
  response when rows with a missing predictor are removed. Previously a factor
  response whose non-event class disappeared with those rows was re-levelled
  and its remaining event class became the non-event class; numeric 0/1
  responses already gave the correct counts. Both encodings now agree.

- `blr_woe_iv()` and `blr_woe_iv_stats()` now stop with a named complete-case
  error when no complete observations remain. Previously a two-class response
  with an all-missing predictor produced an empty table and printed
  Information Value 0, which read as zero information rather than no evidence.

- The association table in the `blr_regress()` output now prints `% Concordant`,
  `% Discordant` and `% Tied` on the percentage scale, keeping the `%` labels:
  for `am ~ wt + hp` on `mtcars` it shows 98.3806, 1.6194 and 0.0000. Versions
  up to 0.3.2 printed the proportions 0.9838, 0.0162 and 0.0000 under the same
  labels. The `n%` and `1s%` columns printed by `blr_segment()` and
  `blr_segment_dist()` follow the same convention; their returned proportions
  are now unrounded (see Behaviour changes).

- The total information value printed by `blr_woe_iv()` and
  `blr_woe_iv_stats()` is now computed from the unrounded information value
  components and rounded once for printing, instead of summing components that
  had already been rounded to `digits`. For the two-level example with
  `digits = 1` the total is now 0.5 rather than 0.4. `blr_bivariate_analysis()`
  likewise uses the unrounded total.

- `blr_segment()`, `blr_segment_dist()` and `blr_segment_twoway()` keep a
  missing predictor as its own `NA` level instead of failing inside the width
  calculation of the printer. The `NA` rows get their own count, sample share
  and event rate, and print with an `NA` label. Rows with a missing response
  are still excluded first. The two-way function keeps the within-cell rate
  fix and adds an `NA` row or column for a missing grouping value.

- The `blr_segment_twoway()` printer now handles a table with no rows or no
  columns without failing in `max()` or `rep()`. A no-event cell in a
  non-empty table shows a zero rate, and an empty cell shows `-`.
- `blr_segment_dist()` now prints the predictor's factor levels (for
  `blr_segment_dist(mtcarz, am, cyl)`, 4, 6 and 8 instead of the integer codes
  1, 2 and 3) and the predictor's name as the column header (`cyl` instead of
  the internal `pred`); the same name is used as the plot title.
  `blr_segment_twoway()` now prints the two grouping variables as its headers
  (`gear` across the top and `cyl` down the left) instead of the response and
  the first predictor. Earlier versions printed `am` and `cyl` for
  `blr_segment_twoway(mtcarz, am, cyl, gear)`.
- `blr_residual_diagnostics()` now computes the delta deviance (`difdev`) as
  the squared deviance residual plus CBAR. Versions up to and including 0.3.2
  used the squared standardized Pearson residual instead, so their `difdev`
  values were wrong (for `glm(am ~ wt + hp, mtcars, family = binomial())` the
  first three were 0.272, 2.398 and 0.036 instead of 0.385, 2.273 and 0.063).
  `blr_plot_diag_difdev()`, `blr_plot_difdev_fitted()`,
  `blr_plot_difdev_leverage()` and `blr_plot_diag_influence()` show the
  corrected values.

# blorr 0.3.2

This is a patch release that fixes several bugs in model fit statistics,
variable selection, bivariate analysis, model validation, gains tables,
weight of evidence, and the Gini index.

## Bug Fixes

- `blr_regress()` now summarises the fitted model directly, so an offset
  supplied in the model formula and the formula environment are respected.
- The intercept-only null model used in model fit statistics and likelihood
  ratio tests retains the fitted model's response, offset and family/link, and
  is not confused by data columns named `weights` or `offset`.
- Package attachment no longer accesses the network to check for updates.
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
- Only binary logistic regression models (binomial family with logit link) are supported. All model entry points reject other links and families, including quasibinomial, with a clear error; selection rejects them before refitting.
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
- Variable selection now adds or removes exactly one term per step when
  candidate p values or AIC values tie. The printed step history, AIC and the
  other per-step values are no longer mislabelled or shifted when several
  candidates share a p value (which can underflow to 0 on large data): p-value
  ties are broken by the Wald chi-square statistic and AIC ties by candidate
  order.
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
- Variable selection preserves the original formula and offsets, so `predict()`
  on new data uses the new offset values. If a selected model cannot be rebuilt
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
- Forward, backward and stepwise selection now keep the fitted model's
  offset and contrasts in every candidate model, and print the same
  fitted model they return.
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
- `blr_regress()` can summarise a fit whose response is a transformed
  expression.
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
- informs users about new versions
  ([#1](https://github.com/rsquaredacademy/blorr/issues/1))

## Bug Fixes

- tibble package breaking changes
  ([#74](https://github.com/rsquaredacademy/blorr/issues/74))


# blorr 0.1.0

Initial release
