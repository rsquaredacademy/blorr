# blorr 0.3.2

This is a patch release that fixes several bugs in model fit statistics,
variable selection, bivariate analysis, model validation, gains tables,
weight of evidence, and the Gini index.

## Bug Fixes

- `blr_test_hosmer_lemeshow()` now calculates degrees of freedom from the
  actual number of partition groups used (`max(1, g - 2)`) rather than hardcoding it to 8.
- The intercept-only null model used in model fit statistics and likelihood
  ratio tests now retains the fitted model's response, prior weights, offset, and family/link.
- Package attachment no longer accesses the network to check for updates.
- Cox-Snell and Nagelkerke R2 now use the number of observations the model was
  fitted on, so they are correct when rows are dropped for missing values or
  when the model is fitted without a `data` argument. The model fit statistics
  table is also assembled by name, so values no longer shift between labels.
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
  for each coefficient.
- Somers' D and Goodman-Kruskal gamma in `blr_pairs()` now use the correct
  denominators.
- Gains tables now assign every observation to exactly one of ten deciles,
  count events using the event level of the response, and no longer fail for
  small samples or when a decile contains only one outcome.
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
- Variable selection procedures now return an intercept-only model when no
  predictor qualifies, and p-value based procedures apply the entry criterion
  to the first predictor as well.
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
