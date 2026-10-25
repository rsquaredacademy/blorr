# blorr 0.3.1.9000

## Bug Fixes

- Cox-Snell and Nagelkerke R2 use the number of observations the model was
  fitted on, so they are correct when rows are dropped for missing values or
  when the model is fitted without a `data` argument; the model fit statistics
  table is assembled by name so values no longer shift between labels.
- the likelihood ratio test degrees of freedom for nested models is the
  difference in residual degrees of freedom between the reduced and full
  models.
- multi-model fit statistics are labelled by name, so each measure is shown
  against the correct model statistic.
- event rate in `blr_segment()` and `blr_segment_dist()` is the share of
  events within each level rather than the share of all events, and no longer
  fails when a level has no events.
- `blr_regress()` reports the number of observations and the response profile
  from the data used to fit the model when rows are dropped for missing values.
- forward selection stores the BIC and deviance of each step instead of the
  AIC after the first step.
- variance decomposition proportions in `blr_eigen_cindex()` sum to one for
  each coefficient.
- Somers' D and Goodman-Kruskal gamma in `blr_pairs()` use the correct
  denominators.

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
