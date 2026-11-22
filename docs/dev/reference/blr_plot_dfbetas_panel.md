# `DFBETAs panel`

# `DFBETAs panel`

Source:
[`R/blr-plots.R`](https://github.com/rsquaredacademy/blorr/blob/HEAD/R/blr-plots.R)

`blr_plot_dfbetas_panel.Rd`

Panel of plots to detect influential observations using DFBETAs.

## Usage

``` r
blr_plot_dfbetas_panel(model, print_plot = TRUE)
```

## Arguments

- model:

  An object of class `glm`. Only binary logistic regression models
  (binomial family with logit link) are supported. Weighted models (any
  prior weight other than 1) and grouped binomial models
  (`cbind(successes, failures)` or proportions) are not supported; the
  model must be fitted with `model = TRUE, y = TRUE`.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## Value

list; `blr_dfbetas_panel` returns a list of tibbles (for intercept and
each predictor) with the observation number and DFBETA of observations
that exceed the threshold for classifying an observation as an
outlier/influential observation.

## Details

DFBETA measures the difference in each parameter estimate with and
without the influential point. There is a DFBETA for each data point i.e
if there are n observations and k variables, there will be \\(n \* k\\)
DFBETAs. In general, large values of DFBETAS indicate observations that
are influential in estimating a given parameter. Belsley, Kuh, and
Welsch recommend 2 as a general cutoff value to indicate influential
observations and \\(2/\sqrt(n)\\) as a size-adjusted cutoff.

## References

Belsley, David A.; Kuh, Edwin; Welsh, Roy E. (1980). Regression
Diagnostics: Identifying Influential Data and Sources of Collinearity.
Wiley Series in Probability and Mathematical Statistics. New York: John
Wiley & Sons. pp. ISBN 0-471-05856-4.

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
family = binomial(link = 'logit'))

blr_plot_dfbetas_panel(model)

```
