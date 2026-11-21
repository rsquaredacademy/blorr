# `Standardized deviance residual values`

# `Standardized deviance residual values`

Source:
[`R/blr-plots.R`](https://github.com/rsquaredacademy/blorr/blob/HEAD/R/blr-plots.R)

`blr_plot_deviance_residual.Rd`

Standardized deviance residuals plot.

## Usage

``` r
blr_plot_deviance_residual(
  model,
  point_color = "blue",
  title = "Standardized Deviance Residuals Plot",
  xaxis_title = "id",
  yaxis_title = "Standardized Deviance Residuals"
)
```

## Arguments

- model:

  An object of class `glm`. Only binary logistic regression models
  (binomial family with logit link) are supported. Weighted models (any
  prior weight other than 1) and grouped binomial models
  (`cbind(successes, failures)` or proportions) are not supported; the
  model must be fitted with `model = TRUE, y = TRUE`.

- point_color:

  Color of the points.

- title:

  Title of the plot.

- xaxis_title:

  X axis label.

- yaxis_title:

  Y axis label.

## References

Williams, D. A. (1987). Generalized linear model diagnostics using the
deviance and single case deletions. *Journal of the Royal Statistical
Society, Series C (Applied Statistics)*, 36(2), 181–191.

McCullagh, P. and Nelder, J. A. (1989). *Generalized Linear Models*, 2nd
ed. London: Chapman and Hall.

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
family = binomial(link = 'logit'))

blr_plot_deviance_residual(model)

```
