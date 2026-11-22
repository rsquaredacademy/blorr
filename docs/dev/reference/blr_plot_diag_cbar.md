# `CI Displacement CBAR plot`

# `CI Displacement CBAR plot`

Source:
[`R/blr-plots.R`](https://github.com/rsquaredacademy/blorr/blob/HEAD/R/blr-plots.R)

`blr_plot_diag_cbar.Rd`

Confidence interval displacement diagnostics CBAR plot.

## Usage

``` r
blr_plot_diag_cbar(
  model,
  point_color = "blue",
  title = "CI Displacement CBAR Plot",
  xaxis_title = "id",
  yaxis_title = "CI Displacement CBAR"
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

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
family = binomial(link = 'logit'))

blr_plot_diag_cbar(model)

```
