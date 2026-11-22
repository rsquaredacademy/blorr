# `Fitted values diagnostics plot`

# `Fitted values diagnostics plot`

Source:
[`R/blr-residual-diagnostics.R`](https://github.com/rsquaredacademy/blorr/blob/HEAD/R/blr-residual-diagnostics.R)

`blr_plot_diag_fit.Rd`

Diagnostic plots for fitted values.

## Usage

``` r
blr_plot_diag_fit(model, print_plot = TRUE)
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

A panel of diagnostic plots for fitted values.

## References

Fox, John (1991), Regression Diagnostics. Newbury Park, CA: Sage
Publications.

Cook, R. D. and Weisberg, S. (1982), Residuals and Influence in
Regression, New York: Chapman & Hall.

## See also

Other diagnostic plots:
[`blr_plot_diag_influence()`](https://blorr.rsquaredacademy.com/reference/blr_plot_diag_influence.md),
[`blr_plot_diag_leverage()`](https://blorr.rsquaredacademy.com/reference/blr_plot_diag_leverage.md)

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
family = binomial(link = 'logit'))

blr_plot_diag_fit(model)

```
