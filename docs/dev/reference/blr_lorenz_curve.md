# `Lorenz curve`

# `Lorenz curve`

Source:
[`R/blr-lorenz-curve.R`](https://github.com/rsquaredacademy/blorr/blob/HEAD/R/blr-lorenz-curve.R)

`blr_lorenz_curve.Rd`

Lorenz curve is a visual representation of inequality. It is used to
measure the discriminatory power of the predictive model.

## Usage

``` r
blr_lorenz_curve(
  model,
  data = NULL,
  title = "Lorenz Curve",
  xaxis_title = "Cumulative Events %",
  yaxis_title = "Cumulative Non Events %",
  diag_line_col = "red",
  lorenz_curve_col = "blue",
  print_plot = TRUE
)
```

## Arguments

- model:

  An object of class `glm`. Only binary logistic regression models
  (binomial family with logit link) are supported. Weighted models (any
  prior weight other than 1) and grouped binomial models
  (`cbind(successes, failures)` or proportions) are not supported; the
  model must be fitted with `model = TRUE, y = TRUE`.

- data:

  A `tibble` or `data.frame`.

- title:

  Plot title.

- xaxis_title:

  X axis title.

- yaxis_title:

  Y axis title.

- diag_line_col:

  Diagonal line color.

- lorenz_curve_col:

  Color of the lorenz curve.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## References

Hand, D. J. and Till, R. J. (2001). A simple generalisation of the area
under the ROC curve for multiple class classification problems. *Machine
Learning*, 45(2), 171–186.

Hanley, J. A. and McNeil, B. J. (1982). The meaning and use of the area
under a receiver operating characteristic (ROC) curve. *Radiology*,
143(1), 29–36.

Mann, H. B. and Whitney, D. R. (1947). On a test of whether one of two
random variables is stochastically larger than the other. *The Annals of
Mathematical Statistics*, 18(1), 50–60.

## See also

Other model validation techniques:
[`blr_confusion_matrix()`](https://blorr.rsquaredacademy.com/dev/reference/blr_confusion_matrix.md),
[`blr_decile_capture_rate()`](https://blorr.rsquaredacademy.com/dev/reference/blr_decile_capture_rate.md),
[`blr_decile_lift_chart()`](https://blorr.rsquaredacademy.com/dev/reference/blr_decile_lift_chart.md),
[`blr_gains_table()`](https://blorr.rsquaredacademy.com/dev/reference/blr_gains_table.md),
[`blr_gini_index()`](https://blorr.rsquaredacademy.com/dev/reference/blr_gini_index.md),
[`blr_ks_chart()`](https://blorr.rsquaredacademy.com/dev/reference/blr_ks_chart.md),
[`blr_roc_curve()`](https://blorr.rsquaredacademy.com/dev/reference/blr_roc_curve.md),
[`blr_test_hosmer_lemeshow()`](https://blorr.rsquaredacademy.com/dev/reference/blr_test_hosmer_lemeshow.md)

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
family = binomial(link = 'logit'))

blr_lorenz_curve(model)

```
