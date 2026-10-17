# Lorenz curve

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

  An object of class `glm`.

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

## See also

Other model validation techniques:
[`blr_confusion_matrix()`](https://blorr.rsquaredacademy.com/reference/blr_confusion_matrix.md),
[`blr_decile_capture_rate()`](https://blorr.rsquaredacademy.com/reference/blr_decile_capture_rate.md),
[`blr_decile_lift_chart()`](https://blorr.rsquaredacademy.com/reference/blr_decile_lift_chart.md),
[`blr_gains_table()`](https://blorr.rsquaredacademy.com/reference/blr_gains_table.md),
[`blr_gini_index()`](https://blorr.rsquaredacademy.com/reference/blr_gini_index.md),
[`blr_ks_chart()`](https://blorr.rsquaredacademy.com/reference/blr_ks_chart.md),
[`blr_roc_curve()`](https://blorr.rsquaredacademy.com/reference/blr_roc_curve.md),
[`blr_test_hosmer_lemeshow()`](https://blorr.rsquaredacademy.com/reference/blr_test_hosmer_lemeshow.md)

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
family = binomial(link = 'logit'))

blr_lorenz_curve(model)

```
