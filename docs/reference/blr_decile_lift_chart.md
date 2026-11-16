# Decile lift chart

Decile wise lift chart.

## Usage

``` r
blr_decile_lift_chart(
  gains_table,
  xaxis_title = "Decile",
  yaxis_title = "Decile Mean / Global Mean",
  title = "Decile Lift Chart",
  bar_color = "blue",
  text_size = 3.5,
  text_vjust = -0.3,
  print_plot = TRUE
)
```

## Arguments

- gains_table:

  An object of class `blr_gains_table`.

- xaxis_title:

  X axis title.

- yaxis_title:

  Y axis title.

- title:

  Plot title.

- bar_color:

  Color of the bars.

- text_size:

  Size of the bar labels.

- text_vjust:

  Vertical justification of the bar labels.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## See also

Other model validation techniques:
[`blr_confusion_matrix()`](https://blorr.rsquaredacademy.com/reference/blr_confusion_matrix.md),
[`blr_decile_capture_rate()`](https://blorr.rsquaredacademy.com/reference/blr_decile_capture_rate.md),
[`blr_gains_table()`](https://blorr.rsquaredacademy.com/reference/blr_gains_table.md),
[`blr_gini_index()`](https://blorr.rsquaredacademy.com/reference/blr_gini_index.md),
[`blr_ks_chart()`](https://blorr.rsquaredacademy.com/reference/blr_ks_chart.md),
[`blr_lorenz_curve()`](https://blorr.rsquaredacademy.com/reference/blr_lorenz_curve.md),
[`blr_roc_curve()`](https://blorr.rsquaredacademy.com/reference/blr_roc_curve.md),
[`blr_test_hosmer_lemeshow()`](https://blorr.rsquaredacademy.com/reference/blr_test_hosmer_lemeshow.md)

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
             family = binomial(link = 'logit'))
gt <- blr_gains_table(model)
blr_decile_lift_chart(gt)

```
