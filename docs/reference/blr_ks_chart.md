# KS chart

Kolmogorov-Smirnov (KS) statistics is used to assess predictive power
for marketing or credit risk models. It is the maximum difference
between cumulative event and non-event distribution across
score/probability bands. The gains table typically has across score
bands and can be used to find the KS for a model.

## Usage

``` r
blr_ks_chart(
  gains_table,
  title = "KS Chart",
  yaxis_title = " ",
  xaxis_title = "Cumulative Population %",
  ks_line_color = "black",
  print_plot = TRUE
)
```

## Arguments

- gains_table:

  An object of class `blr_gains_table`.

- title:

  Plot title.

- yaxis_title:

  Y axis title.

- xaxis_title:

  X axis title.

- ks_line_color:

  Color of the line indicating maximum KS statistic.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## References

<https://pubmed.ncbi.nlm.nih.gov/843576/>

## See also

Other model validation techniques:
[`blr_confusion_matrix()`](https://blorr.rsquaredacademy.com/reference/blr_confusion_matrix.md),
[`blr_decile_capture_rate()`](https://blorr.rsquaredacademy.com/reference/blr_decile_capture_rate.md),
[`blr_decile_lift_chart()`](https://blorr.rsquaredacademy.com/reference/blr_decile_lift_chart.md),
[`blr_gains_table()`](https://blorr.rsquaredacademy.com/reference/blr_gains_table.md),
[`blr_gini_index()`](https://blorr.rsquaredacademy.com/reference/blr_gini_index.md),
[`blr_lorenz_curve()`](https://blorr.rsquaredacademy.com/reference/blr_lorenz_curve.md),
[`blr_roc_curve()`](https://blorr.rsquaredacademy.com/reference/blr_roc_curve.md),
[`blr_test_hosmer_lemeshow()`](https://blorr.rsquaredacademy.com/reference/blr_test_hosmer_lemeshow.md)

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
             family = binomial(link = 'logit'))
gt <- blr_gains_table(model)
blr_ks_chart(gt)

```
