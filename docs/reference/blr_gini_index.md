# Gini index

Gini index is a measure of inequality and was developed to measure
income inequality in labour market. In the predictive model, Gini Index
is used for measuring discriminatory power.

## Usage

``` r
blr_gini_index(model, data = NULL)
```

## Arguments

- model:

  An object of class `glm`.

- data:

  A `tibble` or `data.frame`.

## Value

Gini index.

## References

Siddiqi N (2006): Credit Risk Scorecards: developing and implementing
intelligent credit scoring. New Jersey, Wiley.

Müller M, Rönz B (2000): Credit Scoring using Semiparametric Methods.
In: Franke J, Härdle W, Stahl G (Eds.): Measuring Risk in Complex
Stochastic Systems. New York, Springer-Verlag.

## See also

Other model validation techniques:
[`blr_confusion_matrix()`](https://blorr.rsquaredacademy.com/reference/blr_confusion_matrix.md),
[`blr_decile_capture_rate()`](https://blorr.rsquaredacademy.com/reference/blr_decile_capture_rate.md),
[`blr_decile_lift_chart()`](https://blorr.rsquaredacademy.com/reference/blr_decile_lift_chart.md),
[`blr_gains_table()`](https://blorr.rsquaredacademy.com/reference/blr_gains_table.md),
[`blr_ks_chart()`](https://blorr.rsquaredacademy.com/reference/blr_ks_chart.md),
[`blr_lorenz_curve()`](https://blorr.rsquaredacademy.com/reference/blr_lorenz_curve.md),
[`blr_roc_curve()`](https://blorr.rsquaredacademy.com/reference/blr_roc_curve.md),
[`blr_test_hosmer_lemeshow()`](https://blorr.rsquaredacademy.com/reference/blr_test_hosmer_lemeshow.md)

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
family = binomial(link = 'logit'))

blr_gini_index(model)
#> [1] 0.5252134
```
