# `Gini index`

# `Gini index`

Source:
[`R/blr-lorenz-curve.R`](https://github.com/rsquaredacademy/blorr/blob/HEAD/R/blr-lorenz-curve.R)

`blr_gini_index.Rd`

Gini index is a measure of inequality and was developed to measure
income inequality in labour market. In the predictive model, Gini Index
is used for measuring discriminatory power.

## Usage

``` r
blr_gini_index(model, data = NULL)
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

## Value

Gini index.

## Details

The Gini index is computed as 2 \* AUC - 1, where AUC is the area under
the receiver operating characteristic curve.

## References

Siddiqi N (2006): Credit Risk Scorecards: developing and implementing
intelligent credit scoring. New Jersey, Wiley.

Müller M, Rönz B (2000): Credit Scoring using Semiparametric Methods.
In: Franke J, Härdle W, Stahl G (Eds.): Measuring Risk in Complex
Stochastic Systems. New York, Springer-Verlag.

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
#> [1] 0.7136439
```
