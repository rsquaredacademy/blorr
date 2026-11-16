# Effron R2

Effron pseudo r-squared.

## Usage

``` r
blr_rsq_effron(model)
```

## Arguments

- model:

  An object of class `glm`.

## Value

Effron pseudo r-squared.

## References

Efron, B. (1978). Regression and ANOVA with zero-one data: Measures of
residual variation. Journal of the American Statistical Association, 73,
113-121.

## See also

Other model fit statistics:
[`blr_model_fit_stats()`](https://blorr.rsquaredacademy.com/reference/blr_model_fit_stats.md),
[`blr_multi_model_fit_stats()`](https://blorr.rsquaredacademy.com/reference/blr_multi_model_fit_stats.md),
[`blr_pairs()`](https://blorr.rsquaredacademy.com/reference/blr_pairs.md),
[`blr_rsq_adj_count()`](https://blorr.rsquaredacademy.com/reference/blr_rsq_adj_count.md),
[`blr_rsq_cox_snell()`](https://blorr.rsquaredacademy.com/reference/blr_rsq_cox_snell.md),
[`blr_rsq_mcfadden_adj()`](https://blorr.rsquaredacademy.com/reference/blr_rsq_mcfadden_adj.md),
[`blr_rsq_mckelvey_zavoina()`](https://blorr.rsquaredacademy.com/reference/blr_rsq_mckelvey_zavoina.md),
[`blr_rsq_nagelkerke()`](https://blorr.rsquaredacademy.com/reference/blr_rsq_nagelkerke.md),
[`blr_test_lr()`](https://blorr.rsquaredacademy.com/reference/blr_test_lr.md)

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
            family = binomial(link = 'logit'))

blr_rsq_effron(model)
#> [1] 0.3304513
```
