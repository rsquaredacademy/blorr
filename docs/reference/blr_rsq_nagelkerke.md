# Cragg-Uhler (Nagelkerke) R2

Cragg-Uhler (Nagelkerke) R2 pseudo r-squared.

## Usage

``` r
blr_rsq_nagelkerke(model)
```

## Arguments

- model:

  An object of class `glm`.

## Value

Cragg-Uhler (Nagelkerke) R2 pseudo r-squared.

## References

Cragg, S. G., & Uhler, R. (1970). The demand for automobiles. Canadian
Journal of Economics, 3, 386-406.

Maddala, G. S. (1983). Limited dependent and qualitative variables in
economics. New York: Cambridge Press.

Nagelkerke, N. (1991). A note on a general definition of the coefficient
of determination.

## See also

Other model fit statistics:
[`blr_model_fit_stats()`](https://blorr.rsquaredacademy.com/reference/blr_model_fit_stats.md),
[`blr_multi_model_fit_stats()`](https://blorr.rsquaredacademy.com/reference/blr_multi_model_fit_stats.md),
[`blr_pairs()`](https://blorr.rsquaredacademy.com/reference/blr_pairs.md),
[`blr_rsq_adj_count()`](https://blorr.rsquaredacademy.com/reference/blr_rsq_adj_count.md),
[`blr_rsq_cox_snell()`](https://blorr.rsquaredacademy.com/reference/blr_rsq_cox_snell.md),
[`blr_rsq_effron()`](https://blorr.rsquaredacademy.com/reference/blr_rsq_effron.md),
[`blr_rsq_mcfadden_adj()`](https://blorr.rsquaredacademy.com/reference/blr_rsq_mcfadden_adj.md),
[`blr_rsq_mckelvey_zavoina()`](https://blorr.rsquaredacademy.com/reference/blr_rsq_mckelvey_zavoina.md),
[`blr_test_lr()`](https://blorr.rsquaredacademy.com/reference/blr_test_lr.md)

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
            family = binomial(link = 'logit'))

blr_rsq_nagelkerke(model)
#> [1] 0.4362587
```
