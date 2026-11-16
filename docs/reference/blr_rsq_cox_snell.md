# Cox Snell R2

Cox Snell pseudo r-squared.

## Usage

``` r
blr_rsq_cox_snell(model)
```

## Arguments

- model:

  An object of class `glm`.

## Value

Cox Snell pseudo r-squared.

## References

Cox, D. R., & Snell, E. J. (1989). The analysis of binary data (2nd
ed.). London: Chapman and Hall.

Maddala, G. S. (1983). Limited dependent and qualitative variables in
economics. New York: Cambridge Press.

## See also

Other model fit statistics:
[`blr_model_fit_stats()`](https://blorr.rsquaredacademy.com/reference/blr_model_fit_stats.md),
[`blr_multi_model_fit_stats()`](https://blorr.rsquaredacademy.com/reference/blr_multi_model_fit_stats.md),
[`blr_pairs()`](https://blorr.rsquaredacademy.com/reference/blr_pairs.md),
[`blr_rsq_adj_count()`](https://blorr.rsquaredacademy.com/reference/blr_rsq_adj_count.md),
[`blr_rsq_effron()`](https://blorr.rsquaredacademy.com/reference/blr_rsq_effron.md),
[`blr_rsq_mcfadden_adj()`](https://blorr.rsquaredacademy.com/reference/blr_rsq_mcfadden_adj.md),
[`blr_rsq_mckelvey_zavoina()`](https://blorr.rsquaredacademy.com/reference/blr_rsq_mckelvey_zavoina.md),
[`blr_rsq_nagelkerke()`](https://blorr.rsquaredacademy.com/reference/blr_rsq_nagelkerke.md),
[`blr_test_lr()`](https://blorr.rsquaredacademy.com/reference/blr_test_lr.md)

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
            family = binomial(link = 'logit'))

blr_rsq_cox_snell(model)
#> [1] 0.2990105
```
