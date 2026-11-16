# Model fit statistics

Model fit statistics.

## Usage

``` r
blr_model_fit_stats(model, ...)
```

## Arguments

- model:

  An object of class `glm`.

- ...:

  Other inputs.

## References

Menard, S. (2000). Coefficients of determination for multiple logistic
regression analysis. The American Statistician, 54(1), 17-24.

Windmeijer, F. A. G. (1995). Goodness-of-fit measures in binary choice
models. Econometric Reviews, 14, 101-116.

Hosmer, D.W., Jr., & Lemeshow, S. (2000), Applied logistic
regression(2nd ed.). New York: John Wiley & Sons.

J. Scott Long & Jeremy Freese, 2000. "FITSTAT: Stata module to compute
fit statistics for single equation regression models," Statistical
Software Components S407201, Boston College Department of Economics,
revised 22 Feb 2001.

Freese, Jeremy and J. Scott Long. Regression Models for Categorical
Dependent Variables Using Stata. College Station: Stata Press, 2006.

Long, J. Scott. Regression Models for Categorical and Limited Dependent
Variables. Thousand Oaks: Sage Publications, 1997.

## See also

Other model fit statistics:
[`blr_multi_model_fit_stats()`](https://blorr.rsquaredacademy.com/reference/blr_multi_model_fit_stats.md),
[`blr_pairs()`](https://blorr.rsquaredacademy.com/reference/blr_pairs.md),
[`blr_rsq_adj_count()`](https://blorr.rsquaredacademy.com/reference/blr_rsq_adj_count.md),
[`blr_rsq_cox_snell()`](https://blorr.rsquaredacademy.com/reference/blr_rsq_cox_snell.md),
[`blr_rsq_effron()`](https://blorr.rsquaredacademy.com/reference/blr_rsq_effron.md),
[`blr_rsq_mcfadden_adj()`](https://blorr.rsquaredacademy.com/reference/blr_rsq_mcfadden_adj.md),
[`blr_rsq_mckelvey_zavoina()`](https://blorr.rsquaredacademy.com/reference/blr_rsq_mckelvey_zavoina.md),
[`blr_rsq_nagelkerke()`](https://blorr.rsquaredacademy.com/reference/blr_rsq_nagelkerke.md),
[`blr_test_lr()`](https://blorr.rsquaredacademy.com/reference/blr_test_lr.md)

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
            family = binomial(link = 'logit'))

blr_model_fit_stats(model)
#>                               Model Fit Statistics                                
#> ---------------------------------------------------------------------------------
#> Log-Lik Intercept Only:      -115.644    Log-Lik Full Model:              -80.118 
#> Deviance(196):                160.236    LR(3):                            71.052 
#>                                          Prob > LR:                         0.000 
#> MCFadden's R2                   0.307    McFadden's Adj R2:                 0.273 
#> ML (Cox-Snell) R2:              0.299    Cragg-Uhler(Nagelkerke) R2:        0.436 
#> McKelvey & Zavoina's R2:        0.518    Efron's R2:                        0.330 
#> Count R2:                       0.810    Adj Count R2:                      0.283 
#> BIC:                          181.430    AIC:                             168.236 
#> ---------------------------------------------------------------------------------
#> 
```
