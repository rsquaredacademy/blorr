# `Multi model fit statistics`

# `Multi model fit statistics`

Source:
[`R/blr-multi-model-fit-stats.R`](https://github.com/rsquaredacademy/blorr/blob/HEAD/R/blr-multi-model-fit-stats.R)

`blr_multi_model_fit_stats.Rd`

Measures of model fit statistics for multiple models.

## Usage

``` r
blr_multi_model_fit_stats(model, ...)

# Default S3 method
blr_multi_model_fit_stats(model, ...)
```

## Arguments

- model:

  An object of class `glm`. Only binary logistic regression models
  (binomial family with logit link) are supported. Weighted models (any
  prior weight other than 1) and grouped binomial models
  (`cbind(successes, failures)` or proportions) are not supported; the
  model must be fitted with `model = TRUE, y = TRUE`.

- ...:

  Objects of class `glm`.

## Value

A tibble.

## See also

Other model fit statistics:
[`blr_model_fit_stats()`](https://blorr.rsquaredacademy.com/dev/reference/blr_model_fit_stats.md),
[`blr_pairs()`](https://blorr.rsquaredacademy.com/dev/reference/blr_pairs.md),
[`blr_rsq_adj_count()`](https://blorr.rsquaredacademy.com/dev/reference/blr_rsq_adj_count.md),
[`blr_rsq_cox_snell()`](https://blorr.rsquaredacademy.com/dev/reference/blr_rsq_cox_snell.md),
[`blr_rsq_effron()`](https://blorr.rsquaredacademy.com/dev/reference/blr_rsq_effron.md),
[`blr_rsq_mcfadden_adj()`](https://blorr.rsquaredacademy.com/dev/reference/blr_rsq_mcfadden_adj.md),
[`blr_rsq_mckelvey_zavoina()`](https://blorr.rsquaredacademy.com/dev/reference/blr_rsq_mckelvey_zavoina.md),
[`blr_rsq_nagelkerke()`](https://blorr.rsquaredacademy.com/dev/reference/blr_rsq_nagelkerke.md),
[`blr_test_lr()`](https://blorr.rsquaredacademy.com/dev/reference/blr_test_lr.md)

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
family = binomial(link = 'logit'))

model2 <- glm(honcomp ~ female + read + math, data = hsb2,
family = binomial(link = 'logit'))

blr_multi_model_fit_stats(model, model2)
#>                                Measures  Model 1  Model 2
#> loglik_null      Log-Lik Intercept Only -115.644 -115.644
#> loglik_model         Log-Lik Full Model  -80.118  -75.210
#> m_deviance                     Deviance  160.236  150.420
#> lr_ratio                             LR   71.052   80.869
#> lr_pval                       Prob > LR    0.000    0.000
#> mcfadden                  MCFadden's R2    0.307    0.350
#> adj_mcfadden          McFadden's Adj R2    0.273    0.315
#> cox_snell             ML (Cox-Snell) R2    0.299    0.333
#> nagelkerke   Cragg-Uhler(Nagelkerke) R2    0.436    0.485
#> mckelvey        McKelvey & Zavoina's R2    0.518    0.523
#> effron                       Efron's R2    0.330    0.379
#> count_r2                       Count R2    0.810    0.830
#> count_adj                  Adj Count R2    0.283    0.358
#> m_aic                               AIC  168.236  158.420
#> m_bic                               BIC  181.430  171.613
```
