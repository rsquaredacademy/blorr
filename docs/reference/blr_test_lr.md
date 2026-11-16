# Likelihood ratio test

Performs the likelihood ratio test for full and reduced model.

## Usage

``` r
blr_test_lr(full_model, reduced_model)

# Default S3 method
blr_test_lr(full_model, reduced_model)
```

## Arguments

- full_model:

  An object of class `glm`; model with all predictors.

- reduced_model:

  An object of class `glm`; nested model. Optional if you are comparing
  the `full_model` with an intercept only model.

## Value

Two tibbles with model information and test results.

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
[`blr_rsq_nagelkerke()`](https://blorr.rsquaredacademy.com/reference/blr_rsq_nagelkerke.md)

## Examples

``` r
# compare full model with intercept only model
# full model
model_1 <- glm(honcomp ~ female + read + science, data = hsb2,
            family = binomial(link = 'logit'))

blr_test_lr(model_1)

# compare full model with nested model
# nested model
model_2 <- glm(honcomp ~ female + read, data = hsb2,
            family = binomial(link = 'logit'))

blr_test_lr(model_1, model_2)

```
