# `Count R2`

# `Count R2`

Source:
[`R/blr-model-fit-stats.R`](https://github.com/rsquaredacademy/blorr/blob/HEAD/R/blr-model-fit-stats.R)

`blr_rsq_count.Rd`

Count r-squared.

## Usage

``` r
blr_rsq_count(model)
```

## Arguments

- model:

  An object of class `glm`. Only binary logistic regression models
  (binomial family with logit link) are supported. Weighted models (any
  prior weight other than 1) and grouped binomial models
  (`cbind(successes, failures)` or proportions) are not supported; the
  model must be fitted with `model = TRUE, y = TRUE`.

## Value

Count r-squared.

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
            family = binomial(link = 'logit'))

blr_rsq_count(model)
#> [1] 0.81
```
