# `McFadden's R2`

# `McFadden's R2`

Source:
[`R/blr-model-fit-stats.R`](https://github.com/rsquaredacademy/blorr/blob/HEAD/R/blr-model-fit-stats.R)

`blr_rsq_mcfadden.Rd`

McFadden's pseudo r-squared for the model.

## Usage

``` r
blr_rsq_mcfadden(model)
```

## Arguments

- model:

  An object of class `glm`. Only binary logistic regression models
  (binomial family with logit link) are supported. Weighted models (any
  prior weight other than 1) and grouped binomial models
  (`cbind(successes, failures)` or proportions) are not supported; the
  model must be fitted with `model = TRUE, y = TRUE`.

## Value

McFadden's r-squared.

## References

<https://eml.berkeley.edu/reprints/mcfadden/zarembka.pdf>

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
            family = binomial(link = 'logit'))

blr_rsq_mcfadden(model)
#> [1] 0.3072023
```
