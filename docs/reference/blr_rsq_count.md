# Count R2

Count r-squared.

## Usage

``` r
blr_rsq_count(model)
```

## Arguments

- model:

  An object of class `glm`.

## Value

Count r-squared.

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
            family = binomial(link = 'logit'))

blr_rsq_count(model)
#> [1] 0.81
```
