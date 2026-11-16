# McFadden's R2

McFadden's pseudo r-squared for the model.

## Usage

``` r
blr_rsq_mcfadden(model)
```

## Arguments

- model:

  An object of class `glm`.

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
