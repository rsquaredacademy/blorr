# Lorenz curve data

Data for generating Lorenz curve.

## Usage

``` r
blr_prep_lorenz_data(model, data = NULL, test_data = FALSE)
```

## Arguments

- model:

  An object of class `glm`.

- data:

  A `tibble` or `data.frame`.

- test_data:

  Logical; `TRUE` if data is test data and `FALSE` if training data.

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
             family = binomial(link = 'logit'))
data <- model$data
blr_prep_lorenz_data(model, data, FALSE)
#>    cum_0s_per cum_1s_per
#> 1  0.00000000  0.0000000
#> 2  0.04081633  0.2641509
#> 3  0.08843537  0.5094340
#> 4  0.15646259  0.6981132
#> 5  0.24489796  0.8301887
#> 6  0.36054422  0.8867925
#> 7  0.47619048  0.9433962
#> 8  0.60544218  0.9622642
#> 9  0.72789116  1.0000000
#> 10 0.86394558  1.0000000
#> 11 1.00000000  1.0000000
#> 12 1.00000000  1.0000000
```
