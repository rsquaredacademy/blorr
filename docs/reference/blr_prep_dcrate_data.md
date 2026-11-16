# Decile capture rate data

Data for generating decile capture rate.

## Usage

``` r
blr_prep_dcrate_data(gains_table)
```

## Arguments

- gains_table:

  An object of clas `blr_gains_table`

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
             family = binomial(link = 'logit'))
gt <- blr_gains_table(model)
blr_prep_dcrate_data(gt)
#>    decile total  1 decile_mean
#> 1       1    20 14        0.70
#> 2       2    20 13        0.65
#> 3       3    20 10        0.50
#> 4       4    20  7        0.35
#> 5       5    20  3        0.15
#> 6       6    20  3        0.15
#> 7       7    20  1        0.05
#> 8       8    20  2        0.10
#> 9       9    20  0        0.00
#> 10     10    20  0        0.00
```
