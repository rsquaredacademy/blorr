# Lift Chart data

Data for generating lift chart.

## Usage

``` r
blr_prep_lchart_gmean(gains_table)

blr_prep_lchart_data(gains_table, global_mean)
```

## Arguments

- gains_table:

  An object of clas `blr_gains_table`.

- global_mean:

  Overall conversion rate.

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
             family = binomial(link = 'logit'))
gt <- blr_gains_table(model)
globalmean <- blr_prep_lchart_gmean(gt)
blr_prep_lchart_data(gt, globalmean)
#>    decile total  1 decile_mean d_by_g_mean
#> 1       1    20 14        0.70   2.6415094
#> 2       2    20 13        0.65   2.4528302
#> 3       3    20 10        0.50   1.8867925
#> 4       4    20  7        0.35   1.3207547
#> 5       5    20  3        0.15   0.5660377
#> 6       6    20  3        0.15   0.5660377
#> 7       7    20  1        0.05   0.1886792
#> 8       8    20  2        0.10   0.3773585
#> 9       9    20  0        0.00   0.0000000
#> 10     10    20  0        0.00   0.0000000
```
