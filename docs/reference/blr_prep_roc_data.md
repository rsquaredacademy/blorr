# ROC curve data

Data for generating ROC curve.

## Usage

``` r
blr_prep_roc_data(gains_table)
```

## Arguments

- gains_table:

  An object of clas `blr_gains_table`

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
             family = binomial(link = 'logit'))
gt <- blr_gains_table(model)
blr_prep_roc_data(gt)
#>    sensitivity specificity sensitivity_per 1 - specificity
#> 1           NA          NA       0.0000000      0.00000000
#> 2     26.41509    95.91837       0.2641509      0.04081633
#> 3     50.94340    91.15646       0.5094340      0.08843537
#> 4     69.81132    84.35374       0.6981132      0.15646259
#> 5     83.01887    75.51020       0.8301887      0.24489796
#> 6     88.67925    63.94558       0.8867925      0.36054422
#> 7     94.33962    52.38095       0.9433962      0.47619048
#> 8     96.22642    39.45578       0.9622642      0.60544218
#> 9    100.00000    27.21088       1.0000000      0.72789116
#> 10   100.00000    13.60544       1.0000000      0.86394558
#> 11   100.00000     0.00000       1.0000000      1.00000000
```
