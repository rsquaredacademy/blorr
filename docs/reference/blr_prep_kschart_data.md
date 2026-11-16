# KS Chart data

Data for generating KS chart.

## Usage

``` r
blr_prep_kschart_data(gains_table)

blr_prep_kschart_line(gains_table)

blr_prep_ksannotate_y(ks_line)

blr_prep_kschart_stat(ks_line)

blr_prep_ksannotate_x(ks_line)
```

## Arguments

- gains_table:

  An object of clas `blr_gains_table`.

- ks_line:

  Overall conversion rate.

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
             family = binomial(link = 'logit'))
gt <- blr_gains_table(model)
blr_prep_kschart_data(gt)
#>    cum_total_per cum_1s_per cum_0s_per
#> 1            0.0  0.0000000 0.00000000
#> 2            0.1  0.2641509 0.04081633
#> 3            0.2  0.5094340 0.08843537
#> 4            0.3  0.6981132 0.15646259
#> 5            0.4  0.8301887 0.24489796
#> 6            0.5  0.8867925 0.36054422
#> 7            0.6  0.9433962 0.47619048
#> 8            0.7  0.9622642 0.60544218
#> 9            0.8  1.0000000 0.72789116
#> 10           0.9  1.0000000 0.86394558
#> 11           1.0  1.0000000 1.00000000
ks_line <- blr_prep_kschart_line(gt)
blr_prep_kschart_stat(ks_line)
#> [1] 59
blr_prep_ksannotate_y(ks_line)
#> [1] 0.5375433
blr_prep_ksannotate_x(ks_line)
#> [1] 0.5
```
