# `Hosmer-Lemeshow test`

# `Hosmer-Lemeshow test`

Source:
[`R/blr-hosmer-lemeshow-test.R`](https://github.com/rsquaredacademy/blorr/blob/HEAD/R/blr-hosmer-lemeshow-test.R)

`blr_test_hosmer_lemeshow.Rd`

Hosmer-Lemeshow goodness of fit test.

## Usage

``` r
blr_test_hosmer_lemeshow(model, data = NULL)
```

## Arguments

- model:

  An object of class `glm`. Only binary logistic regression models
  (binomial family with logit link) are supported. Weighted models (any
  prior weight other than 1) and grouped binomial models
  (`cbind(successes, failures)` or proportions) are not supported; the
  model must be fitted with `model = TRUE, y = TRUE`.

- data:

  a `tibble` or `data.frame`.

## Details

The degrees of freedom are `g - 2`, where `g` is the number of partition
groups actually used. The table prints exactly those groups, so a model
with fewer than 10 groups no longer prints empty rows. With fewer than
three groups the test is not available and the statistic, degrees of
freedom and p-value are `NA`: with few groups the test is degenerate,
for example a model with a single binary predictor gives only two groups
and a p-value close to one, which should not be read as evidence of good
fit. Weighted models are rejected before the test by the shared model
guard.

## References

Hosmer, D.W., Jr., & Lemeshow, S. (2000), Applied logistic
regression(2nd ed.). New York: John Wiley & Sons.

## See also

Other model validation techniques:
[`blr_confusion_matrix()`](https://blorr.rsquaredacademy.com/reference/blr_confusion_matrix.md),
[`blr_decile_capture_rate()`](https://blorr.rsquaredacademy.com/reference/blr_decile_capture_rate.md),
[`blr_decile_lift_chart()`](https://blorr.rsquaredacademy.com/reference/blr_decile_lift_chart.md),
[`blr_gains_table()`](https://blorr.rsquaredacademy.com/reference/blr_gains_table.md),
[`blr_gini_index()`](https://blorr.rsquaredacademy.com/reference/blr_gini_index.md),
[`blr_ks_chart()`](https://blorr.rsquaredacademy.com/reference/blr_ks_chart.md),
[`blr_lorenz_curve()`](https://blorr.rsquaredacademy.com/reference/blr_lorenz_curve.md),
[`blr_roc_curve()`](https://blorr.rsquaredacademy.com/reference/blr_roc_curve.md)

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
             family = binomial(link = 'logit'))

blr_test_hosmer_lemeshow(model)
#>            Partition for the Hosmer & Lemeshow Test            
#> --------------------------------------------------------------
#>                         def = 1                 def = 0        
#> Group    Total    Observed    Expected    Observed    Expected 
#> --------------------------------------------------------------
#>   1       20         0          0.16         20        19.84   
#>   2       20         0          0.53         20        19.47   
#>   3       20         2          0.99         18        19.01   
#>   4       20         1          1.64         19        18.36   
#>   5       21         3          2.72         18        18.28   
#>   6       19         3          4.05         16        14.95   
#>   7       20         7          6.50         13        13.50   
#>   8       20         10         8.90         10        11.10   
#>   9       20         13        11.49         7          8.51   
#>  10       20         14        16.02         6          3.98   
#> --------------------------------------------------------------
#> 
#>      Goodness of Fit Test      
#> ------------------------------
#> Chi-Square    DF    Pr > ChiSq 
#> ------------------------------
#>   4.4998      8       0.8095   
#> ------------------------------
```
