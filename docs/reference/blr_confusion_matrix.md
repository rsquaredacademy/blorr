# Confusion matrix

Confusion matrix and statistics.

## Usage

``` r
blr_confusion_matrix(model, cutoff = 0.5, data = NULL, ...)

# Default S3 method
blr_confusion_matrix(model, cutoff = 0.5, data = NULL, ...)
```

## Arguments

- model:

  An object of class `glm`.

- cutoff:

  Cutoff for classification.

- data:

  A `tibble` or a `data.frame`.

- ...:

  Other arguments.

## Value

Confusion matix.

## See also

Other model validation techniques:
[`blr_decile_capture_rate()`](https://blorr.rsquaredacademy.com/reference/blr_decile_capture_rate.md),
[`blr_decile_lift_chart()`](https://blorr.rsquaredacademy.com/reference/blr_decile_lift_chart.md),
[`blr_gains_table()`](https://blorr.rsquaredacademy.com/reference/blr_gains_table.md),
[`blr_gini_index()`](https://blorr.rsquaredacademy.com/reference/blr_gini_index.md),
[`blr_ks_chart()`](https://blorr.rsquaredacademy.com/reference/blr_ks_chart.md),
[`blr_lorenz_curve()`](https://blorr.rsquaredacademy.com/reference/blr_lorenz_curve.md),
[`blr_roc_curve()`](https://blorr.rsquaredacademy.com/reference/blr_roc_curve.md),
[`blr_test_hosmer_lemeshow()`](https://blorr.rsquaredacademy.com/reference/blr_test_hosmer_lemeshow.md)

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
            family = binomial(link = 'logit'))

blr_confusion_matrix(model, cutoff = 0.4)
#> Confusion Matrix and Statistics 
#> 
#>           Reference
#> Prediction   0   1
#>          0 125  16
#>          1  22  37
#> 
#> 
#>                 Accuracy : 0.8100 
#>      No Information Rate : 0.7350 
#> 
#>                    Kappa : 0.5293 
#> 
#> McNemars's Test P-Value  : 0.4173 
#> 
#>              Sensitivity : 0.6981 
#>              Specificity : 0.8503 
#>           Pos Pred Value : 0.6271 
#>           Neg Pred Value : 0.8865 
#>               Prevalence : 0.2650 
#>           Detection Rate : 0.1850 
#>     Detection Prevalence : 0.2950 
#>        Balanced Accuracy : 0.7742 
#>                Precision : 0.6271 
#>                   Recall : 0.6981 
#> 
#>         'Positive' Class : 1
```
