# WoE & IV

Weight of evidence and information value. Currently avialable for
categorical predictors only.

## Usage

``` r
blr_woe_iv(data, predictor, response, digits = 4, ...)

# S3 method for class 'blr_woe_iv'
plot(
  x,
  title = NA,
  xaxis_title = "Levels",
  yaxis_title = "WoE",
  bar_color = "blue",
  line_color = "red",
  print_plot = TRUE,
  ...
)
```

## Arguments

- data:

  A `tibble` or `data.frame`.

- predictor:

  Predictor variable; column in `data`.

- response:

  Response variable; column in `data`.

- digits:

  Number of decimal digits to round off.

- ...:

  Other inputs.

- x:

  An object of class `blr_segment_dist`.

- title:

  Plot title.

- xaxis_title:

  X axis title.

- yaxis_title:

  Y axis title.

- bar_color:

  Color of the bar.

- line_color:

  Color of the horizontal line.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## Value

A tibble.

## References

Siddiqi N (2006): Credit Risk Scorecards: developing and implementing
intelligent credit scoring. New Jersey, Wiley.

## See also

Other bivariate analysis procedures:
[`blr_bivariate_analysis()`](https://blorr.rsquaredacademy.com/reference/blr_bivariate_analysis.md),
[`blr_segment()`](https://blorr.rsquaredacademy.com/reference/blr_segment.md),
[`blr_segment_dist()`](https://blorr.rsquaredacademy.com/reference/blr_segment_dist.md),
[`blr_segment_twoway()`](https://blorr.rsquaredacademy.com/reference/blr_segment_twoway.md),
[`blr_woe_iv_stats()`](https://blorr.rsquaredacademy.com/reference/blr_woe_iv_stats.md)

## Examples

``` r
# woe and iv
k <- blr_woe_iv(hsb2, female, honcomp)
k
#>                            Weight of Evidence                             
#> -------------------------------------------------------------------------
#> levels    count_0s    count_1s    dist_0s    dist_1s        woe      iv   
#> -------------------------------------------------------------------------
#>   0          73          18          0.50       0.34       0.38     0.06  
#>   1          74          35          0.50       0.66      -0.27     0.04  
#> -------------------------------------------------------------------------
#> 
#>       Information Value       
#> -----------------------------
#> Variable    Information Value 
#> -----------------------------
#>  female          0.1023       
#> -----------------------------

# plot woe
plot(k)

```
