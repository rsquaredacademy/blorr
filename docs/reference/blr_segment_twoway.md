# Two way event rate

Event rate across two qualitative variables.

## Usage

``` r
blr_segment_twoway(data, response, variable_1, variable_2)

# Default S3 method
blr_segment_twoway(data, response, variable_1, variable_2)
```

## Arguments

- data:

  A `tibble` or `data.frame`.

- response:

  Response variable; column in `data`.

- variable_1:

  Column in `data`.

- variable_2:

  Column in `data`.

## Value

A tibble.

## See also

Other bivariate analysis procedures:
[`blr_bivariate_analysis()`](https://blorr.rsquaredacademy.com/reference/blr_bivariate_analysis.md),
[`blr_segment()`](https://blorr.rsquaredacademy.com/reference/blr_segment.md),
[`blr_segment_dist()`](https://blorr.rsquaredacademy.com/reference/blr_segment_dist.md),
[`blr_woe_iv()`](https://blorr.rsquaredacademy.com/reference/blr_woe_iv.md),
[`blr_woe_iv_stats()`](https://blorr.rsquaredacademy.com/reference/blr_woe_iv_stats.md)

## Examples

``` r
blr_segment_twoway(hsb2, honcomp, prog, female)
#>                 prog     
#> -----------------------
#> honcomp     0       1     
#> -----------------------
#>   1     0.010   0.025   
#> -----------------------
#>   2     0.075   0.125   
#> -----------------------
#>   3     0.005   0.025   
#> -----------------------
```
