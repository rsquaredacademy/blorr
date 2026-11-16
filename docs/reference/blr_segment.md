# Event rate

Event rate by segements/levels of a qualitative variable.

## Usage

``` r
blr_segment(data, response, predictor)

# Default S3 method
blr_segment(data, response, predictor)
```

## Arguments

- data:

  A `tibble` or `data.frame`.

- response:

  Response variable; column in `data`.

- predictor:

  Predictor variable; column in `data`.

## Value

A tibble.

## See also

Other bivariate analysis procedures:
[`blr_bivariate_analysis()`](https://blorr.rsquaredacademy.com/reference/blr_bivariate_analysis.md),
[`blr_segment_dist()`](https://blorr.rsquaredacademy.com/reference/blr_segment_dist.md),
[`blr_segment_twoway()`](https://blorr.rsquaredacademy.com/reference/blr_segment_twoway.md),
[`blr_woe_iv()`](https://blorr.rsquaredacademy.com/reference/blr_woe_iv.md),
[`blr_woe_iv_stats()`](https://blorr.rsquaredacademy.com/reference/blr_woe_iv_stats.md)

## Examples

``` r
blr_segment(hsb2, honcomp, prog)
#> Event By Attributes 
#> -------------------
#> pred           1s%  
#> -------------------
#>  1             0.04 
#>  2             0.20 
#>  3             0.03 
#> -------------------
#> 
```
