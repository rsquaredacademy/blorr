# `Event rate`

# `Event rate`

Source:
[`R/blr-bivariate-analysis.R`](https://github.com/rsquaredacademy/blorr/blob/HEAD/R/blr-bivariate-analysis.R)

`blr_segment.Rd`

Event rate by segments/levels of a qualitative variable.

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

  Response variable; column in `data` with one or two observed,
  non-missing levels. The event is the second level of a factor or
  character response, `TRUE` for a logical response and `1` for a
  numeric response; a single level labelled `"1"` or `TRUE` is the
  event. Numeric responses must be coded 0/1. Unused factor levels are
  ignored.

- predictor:

  Predictor variable; column in `data`.

## Value

A tibble.

## Details

A response with one observed level is allowed: the defined event rate is
returned for that class (0 when all responses are non-events, 1 when all
are events). A response with no observed levels is an error. Rows with a
missing response are excluded. A missing predictor is kept as its own
`NA` level, so its rows appear in a final `NA` segment with their own
count and rate. The event rate is returned unrounded and printed to two
decimal places.

## See also

Other bivariate analysis procedures:
[`blr_bivariate_analysis()`](https://blorr.rsquaredacademy.com/dev/reference/blr_bivariate_analysis.md),
[`blr_segment_dist()`](https://blorr.rsquaredacademy.com/dev/reference/blr_segment_dist.md),
[`blr_segment_twoway()`](https://blorr.rsquaredacademy.com/dev/reference/blr_segment_twoway.md),
[`blr_woe_iv()`](https://blorr.rsquaredacademy.com/dev/reference/blr_woe_iv.md),
[`blr_woe_iv_stats()`](https://blorr.rsquaredacademy.com/dev/reference/blr_woe_iv_stats.md)

## Examples

``` r
blr_segment(hsb2, honcomp, prog)
#> Event By Attributes  
#> --------------------
#> pred            1s%  
#> --------------------
#>  1             15.56 
#>  2             38.10 
#>  3             12.00 
#> --------------------
#> 
```
