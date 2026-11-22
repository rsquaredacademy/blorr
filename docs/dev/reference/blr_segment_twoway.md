# `Two way event rate`

# `Two way event rate`

Source:
[`R/blr-bivariate-analysis.R`](https://github.com/rsquaredacademy/blorr/blob/HEAD/R/blr-bivariate-analysis.R)

`blr_segment_twoway.Rd`

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

  Response variable; column in `data` with one or two observed,
  non-missing levels. The event is the second level of a factor or
  character response, `TRUE` for a logical response and `1` for a
  numeric response; a single level labelled `"1"` or `TRUE` is the
  event. Numeric responses must be coded 0/1. Unused factor levels are
  ignored.

- variable_1:

  Column in `data`.

- variable_2:

  Column in `data`.

## Value

A tibble.

## Details

A response with one observed level is allowed: each defined within-cell
event rate is 0 when all responses are non-events or 1 when all are
events. A response with no observed levels is an error. Rows with a
missing response are excluded. A missing value in either grouping
variable is kept as its own `NA` level and appears as a final row or
column labelled `NA`.

Each cell is the event rate within that pair of levels: the number of
events in the cell divided by the number of non-missing responses in the
cell. It is not the share of all observations. A cell with no
non-missing responses is `NA` and prints as `-`. Rates are returned
unrounded and printed to three decimal places.

## See also

Other bivariate analysis procedures:
[`blr_bivariate_analysis()`](https://blorr.rsquaredacademy.com/dev/reference/blr_bivariate_analysis.md),
[`blr_segment()`](https://blorr.rsquaredacademy.com/dev/reference/blr_segment.md),
[`blr_segment_dist()`](https://blorr.rsquaredacademy.com/dev/reference/blr_segment_dist.md),
[`blr_woe_iv()`](https://blorr.rsquaredacademy.com/dev/reference/blr_woe_iv.md),
[`blr_woe_iv_stats()`](https://blorr.rsquaredacademy.com/dev/reference/blr_woe_iv_stats.md)

## Examples

``` r
blr_segment_twoway(hsb2, honcomp, prog, female)
#>                female    
#> -----------------------
#> prog      0       1     
#> -----------------------
#>   1     0.095   0.208   
#> -----------------------
#>   2     0.319   0.431   
#> -----------------------
#>   3     0.043   0.185   
#> -----------------------
```
