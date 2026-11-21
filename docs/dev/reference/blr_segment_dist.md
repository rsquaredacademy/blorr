# `Response distribution`

# `Response distribution`

Source:
[`R/blr-bivariate-analysis.R`](https://github.com/rsquaredacademy/blorr/blob/HEAD/R/blr-bivariate-analysis.R)

`blr_segment_dist.Rd`

Distribution of response variable by segments/levels of a qualitative
variable.

## Usage

``` r
blr_segment_dist(data, response, predictor)

# S3 method for class 'blr_segment_dist'
plot(
  x,
  title = NA,
  xaxis_title = "Levels",
  yaxis_title = "Sample Distribution",
  sec_yaxis_title = "1s Distribution",
  bar_color = "blue",
  line_color = "red",
  print_plot = TRUE,
  ...
)
```

## Arguments

- data:

  A `tibble` or a `data.frame`.

- response:

  Response variable; column in `data` with one or two observed,
  non-missing levels. The event is the second level of a factor or
  character response, `TRUE` for a logical response and `1` for a
  numeric response; a single level labelled `"1"` or `TRUE` is the
  event. Numeric responses must be coded 0/1. Unused factor levels are
  ignored.

- predictor:

  Predictor variable; column in `data`.

- x:

  An object of class `blr_segment_dist`.

- title:

  Plot title.

- xaxis_title:

  X axis title.

- yaxis_title:

  Y axis title.

- sec_yaxis_title:

  Secondary y axis title.

- bar_color:

  Bar color.

- line_color:

  Line color.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

- ...:

  Other inputs.

## Value

A tibble.

## Details

A response with one observed level is allowed: the defined event rate is
returned for that class (0 when all responses are non-events, 1 when all
are events). A response with no observed levels is an error. Rows with a
missing response are excluded. A missing predictor is kept as its own
`NA` level, so its rows appear in a final `NA` segment with their own
count, sample share and event rate. The sample share (`n%`) and event
rate (`1s%`) are returned unrounded and printed to two decimal places;
the plot uses the unrounded proportions.

## See also

Other bivariate analysis procedures:
[`blr_bivariate_analysis()`](https://blorr.rsquaredacademy.com/dev/reference/blr_bivariate_analysis.md),
[`blr_segment()`](https://blorr.rsquaredacademy.com/dev/reference/blr_segment.md),
[`blr_segment_twoway()`](https://blorr.rsquaredacademy.com/dev/reference/blr_segment_twoway.md),
[`blr_woe_iv()`](https://blorr.rsquaredacademy.com/dev/reference/blr_woe_iv.md),
[`blr_woe_iv_stats()`](https://blorr.rsquaredacademy.com/dev/reference/blr_woe_iv_stats.md)

## Examples

``` r
k <- blr_segment_dist(hsb2, honcomp, prog)
k
#>         Event Segmentation          
#> -----------------------------------
#> prog     n     1s     n%       1s%  
#> -----------------------------------
#>  1      45     7     22.50    15.56 
#>  2      105    40    52.50    38.10 
#>  3      50     6     25.00    12.00 
#> -----------------------------------
#> 

# plot
plot(k)

```
