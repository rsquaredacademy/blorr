# `WoE & IV`

# `WoE & IV`

Source:
[`R/blr-woe-iv.R`](https://github.com/rsquaredacademy/blorr/blob/HEAD/R/blr-woe-iv.R)

`blr_woe_iv.Rd`

Weight of evidence and information value.

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

  Response variable; column in `data` with one or two observed,
  non-missing levels. The event is the second level of a factor or
  character response, `TRUE` for a logical response and `1` for a
  numeric response; a single level labelled `"1"` or `TRUE` is the
  event. Numeric responses must be coded 0/1. Unused factor levels are
  ignored.

- digits:

  Number of decimal digits used for the returned WoE/IV table and
  printed values; the total information value is rounded only at print
  time.

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

## Details

Character and numeric predictors are treated as categorical and factor
levels without observations are dropped before the table is built. Rows
with a missing response or predictor (including an explicit missing
factor level) are excluded. If no complete observations remain, WoE/IV
is an error rather than an empty table with a zero total. If a level has
no events or no non-events, 0.5 is added to both counts for that level
(Haldane-Anscombe continuity correction) before the weight of evidence
and information value are computed.

The response may have one or two observed, non-missing levels. A
response with no observed levels is an error. With a single observed
level, the Haldane-Anscombe correction adds 0.5 to both zero cells, so
the corrected within-class distributions are equal and WoE and IV are 0.
A level labelled `"1"` or `TRUE` is the event, so numeric and factor
one-level encodings agree. The event is otherwise the second level of a
factor or character response, `TRUE` for a logical response and `1` for
a numeric response. Numeric responses must be coded 0/1; unused factor
levels are ignored. The `digits` argument controls the returned weight
of evidence and information value components and the printed values. The
total information value is computed from the unrounded components and
rounded only once for printing.

## References

Siddiqi N (2006): Credit Risk Scorecards: developing and implementing
intelligent credit scoring. New Jersey, Wiley.

Haldane, J. B. S. (1956). The estimation and significance of the
logarithm of a ratio of frequencies. *Annals of Human Genetics*, 20(4),
309–311.

## See also

Other bivariate analysis procedures:
[`blr_bivariate_analysis()`](https://blorr.rsquaredacademy.com/dev/reference/blr_bivariate_analysis.md),
[`blr_segment()`](https://blorr.rsquaredacademy.com/dev/reference/blr_segment.md),
[`blr_segment_dist()`](https://blorr.rsquaredacademy.com/dev/reference/blr_segment_dist.md),
[`blr_segment_twoway()`](https://blorr.rsquaredacademy.com/dev/reference/blr_segment_twoway.md),
[`blr_woe_iv_stats()`](https://blorr.rsquaredacademy.com/dev/reference/blr_woe_iv_stats.md)

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
#>  female          0.1022       
#> -----------------------------

# plot woe
plot(k)

```
