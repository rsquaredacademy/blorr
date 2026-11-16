# Bivariate analysis

Information value and likelihood ratio chi square test for initial
variable/predictor selection. Currently avialable for categorical
predictors only.

## Usage

``` r
blr_bivariate_analysis(data, response, ...)

# Default S3 method
blr_bivariate_analysis(data, response, ...)
```

## Arguments

- data:

  A `tibble` or a `data.frame`.

- response:

  Response variable; column in `data`.

- ...:

  Predictor variables; columns in `data`.

## Value

A tibble with the following columns:

- Variable:

  Variable name

- Information Value:

  Information value

- LR Chi Square:

  Likelihood ratio statisitc

- LR DF:

  Likelihood ratio degrees of freedom

- LR p-value:

  Likelihood ratio p value

## See also

Other bivariate analysis procedures:
[`blr_segment()`](https://blorr.rsquaredacademy.com/reference/blr_segment.md),
[`blr_segment_dist()`](https://blorr.rsquaredacademy.com/reference/blr_segment_dist.md),
[`blr_segment_twoway()`](https://blorr.rsquaredacademy.com/reference/blr_segment_twoway.md),
[`blr_woe_iv()`](https://blorr.rsquaredacademy.com/reference/blr_woe_iv.md),
[`blr_woe_iv_stats()`](https://blorr.rsquaredacademy.com/reference/blr_woe_iv_stats.md)

## Examples

``` r
blr_bivariate_analysis(hsb2, honcomp, female, prog, race, schtyp)
#>                          Bivariate Analysis                           
#> ---------------------------------------------------------------------
#> Variable    Information Value    LR Chi Square    LR DF    LR p-value 
#> ---------------------------------------------------------------------
#>  female           0.10              3.9350          1        0.0473   
#>   prog            0.43              16.1450         2        3e-04    
#>   race            0.33              11.3694         3        0.0099   
#>  schtyp           0.00              0.0445          1        0.8330   
#> ---------------------------------------------------------------------
```
