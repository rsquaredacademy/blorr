# Multi variable WOE & IV

Prints weight of evidence and information value for multiple variables.
Currently avialable for categorical predictors only.

## Usage

``` r
blr_woe_iv_stats(data, response, ...)
```

## Arguments

- data:

  A `data.frame` or `tibble`.

- response:

  Response variable; column in `data`.

- ...:

  Predictor variables; column in `data`.

## See also

Other bivariate analysis procedures:
[`blr_bivariate_analysis()`](https://blorr.rsquaredacademy.com/reference/blr_bivariate_analysis.md),
[`blr_segment()`](https://blorr.rsquaredacademy.com/reference/blr_segment.md),
[`blr_segment_dist()`](https://blorr.rsquaredacademy.com/reference/blr_segment_dist.md),
[`blr_segment_twoway()`](https://blorr.rsquaredacademy.com/reference/blr_segment_twoway.md),
[`blr_woe_iv()`](https://blorr.rsquaredacademy.com/reference/blr_woe_iv.md)

## Examples

``` r
blr_woe_iv_stats(hsb2, honcomp, prog, race, female, schtyp)
#> Variable: prog
#> 
#>                            Weight of Evidence                             
#> -------------------------------------------------------------------------
#> levels    count_0s    count_1s    dist_0s    dist_1s        woe      iv   
#> -------------------------------------------------------------------------
#>   1          38          7           0.26       0.13       0.67     0.08  
#>   2          65          40          0.44       0.75      -0.53     0.17  
#>   3          44          6           0.30       0.11       0.97     0.18  
#> -------------------------------------------------------------------------
#> 
#>       Information Value       
#> -----------------------------
#> Variable    Information Value 
#> -----------------------------
#>   prog           0.4329       
#> -----------------------------
#> 
#> 
#> Variable: race
#> 
#>                            Weight of Evidence                             
#> -------------------------------------------------------------------------
#> levels    count_0s    count_1s    dist_0s    dist_1s        woe      iv   
#> -------------------------------------------------------------------------
#>   1          22          2           0.15       0.04       1.38     0.15  
#>   2          6           5           0.04       0.09      -0.84     0.04  
#>   3          18          2           0.12       0.04       1.18     0.10  
#>   4         101          44          0.69       0.83      -0.19     0.03  
#> -------------------------------------------------------------------------
#> 
#>       Information Value       
#> -----------------------------
#> Variable    Information Value 
#> -----------------------------
#>   race            0.326       
#> -----------------------------
#> 
#> 
#> Variable: female
#> 
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
#> 
#> 
#> Variable: schtyp
#> 
#>                            Weight of Evidence                            
#> ------------------------------------------------------------------------
#> levels    count_0s    count_1s    dist_0s    dist_1s        woe     iv   
#> ------------------------------------------------------------------------
#>   1         123          45          0.84       0.85      -0.01    0.00  
#>   2          24          8           0.16       0.15       0.08    0.00  
#> ------------------------------------------------------------------------
#> 
#>       Information Value       
#> -----------------------------
#> Variable    Information Value 
#> -----------------------------
#>  schtyp          0.0012       
#> -----------------------------
#> 
#> 
```
