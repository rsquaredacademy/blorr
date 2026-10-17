# Model specification error

Test for model specification error.

## Usage

``` r
blr_linktest(model)
```

## Arguments

- model:

  An object of class `glm`.

## Value

An object of class `glm`.

## References

Pregibon, D. 1979. Data analytic methods for generalized linear models.
PhD diss., University of Toronto.

Pregibon, D. 1980. Goodness of link tests for generalized linear models.

Tukey, J. W. 1949. One degree of freedom for non-additivity.

## Examples

``` r

model <- glm(honcomp ~ female + read + science, data = hsb2,
family = binomial(link = 'logit'))

blr_linktest(model)
#> 
#> Call:
#> glm(formula = resp ~ fit + fit2, family = binomial(link = "logit"), 
#>     data = newdat)
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  0.03569    0.23399   0.153    0.879    
#> fit          0.93715    0.20767   4.513  6.4e-06 ***
#> fit2        -0.03920    0.08970  -0.437    0.662    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 231.29  on 199  degrees of freedom
#> Residual deviance: 160.04  on 197  degrees of freedom
#> AIC: 166.04
#> 
#> Number of Fisher Scoring iterations: 6
#> 
```
