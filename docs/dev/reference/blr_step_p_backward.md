# `Stepwise backward regression`

# `Stepwise backward regression`

Source:
[`R/blr-stepwise-backward-regression.R`](https://github.com/rsquaredacademy/blorr/blob/HEAD/R/blr-stepwise-backward-regression.R)

`blr_step_p_backward.Rd`

Build regression model from a set of candidate predictor variables by
removing predictors based on p values, in a stepwise manner until there
is no variable left to remove any more.

## Usage

``` r
blr_step_p_backward(model, ...)

# Default S3 method
blr_step_p_backward(model, prem = 0.3, details = FALSE, ...)

# S3 method for class 'blr_step_p_backward'
plot(x, model = NA, print_plot = TRUE, ...)
```

## Arguments

- model:

  An object of class `glm`; the model should include all candidate
  predictor variables. Only binary logistic regression models (binomial
  family with logit link) are supported. Weighted models (any prior
  weight other than 1) and grouped binomial models
  (`cbind(successes, failures)` or proportions) are not supported; the
  model must be fitted with `model = TRUE, y = TRUE`.

- ...:

  Other inputs.

- prem:

  p value; variables with p more than `prem` will be removed from the
  model.

- details:

  Logical; if `TRUE`, will print the regression result at each step.

- x:

  An object of class `blr_step_p_backward`.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## Value

`blr_step_p_backward` returns an object of class
`"blr_step_p_backward"`. An object of class `"blr_step_p_backward"` is a
list containing the following components:

- model:

  model with the least AIC; an object of class `glm` Only binary
  logistic regression models (binomial family with logit link) are
  supported. Weighted models (any prior weight other than 1) and grouped
  binomial models (`cbind(successes, failures)` or proportions) are not
  supported; the model must be fitted with `model = TRUE, y = TRUE`.

- steps:

  total number of steps

- removed:

  variables removed from the model

- aic:

  akaike information criteria

- bic:

  bayesian information criteria

- dev:

  deviance

- indvar:

  predictors

## References

Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th
ed. N.p.: John Wiley & Sons, 2012. Print.

## Aliased terms

A model with aliased (perfectly collinear) coefficients is rejected with
an error, because the Wald test used to enter or remove terms does not
exist for them. Remove the aliased terms and refit.

## See also

Other variable selection procedures:
[`blr_step_aic_backward()`](https://blorr.rsquaredacademy.com/dev/reference/blr_step_aic_backward.md),
[`blr_step_aic_both()`](https://blorr.rsquaredacademy.com/dev/reference/blr_step_aic_both.md),
[`blr_step_aic_forward()`](https://blorr.rsquaredacademy.com/dev/reference/blr_step_aic_forward.md),
[`blr_step_p_forward()`](https://blorr.rsquaredacademy.com/dev/reference/blr_step_p_forward.md)

## Examples

``` r
# stepwise backward regression
model <- glm(honcomp ~ female + read + science + math + prog + socst,
  data = hsb2, family = binomial(link = 'logit'))
blr_step_p_backward(model)
#> Backward Elimination Method 
#> ---------------------------
#> 
#> Candidate Terms: 
#> 
#> 1 . female 
#> 2 . read 
#> 3 . science 
#> 4 . math 
#> 5 . prog 
#> 6 . socst 
#> 
#> We are eliminating variables based on p value...
#> 
#> Variables Removed: 
#> 
#> - prog 
#> - socst 
#> 
#> No more variables satisfy the condition of p value =  0.3
#> 
#> 
#> Final Model Output 
#> ------------------
#> 
#>                              Model Overview                              
#> ------------------------------------------------------------------------
#> Data Set    Resp Var    Obs.    Df. Model    Df. Residual    Convergence 
#> ------------------------------------------------------------------------
#>   hsb2      honcomp     200         4            195            TRUE     
#> ------------------------------------------------------------------------
#> 
#>                     Response Summary                     
#> --------------------------------------------------------
#> Outcome        Frequency        Outcome        Frequency 
#> --------------------------------------------------------
#>    0              147              1              53     
#> --------------------------------------------------------
#> 
#>                   Maximum Likelihood Estimates                    
#> -----------------------------------------------------------------
#>  Parameter     DF    Estimate    Std. Error    z value    Pr(>|z|) 
#> -----------------------------------------------------------------
#> (Intercept)    1     -14.5773       2.1568    -6.7589      0.0000 
#>   female1      1      1.3622        0.4605     2.9580      0.0031 
#>    read        1      0.0631        0.0281     2.2455      0.0247 
#>   science      1      0.0569        0.0326     1.7429      0.0814 
#>    math        1      0.1113        0.0338     3.2992      0.0010 
#> -----------------------------------------------------------------
#> 
#> Association of Predicted Probabilities and Observed Responses 
#> -------------------------------------------------------------
#> % Concordant        88.3455         Somers' D        0.7669   
#> % Discordant        11.6545         Gamma            0.7669   
#> % Tied               0.0000         Tau-a            0.3003   
#> Pairs                 7791          c                0.8835   
#> -------------------------------------------------------------
#> 
#> 
#> 
#>                 Elimination Summary                  
#> ----------------------------------------------------
#>         Variable                                        
#> Step    Removed       AIC         BIC       Deviance    
#> ----------------------------------------------------
#>    1    prog        158.9527    178.7427    146.9527    
#>    2    socst       157.2856    173.7772    147.2856    
#> ----------------------------------------------------

# stepwise backward regression plot
model <- glm(honcomp ~ female + read + science + math + prog + socst,
  data = hsb2, family = binomial(link = 'logit'))
k <- blr_step_p_backward(model)
#> Backward Elimination Method 
#> ---------------------------
#> 
#> Candidate Terms: 
#> 
#> 1 . female 
#> 2 . read 
#> 3 . science 
#> 4 . math 
#> 5 . prog 
#> 6 . socst 
#> 
#> We are eliminating variables based on p value...
#> 
#> Variables Removed: 
#> 
#> - prog 
#> - socst 
#> 
#> No more variables satisfy the condition of p value =  0.3
#> 
#> 
#> Final Model Output 
#> ------------------
#> 
#>                              Model Overview                              
#> ------------------------------------------------------------------------
#> Data Set    Resp Var    Obs.    Df. Model    Df. Residual    Convergence 
#> ------------------------------------------------------------------------
#>   hsb2      honcomp     200         4            195            TRUE     
#> ------------------------------------------------------------------------
#> 
#>                     Response Summary                     
#> --------------------------------------------------------
#> Outcome        Frequency        Outcome        Frequency 
#> --------------------------------------------------------
#>    0              147              1              53     
#> --------------------------------------------------------
#> 
#>                   Maximum Likelihood Estimates                    
#> -----------------------------------------------------------------
#>  Parameter     DF    Estimate    Std. Error    z value    Pr(>|z|) 
#> -----------------------------------------------------------------
#> (Intercept)    1     -14.5773       2.1568    -6.7589      0.0000 
#>   female1      1      1.3622        0.4605     2.9580      0.0031 
#>    read        1      0.0631        0.0281     2.2455      0.0247 
#>   science      1      0.0569        0.0326     1.7429      0.0814 
#>    math        1      0.1113        0.0338     3.2992      0.0010 
#> -----------------------------------------------------------------
#> 
#> Association of Predicted Probabilities and Observed Responses 
#> -------------------------------------------------------------
#> % Concordant        88.3455         Somers' D        0.7669   
#> % Discordant        11.6545         Gamma            0.7669   
#> % Tied               0.0000         Tau-a            0.3003   
#> Pairs                 7791          c                0.8835   
#> -------------------------------------------------------------
#> 
plot(k)

# final model
k$model
#> 
#> Call:  glm(formula = honcomp ~ female + read + science + math, family = binomial(link = "logit"), 
#>     data = hsb2)
#> 
#> Coefficients:
#> (Intercept)      female1         read      science         math  
#>   -14.57728      1.36220      0.06311      0.05689      0.11135  
#> 
#> Degrees of Freedom: 199 Total (i.e. Null);  195 Residual
#> Null Deviance:       231.3 
#> Residual Deviance: 147.3     AIC: 157.3


```
