# `Stepwise AIC backward elimination`

# `Stepwise AIC backward elimination`

Source:
[`R/blr-backward-elimination.R`](https://github.com/rsquaredacademy/blorr/blob/HEAD/R/blr-backward-elimination.R)

`blr_step_aic_backward.Rd`

Build regression model from a set of candidate predictor variables by
removing predictors based on akaike information criterion, in a stepwise
manner until there is no variable left to remove any more.

## Usage

``` r
blr_step_aic_backward(model, ...)

# Default S3 method
blr_step_aic_backward(model, progress = FALSE, details = FALSE, ...)

# S3 method for class 'blr_step_aic_backward'
plot(x, text_size = 3, print_plot = TRUE, ...)
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

  Other arguments.

- progress:

  Logical; if `TRUE`, will display variable selection progress.

- details:

  Logical; if `TRUE`, will print the regression result at each step.

- x:

  An object of class `blr_step_aic_backward`.

- text_size:

  size of the text in the plot.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## Value

`blr_step_aic_backward` returns an object of class
`"blr_step_aic_backward"`. An object of class `"blr_step_aic_backward"`
is a list containing the following components:

- model:

  model with the least AIC; an object of class `glm` Only binary
  logistic regression models (binomial family with logit link) are
  supported. Weighted models (any prior weight other than 1) and grouped
  binomial models (`cbind(successes, failures)` or proportions) are not
  supported; the model must be fitted with `model = TRUE, y = TRUE`.

- candidates:

  candidate predictor variables

- steps:

  total number of steps

- predictors:

  variables removed from the model

- aics:

  akaike information criteria

- bics:

  bayesian information criteria

- devs:

  deviances

## References

Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with
S. Fourth edition. Springer.

## See also

Other variable selection procedures:
[`blr_step_aic_both()`](https://blorr.rsquaredacademy.com/dev/reference/blr_step_aic_both.md),
[`blr_step_aic_forward()`](https://blorr.rsquaredacademy.com/dev/reference/blr_step_aic_forward.md),
[`blr_step_p_backward()`](https://blorr.rsquaredacademy.com/dev/reference/blr_step_p_backward.md),
[`blr_step_p_forward()`](https://blorr.rsquaredacademy.com/dev/reference/blr_step_p_forward.md)

## Examples

``` r
model <- glm(honcomp ~ female + read + science + math + prog + socst,
data = hsb2, family = binomial(link = 'logit'))

# elimination summary
blr_step_aic_backward(model)
#> 
#> 
#>          Backward Elimination Summary         
#> --------------------------------------------
#> Variable        AIC        BIC      Deviance 
#> --------------------------------------------
#> Full Model    162.424    188.811     146.424 
#> prog          158.953    178.743     146.953 
#> socst         157.286    173.777     147.286 
#> --------------------------------------------
#> 

# print details of each step
blr_step_aic_backward(model, details = TRUE)
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
#>  Step 0: AIC = 162.4241 
#>  honcomp ~ female + read + science + math + prog + socst 
#> 
#> -------------------------------------------------
#> Variable     DF      AIC        BIC      Deviance 
#> -------------------------------------------------
#> prog         2     158.953    178.743     146.953 
#> socst        1     160.745    183.834     146.745 
#> read         1     163.394    186.482     149.394 
#> science      1     163.815    186.903     149.815 
#> female       1     169.570    192.658     155.570 
#> math         1     170.017    193.105     156.017 
#> -------------------------------------------------
#> 
#> 
#> x prog 
#> 
#> 
#>   Step 1 : AIC = 158.9527 
#>  honcomp ~ female + read + science + math + socst 
#> 
#> -------------------------------------------------
#> Variable     DF      AIC        BIC      Deviance 
#> -------------------------------------------------
#> socst        1     157.286    173.777     147.286 
#> science      1     159.941    176.432     149.941 
#> read         1     160.307    176.798     150.307 
#> female       1     166.114    182.606     156.114 
#> math         1     168.669    185.161     158.669 
#> -------------------------------------------------
#> 
#> x socst 
#> 
#> 
#>   Step 2 : AIC = 157.2856 
#>  honcomp ~ female + read + science + math 
#> 
#> -------------------------------------------------
#> Variable     DF      AIC        BIC      Deviance 
#> -------------------------------------------------
#> science      1     158.420    171.613     150.420 
#> read         1     160.658    173.851     152.658 
#> female       1     164.988    178.181     156.988 
#> math         1     168.236    181.430     160.236 
#> -------------------------------------------------
#> 
#> No more variables to be removed.
#> 
#> Variables Removed: 
#> 
#> - prog 
#> - socst 
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
#>          Backward Elimination Summary         
#> --------------------------------------------
#> Variable        AIC        BIC      Deviance 
#> --------------------------------------------
#> Full Model    162.424    188.811     146.424 
#> prog          158.953    178.743     146.953 
#> socst         157.286    173.777     147.286 
#> --------------------------------------------
#> 

# plot
plot(blr_step_aic_backward(model))


# final model
k <- blr_step_aic_backward(model)
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
