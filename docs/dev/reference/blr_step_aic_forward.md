# `Stepwise AIC forward selection`

# `Stepwise AIC forward selection`

Source:
[`R/blr-forward-selection.R`](https://github.com/rsquaredacademy/blorr/blob/HEAD/R/blr-forward-selection.R)

`blr_step_aic_forward.Rd`

Build regression model from a set of candidate predictor variables by
entering predictors based on Akaike information criterion, in a stepwise
manner until there is no variable left to enter any more.

## Usage

``` r
blr_step_aic_forward(model, ...)

# Default S3 method
blr_step_aic_forward(model, progress = FALSE, details = FALSE, ...)

# S3 method for class 'blr_step_aic_forward'
plot(x, text_size = 3, print_plot = TRUE, ...)
```

## Arguments

- model:

  An object of class `glm`. Only binary logistic regression models
  (binomial family with logit link) are supported. Weighted models (any
  prior weight other than 1) and grouped binomial models
  (`cbind(successes, failures)` or proportions) are not supported; the
  model must be fitted with `model = TRUE, y = TRUE`.

- ...:

  Other arguments.

- progress:

  Logical; if `TRUE`, will display variable selection progress.

- details:

  Logical; if `TRUE`, will print the regression result at each step.

- x:

  An object of class `blr_step_aic_forward`.

- text_size:

  size of the text in the plot.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## Value

`blr_step_aic_forward` returns an object of class
`"blr_step_aic_forward"`. An object of class `"blr_step_aic_forward"` is
a list containing the following components:

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

  variables entered into the model

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
[`blr_step_aic_backward()`](https://blorr.rsquaredacademy.com/dev/reference/blr_step_aic_backward.md),
[`blr_step_aic_both()`](https://blorr.rsquaredacademy.com/dev/reference/blr_step_aic_both.md),
[`blr_step_p_backward()`](https://blorr.rsquaredacademy.com/dev/reference/blr_step_p_backward.md),
[`blr_step_p_forward()`](https://blorr.rsquaredacademy.com/dev/reference/blr_step_p_forward.md)

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
family = binomial(link = 'logit'))

# selection summary
blr_step_aic_forward(model)
#> 
#>                  Selection Summary                  
#> ---------------------------------------------------
#> Step    Variable       AIC        BIC      Deviance 
#> ---------------------------------------------------
#> 1       read         183.063    189.660     179.063 
#> 2       female       176.887    186.782     170.887 
#> 3       science      168.236    181.430     160.236 
#> ---------------------------------------------------

# print details of each step
blr_step_aic_forward(model, details = TRUE)
#> Forward Selection Method 
#> ------------------------
#> 
#> Candidate Terms: 
#> 
#> 1 . female 
#> 2 . read 
#> 3 . science 
#> 
#>  Step 0: AIC = 233.2888 
#>  honcomp ~ 1 
#> 
#> -------------------------------------------------
#> Variable     DF      AIC        BIC      Deviance 
#> -------------------------------------------------
#> read          1    183.063    189.660     179.063 
#> science       1    195.777    202.373     191.777 
#> female        1    231.354    237.950     227.354 
#> -------------------------------------------------
#> 
#> 
#> + read 
#> 
#> 
#>  Step 1 : AIC = 183.063 
#>  honcomp ~ read 
#> 
#> -------------------------------------------------
#> Variable     DF      AIC        BIC      Deviance 
#> -------------------------------------------------
#> female        1    176.887    186.782     170.887 
#> science       1    178.710    188.605     172.710 
#> -------------------------------------------------
#> 
#> + female 
#> 
#> 
#>  Step 2 : AIC = 176.8874 
#>  honcomp ~ read + female 
#> 
#> -------------------------------------------------
#> Variable     DF      AIC        BIC      Deviance 
#> -------------------------------------------------
#> science       1    168.236    181.430     160.236 
#> -------------------------------------------------
#> 
#> + science 
#> 
#> 
#> Variables Entered: 
#> 
#> + read 
#> + female 
#> + science 
#> 
#> 
#> Final Model Output 
#> ------------------
#> 
#>                              Model Overview                              
#> ------------------------------------------------------------------------
#> Data Set    Resp Var    Obs.    Df. Model    Df. Residual    Convergence 
#> ------------------------------------------------------------------------
#>   hsb2      honcomp     200         3            196            TRUE     
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
#> (Intercept)    1     -12.7772       1.9755    -6.4677      0.0000 
#>    read        1      0.1035        0.0258     4.0186       1e-04 
#>   female1      1      1.4825        0.4474     3.3139       9e-04 
#>   science      1      0.0948        0.0305     3.1129      0.0019 
#> -----------------------------------------------------------------
#> 
#>  Association of Predicted Probabilities and Observed Responses  
#> ---------------------------------------------------------------
#> % Concordant         85.6116          Somers' D        0.7136   
#> % Discordant         14.2472          Gamma            0.7147   
#> % Tied                0.1412          Tau-a            0.2794   
#> Pairs                  7791           c                0.8568   
#> ---------------------------------------------------------------
#> 
#> 
#>                  Selection Summary                  
#> ---------------------------------------------------
#> Step    Variable       AIC        BIC      Deviance 
#> ---------------------------------------------------
#> 1       read         183.063    189.660     179.063 
#> 2       female       176.887    186.782     170.887 
#> 3       science      168.236    181.430     160.236 
#> ---------------------------------------------------

# plot
plot(blr_step_aic_forward(model))


# final model
k <- blr_step_aic_forward(model)
k$model
#> 
#> Call:  glm(formula = honcomp ~ read + female + science, family = binomial(link = "logit"), 
#>     data = hsb2)
#> 
#> Coefficients:
#> (Intercept)         read      female1      science  
#>   -12.77720      0.10354      1.48250      0.09479  
#> 
#> Degrees of Freedom: 199 Total (i.e. Null);  196 Residual
#> Null Deviance:       231.3 
#> Residual Deviance: 160.2     AIC: 168.2

```
