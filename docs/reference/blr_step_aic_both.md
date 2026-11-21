# `Stepwise AIC selection`

# `Stepwise AIC selection`

Source:
[`R/blr-stepwise-selection.R`](https://github.com/rsquaredacademy/blorr/blob/HEAD/R/blr-stepwise-selection.R)

`blr_step_aic_both.Rd`

Build regression model from a set of candidate predictor variables by
entering and removing predictors based on akaike information criterion,
in a stepwise manner until there is no variable left to enter or remove
any more.

## Usage

``` r
blr_step_aic_both(model, details = FALSE, ...)

# S3 method for class 'blr_step_aic_both'
plot(x, text_size = 3, ...)
```

## Arguments

- model:

  An object of class `glm`. Only binary logistic regression models
  (binomial family with logit link) are supported. Weighted models (any
  prior weight other than 1) and grouped binomial models
  (`cbind(successes, failures)` or proportions) are not supported; the
  model must be fitted with `model = TRUE, y = TRUE`.

- details:

  Logical; if `TRUE`, details of variable selection will be printed on
  screen.

- ...:

  Other arguments.

- x:

  An object of class `blr_step_aic_both`.

- text_size:

  size of the text in the plot.

## Value

`blr_step_aic_both` returns an object of class `"blr_step_aic_both"`. An
object of class `"blr_step_aic_both"` is a list containing the following
components:

- model:

  model with the least AIC; an object of class `glm` Only binary
  logistic regression models (binomial family with logit link) are
  supported. Weighted models (any prior weight other than 1) and grouped
  binomial models (`cbind(successes, failures)` or proportions) are not
  supported; the model must be fitted with `model = TRUE, y = TRUE`.

- candidates:

  candidate predictor variables

- predictors:

  variables added/removed from the model

- method:

  addition/deletion

- aics:

  akaike information criteria

- bics:

  bayesian information criteria

- devs:

  deviances

- steps:

  total number of steps

## References

Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with
S. Fourth edition. Springer.

## See also

Other variable selection procedures:
[`blr_step_aic_backward()`](https://blorr.rsquaredacademy.com/reference/blr_step_aic_backward.md),
[`blr_step_aic_forward()`](https://blorr.rsquaredacademy.com/reference/blr_step_aic_forward.md),
[`blr_step_p_backward()`](https://blorr.rsquaredacademy.com/reference/blr_step_p_backward.md),
[`blr_step_p_forward()`](https://blorr.rsquaredacademy.com/reference/blr_step_p_forward.md)

## Examples

``` r
# \donttest{
model <- glm(y ~ ., data = stepwise, family = binomial(link = 'logit'))

# selection summary
blr_step_aic_both(model)
#> Stepwise Selection Method 
#> -------------------------
#> 
#> Candidate Terms: 
#> 
#> 1 . x1 
#> 2 . x2 
#> 3 . x3 
#> 4 . x4 
#> 5 . x5 
#> 6 . x6 
#> 
#> 
#> Variables Entered/Removed: 
#> 
#> - x6 added 
#> - x1 added 
#> - x3 added 
#> - x2 added 
#> - x6 removed 
#> - x5 added 
#> 
#> No more variables to be added or removed.
#> 
#> 
#>                       Stepwise Summary                       
#> -----------------------------------------------------------
#> Variable     Method        AIC          BIC       Deviance  
#> -----------------------------------------------------------
#> x6          addition    18869.627    18885.434    18865.627 
#> x1          addition    18571.376    18595.087    18565.376 
#> x3          addition    18016.724    18048.338    18008.724 
#> x2          addition    16642.374    16681.891    16632.374 
#> x6          removal     16640.653    16672.267    16632.653 
#> x5          addition    16639.219    16678.736    16629.219 
#> -----------------------------------------------------------
#> 

# print details at each step
blr_step_aic_both(model, details = TRUE)
#> Stepwise Selection Method 
#> -------------------------
#> 
#> Candidate Terms: 
#> 
#> 1 . x1 
#> 2 . x2 
#> 3 . x3 
#> 4 . x4 
#> 5 . x5 
#> 6 . x6 
#> 
#>  Step 0: AIC = 27727.55 
#>  y ~ 1 
#> 
#> 
#>                   Enter New Variables                 
#> ------------------------------------------------------
#> Variable     DF       AIC          BIC       Deviance  
#> ------------------------------------------------------
#> x6            1    18869.627    18885.434    18865.627 
#> x3            1    23144.515    23160.322    23140.515 
#> x1            1    23205.160    23220.967    23201.160 
#> x2            1    23415.313    23431.120    23411.313 
#> x5            1    27720.114    27735.921    27716.114 
#> x4            1    27729.423    27745.230    27725.423 
#> ------------------------------------------------------
#> 
#> - x6 added 
#> 
#> 
#>  Step 1 : AIC = 18869.63 
#>  y ~ x6 
#> 
#>                   Enter New Variables                 
#> ------------------------------------------------------
#> Variable     DF       AIC          BIC       Deviance  
#> ------------------------------------------------------
#> x1            1    18571.376    18595.087    18565.376 
#> x3            1    18592.653    18616.364    18586.653 
#> x2            1    18658.726    18682.437    18652.726 
#> x5            1    18870.331    18894.042    18864.331 
#> x4            1    18870.842    18894.552    18864.842 
#> ------------------------------------------------------
#> 
#> - x1 added 
#> 
#> 
#>  Step 2 : AIC = 18571.38 
#>  y ~ x6 + x1 
#> 
#>               Remove Existing Variables               
#> ------------------------------------------------------
#> Variable     DF       AIC          BIC       Deviance  
#> ------------------------------------------------------
#> x1            1    18869.627    18885.434    18865.627 
#> x6            1    23205.160    23220.967    23201.160 
#> ------------------------------------------------------
#> 
#>                   Enter New Variables                 
#> ------------------------------------------------------
#> Variable     DF       AIC          BIC       Deviance  
#> ------------------------------------------------------
#> x3            1    18016.724    18048.338    18008.724 
#> x2            1    18116.912    18148.526    18108.912 
#> x5            1    18572.004    18603.618    18564.004 
#> x4            1    18572.573    18604.187    18564.573 
#> ------------------------------------------------------
#> 
#> - x3 added 
#> 
#> 
#>  Step 3 : AIC = 18016.72 
#>  y ~ x6 + x1 + x3 
#> 
#>               Remove Existing Variables               
#> ------------------------------------------------------
#> Variable     DF       AIC          BIC       Deviance  
#> ------------------------------------------------------
#> x3            1    18571.376    18595.087    18565.376 
#> x1            1    18592.653    18616.364    18586.653 
#> x6            1    19489.823    19513.533    19483.823 
#> ------------------------------------------------------
#> 
#>                   Enter New Variables                 
#> ------------------------------------------------------
#> Variable     DF       AIC          BIC       Deviance  
#> ------------------------------------------------------
#> x2            1    16642.374    16681.891    16632.374 
#> x5            1    18016.953    18056.470    18006.953 
#> x4            1    18017.666    18057.183    18007.666 
#> ------------------------------------------------------
#> 
#> - x2 added 
#> 
#> 
#>  Step 4 : AIC = 16642.37 
#>  y ~ x6 + x1 + x3 + x2 
#> 
#>               Remove Existing Variables               
#> ------------------------------------------------------
#> Variable     DF       AIC          BIC       Deviance  
#> ------------------------------------------------------
#> x6            1    16640.653    16672.267    16632.653 
#> x2            1    18016.724    18048.338    18008.724 
#> x3            1    18116.912    18148.526    18108.912 
#> x1            1    18152.512    18184.126    18144.512 
#> ------------------------------------------------------
#> 
#> - x6 removed 
#> 
#> 
#>  Step 5 : AIC = 16640.65 
#>  y ~ x1 + x3 + x2 
#> 
#>                   Enter New Variables                 
#> ------------------------------------------------------
#> Variable     DF       AIC          BIC       Deviance  
#> ------------------------------------------------------
#> x5            1    16639.219    16678.736    16629.219 
#> x4            1    16641.761    16681.279    16631.761 
#> ------------------------------------------------------
#> 
#> - x5 added 
#> 
#> 
#>  Step 6 : AIC = 16639.22 
#>  y ~ x1 + x3 + x2 + x5 
#> 
#>               Remove Existing Variables               
#> ------------------------------------------------------
#> Variable     DF       AIC          BIC       Deviance  
#> ------------------------------------------------------
#> x5            1    16640.653    16672.267    16632.653 
#> x2            1    19487.512    19519.126    19479.512 
#> x3            1    19693.268    19724.882    19685.268 
#> x1            1    19728.577    19760.191    19720.577 
#> ------------------------------------------------------
#> 
#>                   Enter New Variables                 
#> ------------------------------------------------------
#> Variable     DF       AIC          BIC       Deviance  
#> ------------------------------------------------------
#> x4            1    16640.358    16687.778    16628.358 
#> ------------------------------------------------------
#> 
#> 
#> No more variables to be added or removed.
#> 
#> Final Model Output 
#> ------------------
#> 
#>                              Model Overview                               
#> -------------------------------------------------------------------------
#> Data Set    Resp Var     Obs.    Df. Model    Df. Residual    Convergence 
#> -------------------------------------------------------------------------
#> stepwise       y        20000        4           19995           TRUE     
#> -------------------------------------------------------------------------
#> 
#>                     Response Summary                     
#> --------------------------------------------------------
#> Outcome        Frequency        Outcome        Frequency 
#> --------------------------------------------------------
#>    0             10041             1             9959    
#> --------------------------------------------------------
#> 
#>                    Maximum Likelihood Estimates                    
#> ------------------------------------------------------------------
#>  Parameter     DF    Estimate    Std. Error    z value     Pr(>|z|) 
#> ------------------------------------------------------------------
#> (Intercept)    1     -1.5228        0.0287    -53.1302      0.0000 
#>     x1         1      1.0276        0.0212     48.4927      0.0000 
#>     x3         1      1.0260        0.0213     48.2799      0.0000 
#>     x2         1      0.9807        0.0209     46.9690      0.0000 
#>     x5         1      0.0357        0.0192      1.8531      0.0639 
#> ------------------------------------------------------------------
#> 
#>  Association of Predicted Probabilities and Observed Responses  
#> ---------------------------------------------------------------
#> % Concordant          0.8911          Somers' D        0.7821   
#> % Discordant          0.1089          Gamma            0.7821   
#> % Tied                0.0000          Tau-a            0.3911   
#> Pairs                99998319         c                0.8911   
#> ---------------------------------------------------------------
#> 
#> 
#> 
#>                       Stepwise Summary                       
#> -----------------------------------------------------------
#> Variable     Method        AIC          BIC       Deviance  
#> -----------------------------------------------------------
#> x6          addition    18869.627    18885.434    18865.627 
#> x1          addition    18571.376    18595.087    18565.376 
#> x3          addition    18016.724    18048.338    18008.724 
#> x2          addition    16642.374    16681.891    16632.374 
#> x6          removal     16640.653    16672.267    16632.653 
#> x5          addition    16639.219    16678.736    16629.219 
#> -----------------------------------------------------------
#> 

# plot
plot(blr_step_aic_both(model))
#> Stepwise Selection Method 
#> -------------------------
#> 
#> Candidate Terms: 
#> 
#> 1 . x1 
#> 2 . x2 
#> 3 . x3 
#> 4 . x4 
#> 5 . x5 
#> 6 . x6 
#> 
#> 
#> Variables Entered/Removed: 
#> 
#> - x6 added 
#> - x1 added 
#> - x3 added 
#> - x2 added 
#> - x6 removed 
#> - x5 added 
#> 
#> No more variables to be added or removed.


# final model
k <- blr_step_aic_both(model)
#> Stepwise Selection Method 
#> -------------------------
#> 
#> Candidate Terms: 
#> 
#> 1 . x1 
#> 2 . x2 
#> 3 . x3 
#> 4 . x4 
#> 5 . x5 
#> 6 . x6 
#> 
#> 
#> Variables Entered/Removed: 
#> 
#> - x6 added 
#> - x1 added 
#> - x3 added 
#> - x2 added 
#> - x6 removed 
#> - x5 added 
#> 
#> No more variables to be added or removed.
k$model
#> 
#> Call:  glm(formula = y ~ x1 + x3 + x2 + x5, family = binomial(link = "logit"), 
#>     data = stepwise)
#> 
#> Coefficients:
#> (Intercept)           x1           x3           x2           x5  
#>    -1.52278      1.02763      1.02602      0.98066      0.03566  
#> 
#> Degrees of Freedom: 19999 Total (i.e. Null);  19995 Residual
#> Null Deviance:       27730 
#> Residual Deviance: 16630     AIC: 16640

# }
```
