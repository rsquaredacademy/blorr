# `Stepwise regression`

# `Stepwise regression`

Source:
[`R/blr-stepwise-regression.R`](https://github.com/rsquaredacademy/blorr/blob/HEAD/R/blr-stepwise-regression.R)

`blr_step_p_both.Rd`

Build regression model from a set of candidate predictor variables by
entering and removing predictors based on p values, in a stepwise manner
until there is no variable left to enter or remove any more.

## Usage

``` r
blr_step_p_both(model, ...)

# Default S3 method
blr_step_p_both(model, pent = 0.1, prem = 0.3, details = FALSE, ...)

# S3 method for class 'blr_step_p_both'
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

  Other arguments.

- pent:

  p value; variables with p value less than `pent` will enter into the
  model.

- prem:

  p value; variables with p more than `prem` will be removed from the
  model.

- details:

  Logical; if `TRUE`, will print the regression result at each step.

- x:

  An object of class `blr_step_p_both`.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## Value

`blr_step_p_both` returns an object of class `"blr_step_p_both"`. An
object of class `"blr_step_p_both"` is a list containing the following
components:

- model:

  final model; an object of class `glm` Only binary logistic regression
  models (binomial family with logit link) are supported. Weighted
  models (any prior weight other than 1) and grouped binomial models
  (`cbind(successes, failures)` or proportions) are not supported; the
  model must be fitted with `model = TRUE, y = TRUE`.

- orders:

  candidate predictor variables according to the order by which they
  were added or removed from the model

- method:

  addition/deletion

- steps:

  total number of steps

- predictors:

  variables retained in the model (after addition)

- aic:

  akaike information criteria

- bic:

  bayesian information criteria

- dev:

  deviance

- indvar:

  predictors

## Aliased terms

A model with aliased (perfectly collinear) coefficients is rejected with
an error, because the Wald test used to enter or remove terms does not
exist for them. Remove the aliased terms and refit.

## References

Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th
ed. N.p.: John Wiley & Sons, 2012. Print.

## Examples

``` r
# \donttest{
# stepwise regression
model <- glm(y ~ ., data = stepwise, family = binomial(link = 'logit'))
blr_step_p_both(model)
#> Stepwise Selection Method   
#> ---------------------------
#> 
#> Candidate Terms: 
#> 
#> 1. x1 
#> 2. x2 
#> 3. x3 
#> 4. x4 
#> 5. x5 
#> 6. x6 
#> 
#> We are selecting variables based on p value...
#> 
#> Variables Entered/Removed: 
#> 
#> - x6 added 
#> - x1 added 
#> - x3 added 
#> - x2 added 
#> - x6 added 
#> - x5 added 
#> 
#> No more variables to be added/removed.
#> 
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
#>                       Stepwise Selection Summary                       
#> ----------------------------------------------------------------------
#>                      Added/                                                  
#> Step    Variable    Removed        AIC           BIC         Deviance     
#> ----------------------------------------------------------------------
#>    1       x6       addition     18869.627     18885.434    18865.6270    
#>    2       x1       addition     18571.376     18595.087    18565.3760    
#>    3       x3       addition     18016.724     18048.338    18008.7240    
#>    4       x2       addition     16642.374     16681.891    16632.3740    
#>    5       x6       removal      16640.653     16672.267    16632.6530    
#>    6       x5       addition     16639.219     16678.736    16629.2190    
#> ----------------------------------------------------------------------

# stepwise regression plot
model <- glm(y ~ ., data = stepwise, family = binomial(link = 'logit'))
k <- blr_step_p_both(model)
#> Stepwise Selection Method   
#> ---------------------------
#> 
#> Candidate Terms: 
#> 
#> 1. x1 
#> 2. x2 
#> 3. x3 
#> 4. x4 
#> 5. x5 
#> 6. x6 
#> 
#> We are selecting variables based on p value...
#> 
#> Variables Entered/Removed: 
#> 
#> - x6 added 
#> - x1 added 
#> - x3 added 
#> - x2 added 
#> - x6 added 
#> - x5 added 
#> 
#> No more variables to be added/removed.
#> 
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
plot(k)

# final model
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
