# `Stepwise forward regression`

# `Stepwise forward regression`

Source:
[`R/blr-stepwise-forward-regression.R`](https://github.com/rsquaredacademy/blorr/blob/HEAD/R/blr-stepwise-forward-regression.R)

`blr_step_p_forward.Rd`

Build regression model from a set of candidate predictor variables by
entering predictors based on p values, in a stepwise manner until there
is no variable left to enter any more.

## Usage

``` r
blr_step_p_forward(model, ...)

# Default S3 method
blr_step_p_forward(model, penter = 0.3, details = FALSE, ...)

# S3 method for class 'blr_step_p_forward'
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

- penter:

  p value; variables with p value less than `penter` will enter into the
  model

- details:

  Logical; if `TRUE`, will print the regression result at each step.

- x:

  An object of class `blr_step_p_forward`.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## Value

`blr_step_p_forward` returns an object of class `"blr_step_p_forward"`.
An object of class `"blr_step_p_forward"` is a list containing the
following components:

- model:

  model with the least AIC; an object of class `glm` Only binary
  logistic regression models (binomial family with logit link) are
  supported. Weighted models (any prior weight other than 1) and grouped
  binomial models (`cbind(successes, failures)` or proportions) are not
  supported; the model must be fitted with `model = TRUE, y = TRUE`.

- steps:

  number of steps

- predictors:

  variables added to the model

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

Kutner, MH, Nachtsheim CJ, Neter J and Li W., 2004, Applied Linear
Statistical Models (5th edition). Chicago, IL., McGraw Hill/Irwin.

## Aliased terms

A model with aliased (perfectly collinear) coefficients is rejected with
an error, because the Wald test used to enter or remove terms does not
exist for them. Remove the aliased terms and refit.

## See also

Other variable selection procedures:
[`blr_step_aic_backward()`](https://blorr.rsquaredacademy.com/reference/blr_step_aic_backward.md),
[`blr_step_aic_both()`](https://blorr.rsquaredacademy.com/reference/blr_step_aic_both.md),
[`blr_step_aic_forward()`](https://blorr.rsquaredacademy.com/reference/blr_step_aic_forward.md),
[`blr_step_p_backward()`](https://blorr.rsquaredacademy.com/reference/blr_step_p_backward.md)

## Examples

``` r
# stepwise forward regression
model <- glm(honcomp ~ female + read + science, data = hsb2,
  family = binomial(link = 'logit'))
blr_step_p_forward(model)
#> Forward Selection Method    
#> ---------------------------
#> 
#> Candidate Terms: 
#> 
#> 1. female 
#> 2. read 
#> 3. science 
#> 
#> We are selecting variables based on p value...
#> 
#> Variables Entered: 
#> 
#> - read 
#> - female 
#> - science 
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
#> % Concordant          0.8561          Somers' D        0.7136   
#> % Discordant          0.1425          Gamma            0.7147   
#> % Tied                0.0014          Tau-a            0.2794   
#> Pairs                  7791           c                0.8568   
#> ---------------------------------------------------------------
#> 
#> 
#>                  Selection Summary                   
#> ----------------------------------------------------
#>         Variable                                        
#> Step    Entered       AIC         BIC       Deviance    
#> ----------------------------------------------------
#>    1    read        183.0630    189.6596    179.0630    
#>    2    female      176.8874    186.7824    170.8874    
#>    3    science     168.2364    181.4296    160.2364    
#> ----------------------------------------------------

# stepwise forward regression plot
model <- glm(honcomp ~ female + read + science, data = hsb2,
  family = binomial(link = 'logit'))
k <- blr_step_p_forward(model)
#> Forward Selection Method    
#> ---------------------------
#> 
#> Candidate Terms: 
#> 
#> 1. female 
#> 2. read 
#> 3. science 
#> 
#> We are selecting variables based on p value...
#> 
#> Variables Entered: 
#> 
#> - read 
#> - female 
#> - science 
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
#> % Concordant          0.8561          Somers' D        0.7136   
#> % Discordant          0.1425          Gamma            0.7147   
#> % Tied                0.0014          Tau-a            0.2794   
#> Pairs                  7791           c                0.8568   
#> ---------------------------------------------------------------
#> 
plot(k)

# final model
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
