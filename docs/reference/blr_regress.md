# `Binary logistic regression`

# `Binary logistic regression`

Source:
[`R/blr-regress.R`](https://github.com/rsquaredacademy/blorr/blob/HEAD/R/blr-regress.R)

`blr_regress.Rd`

Binary logistic regression.

## Usage

``` r
blr_regress(object, ...)

# Default S3 method
blr_regress(
  object,
  data,
  odd_conf_limit = FALSE,
  weights = NULL,
  offset = NULL,
  ...
)

# S3 method for class 'glm'
blr_regress(object, odd_conf_limit = FALSE, ...)
```

## Arguments

- object:

  An object of class "formula" (or one that can be coerced to that
  class): a symbolic description of the model to be fitted or class
  `glm` with binomial family and logit link. Weighted models (any prior
  weight other than 1) and grouped binomial models
  (`cbind(successes, failures)` or proportions) are not supported; refit
  models stored without their model frame or response using
  `glm(..., model = TRUE, y = TRUE)`.

- ...:

  Other inputs.

- data:

  A `data.frame` or `tibble` containing the variables in the model.

- odd_conf_limit:

  If TRUE, odds ratio confidence limits will be displayed.

- weights:

  An optional vector of prior weights to be used in the fitting process.

- offset:

  An optional offset to be included in the model.

## Details

The default method fits a binomial logit model with
[`glm`](https://rdrr.io/r/stats/glm.html). An `offset` may be supplied
directly, and the formula environment is kept. When a fitted `glm`
object is supplied, the fit is summarised directly, so an offset
supplied through the model formula and the formula environment are
respected. Weighted models are rejected by the shared model guard.

Odds ratio confidence limits are profile-likelihood intervals computed
by [`confint`](https://rdrr.io/r/stats/confint.html).

## Examples

``` r
# using formula
blr_regress(object = honcomp ~ female + read + science, data = hsb2)
#>                              Model Overview                              
#> ------------------------------------------------------------------------
#> Data Set    Resp Var    Obs.    Df. Model    Df. Residual    Convergence 
#> ------------------------------------------------------------------------
#>   data      honcomp     200         3            196            TRUE     
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
#>   female1      1      1.4825        0.4474     3.3139       9e-04 
#>    read        1      0.1035        0.0258     4.0186       1e-04 
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

# using a model built with glm
model <- glm(honcomp ~ female + read + science, data = hsb2,
             family = binomial(link = 'logit'))

blr_regress(model)
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
#>   female1      1      1.4825        0.4474     3.3139       9e-04 
#>    read        1      0.1035        0.0258     4.0186       1e-04 
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

# odds ratio estimates
blr_regress(model, odd_conf_limit = TRUE)
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
#>   female1      1      1.4825        0.4474     3.3139       9e-04 
#>    read        1      0.1035        0.0258     4.0186       1e-04 
#>   science      1      0.0948        0.0305     3.1129      0.0019 
#> -----------------------------------------------------------------
#> 
#>                     Odds Ratio Estimates                     
#> ------------------------------------------------------------
#>  Effects          Estimate          95% Profile Conf. Limit 
#> ------------------------------------------------------------
#>  female1           4.4039           1.8955           11.0521 
#>    read            1.1091           1.0569            1.1699 
#>  science           1.0994           1.0377            1.1702 
#> ------------------------------------------------------------
#> 
#>  Association of Predicted Probabilities and Observed Responses  
#> ---------------------------------------------------------------
#> % Concordant          0.8561          Somers' D        0.7136   
#> % Discordant          0.1425          Gamma            0.7147   
#> % Tied                0.0014          Tau-a            0.2794   
#> Pairs                  7791           c                0.8568   
#> ---------------------------------------------------------------
#> 
```
