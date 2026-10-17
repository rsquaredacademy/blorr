# Stepwise backward regression

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

  An object of class `lm`; the model should include all candidate
  predictor variables.

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

  model with the least AIC; an object of class `glm`

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

## See also

Other variable selection procedures:
[`blr_step_aic_backward()`](https://blorr.rsquaredacademy.com/reference/blr_step_aic_backward.md),
[`blr_step_aic_both()`](https://blorr.rsquaredacademy.com/reference/blr_step_aic_both.md),
[`blr_step_aic_forward()`](https://blorr.rsquaredacademy.com/reference/blr_step_aic_forward.md),
[`blr_step_p_forward()`](https://blorr.rsquaredacademy.com/reference/blr_step_p_forward.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# stepwise backward regression
model <- glm(honcomp ~ female + read + science + math + prog + socst,
  data = hsb2, family = binomial(link = 'logit'))
blr_step_p_backward(model)

# stepwise backward regression plot
model <- glm(honcomp ~ female + read + science + math + prog + socst,
  data = hsb2, family = binomial(link = 'logit'))
k <- blr_step_p_backward(model)
plot(k)

# final model
k$model

} # }

```
