# Stepwise forward regression

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

  An object of class `lm`; the model should include all candidate
  predictor variables.

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

  model with the least AIC; an object of class `glm`

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

Kutner, MH, Nachtscheim CJ, Neter J and Li W., 2004, Applied Linear
Statistical Models (5th edition). Chicago, IL., McGraw Hill/Irwin.

## See also

Other variable selection procedures:
[`blr_step_aic_backward()`](https://blorr.rsquaredacademy.com/reference/blr_step_aic_backward.md),
[`blr_step_aic_both()`](https://blorr.rsquaredacademy.com/reference/blr_step_aic_both.md),
[`blr_step_aic_forward()`](https://blorr.rsquaredacademy.com/reference/blr_step_aic_forward.md),
[`blr_step_p_backward()`](https://blorr.rsquaredacademy.com/reference/blr_step_p_backward.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# stepwise forward regression
model <- glm(honcomp ~ female + read + science, data = hsb2,
  family = binomial(link = 'logit'))
blr_step_p_forward(model)

# stepwise forward regression plot
model <- glm(honcomp ~ female + read + science, data = hsb2,
  family = binomial(link = 'logit'))
k <- blr_step_p_forward(model)
plot(k)

# final model
k$model

} # }
```
