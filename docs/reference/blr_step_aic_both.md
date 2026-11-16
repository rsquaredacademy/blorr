# Stepwise AIC selection

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

  An object of class `lm`.

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

  model with the least AIC; an object of class `glm`

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
if (FALSE) { # \dontrun{
model <- glm(y ~ ., data = stepwise)

# selection summary
blr_step_aic_both(model)

# print details at each step
blr_step_aic_both(model, details = TRUE)

# plot
plot(blr_step_aic_both(model))

# final model
k <- blr_step_aic_both(model)
k$model

} # }
```
