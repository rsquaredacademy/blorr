# Stepwise AIC forward selection

Build regression model from a set of candidate predictor variables by
entering predictors based on chi square statistic, in a stepwise manner
until there is no variable left to enter any more.

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

  An object of class `glm`.

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

  model with the least AIC; an object of class `glm`

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
[`blr_step_aic_backward()`](https://blorr.rsquaredacademy.com/reference/blr_step_aic_backward.md),
[`blr_step_aic_both()`](https://blorr.rsquaredacademy.com/reference/blr_step_aic_both.md),
[`blr_step_p_backward()`](https://blorr.rsquaredacademy.com/reference/blr_step_p_backward.md),
[`blr_step_p_forward()`](https://blorr.rsquaredacademy.com/reference/blr_step_p_forward.md)

## Examples

``` r
if (FALSE) { # \dontrun{
model <- glm(honcomp ~ female + read + science, data = hsb2,
family = binomial(link = 'logit'))

# selection summary
blr_step_aic_forward(model)

# print details of each step
blr_step_aic_forward(model, details = TRUE)

# plot
plot(blr_step_aic_forward(model))

# final model
k <- blr_step_aic_forward(model)
k$model

} # }
```
