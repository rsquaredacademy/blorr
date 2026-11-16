# Stepwise regression

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

  An object of class `lm`; the model should include all candidate
  predictor variables.

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

  final model; an object of class `glm`

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

## References

Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th
ed. N.p.: John Wiley & Sons, 2012. Print.

## Examples

``` r
if (FALSE) { # \dontrun{
# stepwise regression
model <- glm(y ~ ., data = stepwise)
blr_step_p_both(model)

# stepwise regression plot
model <- glm(y ~ ., data = stepwise)
k <- blr_step_p_both(model)
plot(k)

# final model
k$model

} # }
```
