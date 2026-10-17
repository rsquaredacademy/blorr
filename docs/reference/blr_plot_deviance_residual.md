# Deviance residual values

Deviance residuals plot.

## Usage

``` r
blr_plot_deviance_residual(
  model,
  point_color = "blue",
  title = "Deviance Residuals Plot",
  xaxis_title = "id",
  yaxis_title = "Deviance Residuals"
)
```

## Arguments

- model:

  An object of class `glm`.

- point_color:

  Color of the points.

- title:

  Title of the plot.

- xaxis_title:

  X axis label.

- yaxis_title:

  Y axis label.

## Examples

``` r
model <- glm(honcomp ~ female + read + science, data = hsb2,
family = binomial(link = 'logit'))

blr_plot_deviance_residual(model)

```
