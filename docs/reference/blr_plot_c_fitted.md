# CI Displacement C vs fitted values plot

Confidence interval displacement diagnostics C vs fitted values plot.

## Usage

``` r
blr_plot_c_fitted(
  model,
  point_color = "blue",
  title = "CI Displacement C vs Fitted Values Plot",
  xaxis_title = "Fitted Values",
  yaxis_title = "CI Displacement C"
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

blr_plot_c_fitted(model)

```
