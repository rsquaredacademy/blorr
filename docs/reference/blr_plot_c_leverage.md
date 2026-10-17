# CI Displacement C vs leverage plot

Confidence interval displacement diagnostics C vs leverage plot.

## Usage

``` r
blr_plot_c_leverage(
  model,
  point_color = "blue",
  title = "CI Displacement C vs Leverage Plot",
  xaxis_title = "Leverage",
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

blr_plot_c_leverage(model)

```
