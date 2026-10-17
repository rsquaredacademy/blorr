# Leverage plot

Leverage plot.

## Usage

``` r
blr_plot_leverage(
  model,
  point_color = "blue",
  title = "Leverage Plot",
  xaxis_title = "id",
  yaxis_title = "Leverage"
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

blr_plot_leverage(model)

```
