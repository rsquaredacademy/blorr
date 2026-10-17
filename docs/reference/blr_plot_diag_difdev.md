# Delta deviance plot

Diagnostics for detecting ill fitted observations.

## Usage

``` r
blr_plot_diag_difdev(
  model,
  point_color = "blue",
  title = "Delta Deviance Plot",
  xaxis_title = "id",
  yaxis_title = "Delta Deviance"
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

blr_plot_diag_difdev(model)

```
