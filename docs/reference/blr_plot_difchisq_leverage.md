# Delta chi square vs leverage plot

Delta chi square vs leverage plot.

## Usage

``` r
blr_plot_difchisq_leverage(
  model,
  point_color = "blue",
  title = "Delta Chi Square vs Leverage Plot",
  xaxis_title = "Leverage",
  yaxis_title = "Delta Chi Square"
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

blr_plot_difchisq_leverage(model)

```
