% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blr-plots.R
\name{blr_plot_leverage_fitted}
\alias{blr_plot_leverage_fitted}
\title{Leverage vs fitted values plot}
\usage{
blr_plot_leverage_fitted(
  model,
  point_color = "blue",
  title = "Leverage vs Fitted Values",
  xaxis_title = "Fitted Values",
  yaxis_title = "Leverage"
)
}
\arguments{
\item{model}{An object of class \code{glm}.}

\item{point_color}{Color of the points.}

\item{title}{Title of the plot.}

\item{xaxis_title}{X axis label.}

\item{yaxis_title}{Y axis label.}
}
\description{
Leverage vs fitted values plot
}
\examples{
model <- glm(honcomp ~ female + read + science, data = hsb2,
family = binomial(link = 'logit'))

blr_plot_leverage_fitted(model)

}
