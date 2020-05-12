#' Influence diagnostics plot
#'
#' Reisudal diagnostic plots for detecting influential observations.
#'
#' @param model An object of class \code{glm}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @return A panel of influence diagnostic plots.
#'
#' @references
#'
#' Fox, John (1991), Regression Diagnostics. Newbury Park, CA: Sage Publications.
#'
#' Cook, R. D. and Weisberg, S. (1982), Residuals and Influence in Regression, New York: Chapman & Hall.
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_plot_diag_influence(model)
#'
#' @family diagnostic plots
#'
#' @export
#'
blr_plot_diag_influence <- function(model, print_plot = TRUE) {

  blr_check_model(model)

  p1 <- blr_plot_pearson_residual(model)
  p2 <- blr_plot_deviance_residual(model)
  p3 <- blr_plot_diag_c(model)
  p4 <- blr_plot_diag_cbar(model)
  p5 <- blr_plot_diag_difdev(model)
  p6 <- blr_plot_diag_difchisq(model)
  p7 <- blr_plot_leverage(model)

  myplots <- list(pearson_residual = p1,
                  deviance_residual = p2,
                  diag_c = p3,
                  diag_cbar = p4,
                  diag_difdev = p5,
                  diag_difchisq = p6,
                  leverage = p7)

  if (print_plot) {
    check_suggests('gridExtra')
    do.call(grid.arrange, c(myplots, list(ncol = 2)))
  }

  invisible(myplots)
}

#' Fitted values diagnostics plot
#'
#' Diagnostic plots for fitted values.
#'
#' @param model An object of class \code{glm}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @return A panel of diagnostic plots for fitted values.
#'
#' @references
#'
#' Fox, John (1991), Regression Diagnostics. Newbury Park, CA: Sage Publications.
#'
#' Cook, R. D. and Weisberg, S. (1982), Residuals and Influence in Regression, New York: Chapman & Hall.
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_plot_diag_fit(model)
#'
#' @family diagnostic plots
#'
#' @export
#'
blr_plot_diag_fit <- function(model, print_plot = TRUE) {

  blr_check_model(model)

  p1 <- blr_plot_difdev_fitted(model)
  p2 <- blr_plot_difchisq_fitted(model)
  p3 <- blr_plot_leverage_fitted(model)
  p4 <- blr_plot_c_fitted(model)

  myplots <- list(difdev_fitted = p1,
                  difchisq_fitted = p2,
                  leverage_fitted = p3,
                  c_fitted = p4)

  if (print_plot) {
    check_suggests('gridExtra')
    do.call(grid.arrange, c(myplots, list(ncol = 2)))
  }

  invisible(myplots)
}


#' Leverage diagnostics plot
#'
#' Diagnostic plots for leverage.
#'
#' @param model An object of class \code{glm}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @return A panel of diagnostic plots for leverage.
#'
#' @references
#'
#' Fox, John (1991), Regression Diagnostics. Newbury Park, CA: Sage Publications.
#'
#' Cook, R. D. and Weisberg, S. (1982), Residuals and Influence in Regression, New York: Chapman & Hall.
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_plot_diag_leverage(model)
#'
#' @family diagnostic plots
#'
#' @export
#'
blr_plot_diag_leverage <- function(model, print_plot = TRUE) {

  blr_check_model(model)

  p1 <- blr_plot_difdev_leverage(model)
  p2 <- blr_plot_difchisq_leverage(model)
  p3 <- blr_plot_c_leverage(model)
  p4 <- blr_plot_fitted_leverage(model)

  myplots <- list(difdev_leverage = p1,
                  difchisq_leverage = p2,
                  c_leverage = p3,
                  fitted_leverage = p4)

  if (print_plot) {
    check_suggests('gridExtra')
    do.call(grid.arrange, c(myplots, list(ncol = 2)))
  }

  invisible(myplots)
}
