#' @title Influence Diagnostics Plot
#' @description Reisudal diagnostic plots for detecting influential observations
#' @param model an object of class \code{glm}
#' @return a panel of residual diagnostic plots
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_plot_diag_influence(model)
#' @export
#'
blr_plot_diag_influence <- function(model) {

  p1 <- blr_plot_pearson_residual(model)
  p2 <- blr_plot_deviance_residual(model)
  p3 <- blr_plot_diag_c(model)
  p4 <- blr_plot_diag_cbar(model)
  p5 <- blr_plot_diag_difdev(model)
  p6 <- blr_plot_diag_difchisq(model)
  p7 <- blr_plot_leverage(model)

  myplots <- list(p1, p2, p3, p4, p5, p6, p7)
  do.call(grid.arrange, c(myplots, list(ncol = 2)))

  result <- list(plots = myplots)
  invisible(result)

}
