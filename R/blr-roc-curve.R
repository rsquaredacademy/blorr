#' ROC curve
#'
#' Receiver operating characteristic curve (ROC) curve is used for assessing
#'   accuracy of the model classification. 
#'
#' @param gains_table An object of class \code{blr_gains_table}.
#' @param title Plot title.
#' @param xaxis_title X axis title.
#' @param yaxis_title Y axis title.
#' @param roc_curve_col Color of the roc curve.
#' @param diag_line_col Diagonal line color.
#' @param point_shape Shape of the points on the roc curve.
#' @param point_fill Fill of the points on the roc curve.
#' @param point_color Color of the points on the roc curve.
#' @param plot_title_justify Horizontal justification on the plot title.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @references
#' Agresti, A. (2007), An Introduction to Categorical Data Analysis, Second Edition, New York: John Wiley & Sons.
#'
#' Hosmer, D. W., Jr. and Lemeshow, S. (2000), Applied Logistic Regression, 2nd Edition, New York: John Wiley & Sons.
#'
#' Siddiqi  N  (2006):  Credit  Risk  Scorecards:  developing  and  implementing  intelligent
#' credit  scoring. New Jersey, Wiley.
#'
#' Thomas  LC,  Edelman  DB,  Crook  JN  (2002):  Credit  Scoring  and  Its  Applications.  Philadelphia,
#' SIAM Monographs on Mathematical Modeling and Computation.
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'              family = binomial(link = 'logit'))
#' k <- blr_gains_table(model)
#' blr_roc_curve(k)
#'
#' @importFrom ggplot2 geom_point
#'
#' @family model validation techniques
#'
#' @export
#'
blr_roc_curve <- function(gains_table, title = "ROC Curve",
                          xaxis_title = "1 - Specificity",
                          yaxis_title = "Sensitivity", roc_curve_col = "blue",
                          diag_line_col = "red",
                          point_shape = 18, point_fill = "blue",
                          point_color = "blue",
                          plot_title_justify = 0.5, print_plot = TRUE) {

  blr_check_gtable(gains_table)
  plot_data <- blr_prep_roc_data(gains_table)
  
  p <- 
    ggplot(plot_data, aes(x = `1 - specificity`, y = sensitivity_per)) +
    geom_point(shape = point_shape, fill = point_fill, color = point_color) +
    geom_line(color = roc_curve_col) + ggtitle(title) +
    scale_x_continuous(labels = c('0%', '25%', '50%', '75%', '100%')) + 
    scale_y_continuous(labels = c('0%', '25%', '50%', '75%', '100%')) + 
    xlab(xaxis_title) + ylab(yaxis_title) +
    theme(plot.title = element_text(hjust = plot_title_justify)) +
    geom_line(aes(x = `1 - specificity`, y = `1 - specificity`),
              color = diag_line_col)

  if (print_plot) {
    print(p)
  }

  invisible(p)
}

