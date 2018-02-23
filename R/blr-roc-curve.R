#' @importFrom ggplot2 geom_point
#' @title ROC Curve
#' @description ROC Curve
#' @param gains_table an object of class \code{blr_gains_table}
#' @param title plot title
#' @param xaxis_title x axis title
#' @param yaxis_title y axis title
#' @param roc_curve_col color of the roc curve
#' @param diag_line_col diagonal line color
#' @param point_shape shape of the points on the roc curve
#' @param point_fill fill of the points on the roc curve
#' @param point_color color of the points on the roc curve
#' @param plot_title_justify horizontal justification on the plot title
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
#'              family = binomial(link = 'logit'))
#' k <- blr_gains_table(model)
#' blr_roc_curve(k)
#' @export
#'
blr_roc_curve <- function(gains_table, title = "ROC Curve",
                          xaxis_title = "1 - Specificity",
                          yaxis_title = "Sensitivity", roc_curve_col = "blue",
                          diag_line_col = "red",
                          point_shape = 18, point_fill = "blue",
                          point_color = "blue",
                          plot_title_justify = 0.5) {

  roc_data_prep(gains_table) %>%
    ggplot(aes(x = `1 - specificity`, y = sensitivity_per)) +
    geom_point(shape = point_shape, fill = point_fill, color = point_color) +
    geom_line(color = roc_curve_col) + ggtitle(title) +
    scale_x_continuous(labels = scales::percent) + xlab(xaxis_title) +
    scale_y_continuous(labels = scales::percent) + ylab(yaxis_title) +
    theme(plot.title = element_text(hjust = plot_title_justify)) +
    geom_line(aes(x = `1 - specificity`, y = `1 - specificity`),
              color = diag_line_col)
}

roc_data_prep <- function(gains_table) {

  gains_table %>%
    use_series(gains_table) %>%
    select(sensitivity, specificity) %>%
    mutate(
      sensitivity_per   = sensitivity / 100,
      `1 - specificity` = 1 - (specificity / 100)
    ) %>%
    add_row(sensitivity_per = 0, `1 - specificity` = 0, .before = 1)

}

