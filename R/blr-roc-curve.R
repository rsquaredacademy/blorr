#' @importFrom ggplot2 geom_point
#' @title ROC Curve
#' @description ROC Curve
#' @param gains_table an object of class \code{blr_gains_table}
#' @param title plot title
#' @param xaxis_title x axis title
#' @param yaxis_title y axis title
#' @param roc_curve_col color of the roc curve
#' @param point_shape shape of the points on the roc curve
#' @param point_fill fill of the points on the roc curve
#' @param point_color color of the points on the roc curve
#' @param plot_title_justify horizontal justification on the plot title
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
#'              family = binomial(link = 'logit'))
#' k <- blr_gains_table(model, hsb2)
#' blr_roc_curve(k)
#' @export
#'
blr_roc_curve <- function(gains_table, title = 'ROC Curve',
                          xaxis_title = '1 - Specificity',
                          yaxis_title = 'Sensitivity', roc_curve_col = 'blue',
                          point_shape = 18, point_fill = 'blue',
                          point_color = 'blue',
                          plot_title_justify = 0.5) {

  gains_table %>%
    use_series(gains_table) %>%
    select(sensitivity, specificity) %>%
    mutate(
      sensitivity_per = sensitivity / 100,
      `1 - specificity` = 1 - (specificity / 100)
    ) %>%
    ggplot() +
    geom_line(aes(x = `1 - specificity`, y = sensitivity_per), color = roc_curve_col) +
    geom_point(aes(x = `1 - specificity`, y = sensitivity_per), shape = point_shape,
               fill = point_fill, color = point_color) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    theme(
      plot.title = element_text(hjust = plot_title_justify)
    )

}
