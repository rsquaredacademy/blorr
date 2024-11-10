#' Gains table & lift chart
#'
#' Compute sensitivity, specificity, accuracy and KS statistics to
#'   generate the lift chart and the KS chart.
#'
#' @param model An object of class \code{glm}.
#' @param data A \code{tibble} or a \code{data.frame}.
#' @param x An object of class \code{blr_gains_table}.
#' @param title Plot title.
#' @param xaxis_title X axis title.
#' @param yaxis_title Y axis title.
#' @param lift_curve_col Color of the lift curve.
#' @param diag_line_col Diagonal line color.
#' @param plot_title_justify Horizontal justification on the plot title.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param ... Other inputs.
#'
#' @return A tibble.
#'
#' @references
#' Agresti, A. (2007), An Introduction to Categorical Data Analysis, Second Edition, New York: John Wiley & Sons.
#'
#' Agresti, A. (2013), Categorical Data Analysis, Third Edition, New York: John Wiley & Sons.
#'
#' Thomas LC (2009): Consumer  Credit  Models:  Pricing,  Profit,  and  Portfolio.
#' Oxford,  Oxford  Uni-versity Press.
#'
#' Sobehart  J,  Keenan  S,  Stein  R  (2000):  Benchmarking  Quantitative  Default  Risk  Models:
#' A  Validation Methodology, Moody’s Investors Service.
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'              family = binomial(link = 'logit'))
#' # gains table
#' blr_gains_table(model)
#'
#' # lift chart
#' k <- blr_gains_table(model)
#' plot(k)
#'
#' @importFrom stats model.frame model.response predict.glm
#' @importFrom ggplot2 ggplot geom_line aes ggtitle xlab ylab
#' scale_x_continuous scale_y_continuous theme element_text
#'
#' @family model validation techniques
#'
#' @export
#'
blr_gains_table <- function(model, data = NULL) UseMethod("blr_gains_table")

#' @export
#'
blr_gains_table.default <- function(model, data = NULL) {

  blr_check_model(model)

  if (is.null(data)) {
    test_data <- FALSE
    data      <- model$model
  } else {
    test_data <- TRUE
    blr_check_data(data)
    data      <- data
  }

  dct <- gains_decile_count(data)
  gtp <- gains_table_prep(model, data, test_data)
  gtm <- gains_table_modify(gtp, decile_count = dct)
  gt  <- gains_table_mutate(gtm)

  result <- list(gains_table = gt)
  class(result) <- c("blr_gains_table")

  return(result)

}

#' @export
#'
print.blr_gains_table <- function(x, ...) {

  cols_print <- c('decile', 'total', '1', '0', 'ks', 'tp', 'tn', 'fp', 'fn',
    'sensitivity', 'specificity', 'accuracy')
  print(x$gains_table[cols_print])

}

#' @rdname blr_gains_table
#' @export
#'
plot.blr_gains_table <- function(x, title = "Lift Chart", xaxis_title = "% Population",
                                 yaxis_title = "% Cumulative 1s", diag_line_col = "red",
                                 lift_curve_col = "blue", plot_title_justify = 0.5,
                                 print_plot = TRUE, ...) {

  blr_check_gtable(x)

  p <-
    ggplot(gains_plot_data(x)) +
    geom_line(aes(x = cum_total_per, y = cum_1s_per), color = lift_curve_col) +
    geom_line(aes(x = cum_total_per, y = cum_total_y), color = diag_line_col) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title) +
    scale_x_continuous(labels = c('0%', '25%', '50%', '75%', '100%')) +
    scale_y_continuous(labels = c('0%', '25%', '50%', '75%', '100%')) +
    theme(plot.title = element_text(hjust = plot_title_justify))

  if (print_plot) {
    print(p)
  }

  invisible(p)
}


#' KS chart
#'
#' Kolmogorov-Smirnov (KS) statistics is used to assess predictive power for
#' marketing or credit risk models. It is the maximum difference between %
#' cumulative event and non-event distribution across score/probability bands.
#' The gains table typically has % cumulative event and % cumulative non-event
#' across score bands and can be used to find the KS for a model.
#'
#' @param gains_table An object of class \code{blr_gains_table}.
#' @param title Plot title.
#' @param xaxis_title X axis title.
#' @param yaxis_title Y axis title.
#' @param ks_line_color Color of the line indicating maximum KS statistic.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @references
#' \doi{doi.org/10.1198/tast.2009.08210}
#'
#' \url{https://pubmed.ncbi.nlm.nih.gov/843576/}
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'              family = binomial(link = 'logit'))
#' gt <- blr_gains_table(model)
#' blr_ks_chart(gt)
#'
#' @importFrom ggplot2 element_blank annotate geom_segment
#'
#' @family model validation techniques
#'
#' @export
#'
blr_ks_chart <- function(gains_table, title = "KS Chart", yaxis_title = " ",
                         xaxis_title = "Cumulative Population %",
                         ks_line_color = "black", print_plot = TRUE) {

  blr_check_gtable(gains_table)

  ks_line    <- blr_prep_kschart_line(gains_table)
  annotate_y <- blr_prep_ksannotate_y(ks_line)
  ks_stat    <- blr_prep_kschart_stat(ks_line)
  annotate_x <- blr_prep_ksannotate_x(ks_line)

  plot_data <- blr_prep_kschart_data(gains_table)

  p <-
    ggplot(plot_data, aes(x = cum_total_per)) +
    geom_line(aes(y = cum_1s_per, color = "Cumulative 1s %")) +
    geom_line(aes(y = cum_0s_per, color = "Cumulative 0s %")) +
    geom_point(aes(y = cum_1s_per, color = "Cumulative 1s %")) +
    geom_point(aes(y = cum_0s_per, color = "Cumulative 0s %")) +
    geom_segment(x = ks_line[[1]], xend = ks_line[[1]], y = ks_line[[3]],
      yend = ks_line[[2]], color = ks_line_color) +
    annotate("text", x = annotate_x, y = annotate_y,
             label = paste0("KS: ", ks_stat, "%")) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title) +
    scale_x_continuous(labels = c('0%', '25%', '50%', '75%', '100%')) +
    scale_y_continuous(labels = c('0%', '25%', '50%', '75%', '100%')) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_blank())

  if (print_plot) {
    print(p)
  }

  invisible(p)

}


#' Event rate by decile
#'
#' Visualize the decile wise event rate.
#'
#' @param gains_table An object of class \code{blr_gains_table}.
#' @param xaxis_title X axis title.
#' @param yaxis_title Y axis title.
#' @param title Plot title.
#' @param bar_color Bar color.
#' @param text_size Size of the bar labels.
#' @param text_vjust Vertical justification of the bar labels.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'              family = binomial(link = 'logit'))
#' gt <- blr_gains_table(model)
#' blr_decile_capture_rate(gt)
#'
#' @family model validation techniques
#'
#' @export
#'
blr_decile_capture_rate <- function(gains_table, xaxis_title = "Decile",
                                    yaxis_title = "Capture Rate",
                                    title = "Capture Rate by Decile",
                                    bar_color = "blue", text_size = 3.5,
                                    text_vjust = -0.3, print_plot = TRUE) {

  blr_check_gtable(gains_table)

  decile_rate <- blr_prep_dcrate_data(gains_table)

  p <-
    ggplot(data = decile_rate, aes(x = decile, y = decile_mean)) +
    geom_col(fill = bar_color) + ggtitle(title) + xlab(xaxis_title) +
    geom_text(aes(label = round(decile_mean, 2)), vjust = text_vjust,
      size = text_size) + ylab(yaxis_title)

  if (print_plot) {
    print(p)
  }

  result <- list(plot = p, decile_rate = decile_rate)
  invisible(result)

}

#' Decile lift chart
#'
#' Decile wise lift chart.
#'
#' @param gains_table An object of class \code{blr_gains_table}.
#' @param xaxis_title X axis title.
#' @param yaxis_title Y axis title.
#' @param title Plot title.
#' @param bar_color Color of the bars.
#' @param text_size Size of the bar labels.
#' @param text_vjust Vertical justification of the bar labels.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'              family = binomial(link = 'logit'))
#' gt <- blr_gains_table(model)
#' blr_decile_lift_chart(gt)
#'
#' @family model validation techniques
#'
#' @export
#'
blr_decile_lift_chart <- function(gains_table, xaxis_title = "Decile",
                                  yaxis_title = "Decile Mean / Global Mean",
                                  title = "Decile Lift Chart",
                                  bar_color = "blue", text_size = 3.5,
                                  text_vjust = -0.3, print_plot = TRUE) {

  blr_check_gtable(gains_table)
  global_mean <- blr_prep_lchart_gmean(gains_table)
  lift_data   <- blr_prep_lchart_data(gains_table, global_mean)

  p <-
    ggplot(data = lift_data, aes(x = decile, y = d_by_g_mean)) +
    geom_col(fill = bar_color) + ggtitle(title) + xlab(xaxis_title) +
    geom_text(aes(label = round(d_by_g_mean, 2)), vjust = text_vjust,
      size = text_size) + ylab(yaxis_title)

  if (print_plot) {
    print(p)
  }

  result <- list(plot = p, decile_lift = lift_data, global_mean = global_mean)
  invisible(result)

}


gains_decile_count <- function(data) {
  round(nrow(data) / 10)
}


gains_table_prep <- function(model, data, test_data = FALSE) {

  if (test_data) {
    namu <- names(model$model)[1]
    response <- data[[namu]]
  } else {
    response <- model$model[1]
  }

  resp_tib <- data.frame(response = response)
  prob_tib <- data.frame(predicted = predict.glm(model, newdata = data, type = "response"))
  out <- cbind(resp_tib, prob_tib)
  colnames(out) <- c('response', 'predicted')
  return(out)

}

#' @importFrom data.table data.table setorder .N := setDF
gains_table_modify <- function(data, decile_count) {

  residual <- nrow(data) - (decile_count * 9)

  d1 <- data[c('response', 'predicted')]
  colnames(d1) <- c('response', 'prob')
  d1 <- data.table(d1)
  setorder(d1, -prob)
  d1[, decile := c(rep(1:9, each = decile_count),
                   rep(10, times = residual))]

  out <- d1[, .(total = .N, `1` = table(response)[[2]]), by = decile]
  setDF(out)
  return(out)

}

gains_table_mutate <- function(data) {

  data$`0`           <- data$total - data$`1`
  data$cum_1s        <- cumsum(data$`1`)
  data$cum_0s        <- cumsum(data$`0`)
  data$cum_total     <- cumsum(data$total)
  data$`cum_total_%` <- (data$cum_total / sum(data$total)) * 100
  data$`cum_1s_%`    <- (data$cum_1s / sum(data$`1`)) * 100
  data$`cum_0s_%`    <- (data$cum_0s / sum(data$`0`)) * 100
  data$ks            <- data$`cum_1s_%` - data$`cum_0s_%`
  data$tp            <- data$cum_1s
  data$tn            <- data$cum_0s[10] - data$cum_0s
  data$fp            <- data$cum_0s
  data$fn            <- data$cum_1s[10] - data$cum_1s
  data$sensitivity   <- (data$tp / (data$tp + data$fn)) * 100
  data$specificity   <- (data$tn / (data$tn + data$fp)) * 100
  data$accuracy      <- ((data$tp + data$tn) / data$cum_total[10]) * 100

  return(data)

}

gains_plot_data <- function(x) {

  x1 <- x$gains_table[c('cum_total_%', 'cum_1s_%')]
  x1$cum_total_per <-  x1$`cum_total_%` / 100
  x1$cum_1s_per    <-  x1$`cum_1s_%` / 100
  x1$cum_total_y   <-  x1$cum_total_per
  x2 <- x1[c('cum_total_per', 'cum_1s_per', 'cum_total_y')]
  x3 <- data.frame(cum_total_per = 0, cum_1s_per = 0, cum_total_y = 0)
  rbind(x3, x2)

}



