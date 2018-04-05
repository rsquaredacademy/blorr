#' Gains table & lift chart
#'
#' Gains table.
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
#' @param ... Other inputs.
#'
#' @return A tibble.
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
#' @importFrom dplyr bind_cols arrange group_by summarise n summarise_all desc
#' @importFrom tibble add_column add_row
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

  if (is.null(data)) {
    test_data <- FALSE
    data      <- eval(model$call$data)
  } else {
    test_data <- TRUE
    data      <- data
  }

  decile_count <- gains_decile_count(data)

  gains_table <-
    gains_table_prep(model, data, test_data) %>%
    gains_table_modify(decile_count = decile_count) %>%
    gains_table_mutate()

  result <- list(gains_table = gains_table)
  class(result) <- c("blr_gains_table")

  return(result)

}

#' @export
#'
print.blr_gains_table <- function(x, ...) {

  x %>%
    use_series(gains_table) %>%
    select(
      -cum_1s, -cum_0s, -cum_total, -`cum_total_%`,
      -`cum_1s_%`, -`cum_0s_%`
    ) %>%
    print()

}

#' @rdname blr_gains_table
#' @export
#'
plot.blr_gains_table <- function(x, title = "Lift Chart", xaxis_title = "% Population",
                                 yaxis_title = "% Cumulative 1s", diag_line_col = "red",
                                 lift_curve_col = "blue", plot_title_justify = 0.5, ...) {

  gains_plot_data(x) %>%
    ggplot() +
    geom_line(aes(x = cum_total_per, y = cum_1s_per), color = lift_curve_col) +
    geom_line(aes(x = cum_total_per, y = cum_total_y), color = diag_line_col) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    theme(plot.title = element_text(hjust = plot_title_justify))

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
                         ks_line_color = "black") {

  ks_line    <- ks_chart_line(gains_table)
  annotate_y <- ks_chart_annotate_y(ks_line)
  ks_stat    <- ks_chart_stat(ks_line)
  annotate_x <- ks_chart_annotate_x(ks_line)

  ks_chart_data(gains_table) %>%
    ggplot(aes(x = cum_total_per)) +
    geom_line(aes(y = cum_1s_per, color = "Cumulative 1s %")) +
    geom_line(aes(y = cum_0s_per, color = "Cumulative 0s %")) +
    geom_point(aes(y = cum_1s_per, color = "Cumulative 1s %")) +
    geom_point(aes(y = cum_0s_per, color = "Cumulative 0s %")) +
    geom_segment(x = ks_line[[1]], xend = ks_line[[1]], y = ks_line[[3]],
      yend = ks_line[[2]], color = ks_line_color) +
    annotate("text", x = annotate_x, y = annotate_y,
             label = paste0("KS: ", ks_stat, "%")) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_blank())

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
                                    text_vjust = -0.3) {

  decile_rate <- decile_capture_rate(gains_table)

  p <- ggplot(data = decile_rate, aes(x = decile, y = decile_mean)) +
    geom_col(fill = bar_color) + ggtitle(title) + xlab(xaxis_title) +
    geom_text(aes(label = round(decile_mean, 2)), vjust = text_vjust,
      size = text_size) + ylab(yaxis_title)

  print(p)

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
                                  text_vjust = -0.3) {

  global_mean <- lift_chart_global_mean(gains_table)
  lift_data   <- lift_chart_data(gains_table, global_mean)

  p <- ggplot(data = lift_data, aes(x = decile, y = d_by_g_mean)) +
    geom_col(fill = bar_color) + ggtitle(title) + xlab(xaxis_title) +
    geom_text(aes(label = round(d_by_g_mean, 2)), vjust = text_vjust,
      size = text_size) + ylab(yaxis_title)

  print(p)

  result <- list(plot = p, decile_lift = lift_data, global_mean = global_mean)
  invisible(result)

}


gains_decile_count <- function(data) {

  data %>%
    nrow() %>%
    divide_by(10) %>%
    round()

}


gains_table_prep <- function(model, data, test_data = FALSE) {

  if (test_data) {
    namu <-
      model %>%
      formula() %>%
      extract2(2)

    response <- 
      data %>% 
      pull(!! namu)

  } else {
    response <- 
      model %>%
      model.frame() %>%
      model.response()
  }

  response %>%
    as_tibble() %>%
    bind_cols(predict.glm(model, newdata = data, type = "response") %>%
                as_tibble())

}

gains_table_modify <- function(data, decile_count) {

  residual <-
    data %>%
    nrow() %>%
    subtract((decile_count * 9))

  data %>%
    select(response = value, prob = value1) %>%
    arrange(desc(prob)) %>%
    add_column(decile = c(rep(1:9, each = decile_count),
                          rep(10, times = residual))) %>%
    group_by(decile) %>%
    summarise(total = n(), `1` = table(response)[[2]])
}

gains_table_mutate <- function(data) {

  data %>%
    mutate(
      `0`           = total - `1`,
      cum_1s        = cumsum(`1`),
      cum_0s        = cumsum(`0`),
      cum_total     = cumsum(total),
      `cum_total_%` = (cum_total / sum(total)) * 100,
      `cum_1s_%`    = (cum_1s / sum(`1`)) * 100,
      `cum_0s_%`    = (cum_0s / sum(`0`)) * 100,
      ks            = `cum_1s_%` - `cum_0s_%`,
      tp            = cum_1s,
      tn            = cum_0s[10] - cum_0s,
      fp            = cum_0s,
      fn            = cum_1s[10] - cum_1s,
      sensitivity   = (tp / (tp + fn)) * 100,
      specificity   = (tn / (tn + fp)) * 100,
      accuracy      = ((tp + tn) / cum_total[10]) * 100
    )

}

gains_plot_data <- function(x) {

  x %>%
    use_series(gains_table) %>%
    select(`cum_total_%`, `cum_1s_%`) %>%
    mutate(
      cum_total_per = `cum_total_%` / 100,
      cum_1s_per    = `cum_1s_%` / 100,
      cum_total_y   = cum_total_per
    ) %>%
    select(cum_total_per, cum_1s_per, cum_total_y) %>%
    add_row(cum_total_per = 0, cum_1s_per = 0, cum_total_y = 0, .before = 1)

}


ks_chart_line <- function(gains_table) {

  gains_table %>%
    use_series(gains_table) %>%
    select(`cum_total_%`, `cum_1s_%`, `cum_0s_%`, ks) %>%
    filter(ks == max(ks)) %>%
    divide_by(100)

}

ks_chart_annotate_y <- function(ks_line) {

  ks_line %>%
    mutate(
      ann_loc    = (`cum_1s_%` - `cum_0s_%`) / 2,
      ann_locate = `cum_0s_%` + ann_loc
    ) %>%
    pull(ann_locate)

}

ks_chart_stat <- function(ks_line) {

  ks_line %>%
    pull(4) %>%
    round(2) %>%
    multiply_by(100)

}


ks_chart_annotate_x <- function(ks_line) {

  ks_line %>%
    pull(1) +
    0.1

}

ks_chart_data <- function(gains_table) {

  gains_table %>%
    use_series(gains_table) %>%
    select(`cum_total_%`, `cum_1s_%`, `cum_0s_%`) %>%
    mutate(
      cum_total_per = `cum_total_%` / 100,
      cum_1s_per    = `cum_1s_%` / 100,
      cum_0s_per    = `cum_0s_%` / 100
    ) %>%
    select(cum_total_per, cum_1s_per, cum_0s_per) %>%
    add_row(cum_total_per = 0, cum_1s_per = 0, cum_0s_per = 0, .before = 1)

}


decile_capture_rate <- function(gains_table) {

  gains_table %>%
    use_series(gains_table) %>%
    select(decile, total, `1`) %>%
    mutate(
      decile_mean = `1` / total
    )

}

lift_chart_global_mean <- function(gains_table) {

  gains_table %>%
    use_series(gains_table) %>%
    select(total, `1`) %>%
    summarise_all(sum) %>%
    mutate(
      gmean = `1` / total
    ) %>%
    pull(gmean)

}

lift_chart_data <- function(gains_table, global_mean) {

  gains_table %>%
    use_series(gains_table) %>%
    select(decile, total, `1`) %>%
    mutate(
      decile_mean = `1` / total,
      d_by_g_mean = decile_mean / global_mean
    )

}
