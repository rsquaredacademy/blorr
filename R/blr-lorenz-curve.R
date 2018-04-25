#' Gini index
#'
#' Gini index is a measure of inequality and was developed to measure income
#'   inequality in labour market. In the predictive model, Gini Index is used
#'   for measuring discriminatory power.
#'
#' @param model An object of class \code{glm}.
#' @param data A \code{tibble} or \code{data.frame}.
#'
#' @return Gini index.
#'
#' @references
#' Siddiqi  N  (2006):  Credit  Risk  Scorecards:  developing  and  implementing  intelligent
#' credit  scoring. New Jersey, Wiley.
#'
#' Müller M, Rönz B (2000): Credit Scoring using Semiparametric Methods. In: Franke J, Härdle W, Stahl G (Eds.):
#' Measuring Risk in Complex Stochastic Systems. New York, Springer-Verlag.
#'
#' Kočenda  E,  Vojtek  M  (2011):  Default  Predictors  in  Retail  Credit  Scoring:  Evidence  from  Czech
#' Banking Data. Forthcoming in: Emerging Markets Finance and Trade.
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_gini_index(model)
#'
#' @family model validation techniques
#'
#' @export
#'
blr_gini_index <- function(model, data = NULL) {

  if (is.null(data)) {
    data <-
      model %>%
      use_series(data)
  }

  prob <- predict.glm(model, newdata = data, type = "response")
  n    <- length(prob)

  data %>%
    mutate(
      prob = predict.glm(model, newdata = ., type = "response")
    ) %>%
    arrange(prob) %>%
    mutate(
      n      = seq_len(n()),
      prob_n = prob * n
    ) %>%
    pull(prob_n) %>%
    sum() %>%
    divide_by(prob %>%
      sum()) %>%
    multiply_by(2) %>%
    subtract(n %>%
      add(1)) %>%
    divide_by(n)

}

#' Lorenz curve
#'
#' @description Lorenz curve is a visual representation of inequality. It is
#'   used to measure the discriminatory power of the predictive model.
#'
#' @param model An object of class \code{glm}.
#' @param data A \code{tibble} or \code{data.frame}.
#' @param title Plot title.
#' @param xaxis_title X axis title.
#' @param yaxis_title Y axis title.
#' @param lorenz_curve_col Color of the lorenz curve.
#' @param diag_line_col Diagonal line color.
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_lorenz_curve(model)
#'
#' @family model validation techniques
#'
#' @export
#'
blr_lorenz_curve <- function(model, data = NULL, title = "Lorenz Curve",
                             xaxis_title = "Cumulative Events %",
                             yaxis_title = "Cumulative Non Events %",
                             diag_line_col = "red",
                             lorenz_curve_col = "blue") {

  if (is.null(data)) {
    test_data <- FALSE
    data      <- model$data
  } else {
    test_data <- TRUE
    data      <- data
  }

  g_index <-
    blr_gini_index(model = model, data = data) %>%
    round(2)

  decile_count <- lorenz_decile_count(data)

  gains_table_prep(model, data, test_data) %>%
    lorenz_table_modify(decile_count = decile_count) %>%
    gains_table_mutate() %>%
    lorenz_plot_data() %>%
    ggplot() +
    geom_line(aes(x = `cum_1s_per`, y = `cum_0s_per`),
                color = lorenz_curve_col) +
    geom_line(aes(x = `cum_1s_per`, y = `cum_1s_per`), color = diag_line_col) +
    ggtitle(label = title, subtitle = glue("Gini Index = ", {g_index})) +
    xlab(xaxis_title) + ylab(yaxis_title) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

}


lorenz_decile_count <- function(data) {

  data %>%
    nrow() %>%
    divide_by(10) %>%
    round()

}

lorenz_table_modify <- function(data, decile_count) {

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

lorenz_plot_data <- function(gains_table) {

  gains_table %>%
    select(`cum_0s_%`, `cum_1s_%`) %>%
    mutate(
      cum_0s_per    = `cum_0s_%` / 100,
      cum_1s_per    = `cum_1s_%` / 100
    ) %>%
    select(cum_0s_per, cum_1s_per) %>%
    add_row(cum_0s_per = 0, cum_1s_per = 0, .before = 1) %>%
    add_row(cum_0s_per = 1, cum_1s_per = 1)

}
