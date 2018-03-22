#' Gini index
#'
#' Gini index is a measure of inequality and was developed to measure income
#'   inequality in labour market. In the predictive model, Gini Index is used
#'   for measuring inequality/discrimination.
#'
#' @param model An object of class \code{glm}.
#' @param data A \code{tibble} or \code{data.frame}.
#'
#' @return Gini index.
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
    data <- eval(model$call$data)
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
#' @description Lorenz curve is a visual representation of inequality.
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
                             xaxis_title = "Cumulative Population %",
                             yaxis_title = "Cumulative Events %",
                             diag_line_col = "red",
                             lorenz_curve_col = "blue") {

  if (is.null(data)) {
    data <- eval(model$call$data)
  }

  prob <- lorenz_curve_prob(data, model)
  n    <- lorenz_curve_n(prob)
  p    <- lorenz_curve_p(n)
  l    <- lorenz_curve_l(prob, n)

  g_index <-
    blr_gini_index(model = model, data = data) %>%
    round(2)

  tibble(p = p, l = l) %>%
    ggplot() + geom_line(aes(x = p, y = l), color = lorenz_curve_col) +
    geom_line(aes(x = p, y = p), color = diag_line_col) +
    ggtitle(label = title, subtitle = glue("Gini Index = ", {g_index})) +
    scale_x_continuous(labels = scales::percent) + xlab(xaxis_title) +
    scale_y_continuous(labels = scales::percent) + ylab(yaxis_title) +
    theme(plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5))

}


lorenz_curve_prob <- function(data, model) {

  data %>%
    mutate(
      prob = predict.glm(model, newdata = ., type = "response")
    ) %>%
    arrange(prob) %>%
    pull(prob)

}


lorenz_curve_n <- function(prob) {

  prob %>%
    length() %>%
    rep(x = 1)
}

lorenz_curve_p <- function(n) {

  cumsum(n) %>%
    divide_by(sum(n)) %>%
    prepend(0)

}

lorenz_curve_l <- function(prob, n) {

  prob_cum <- prob * n
  cumsum(prob_cum) %>%
    divide_by(sum(prob_cum)) %>%
    prepend(0)

}
