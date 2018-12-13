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

  blr_check_model(model)

  if (is.null(data)) {
    data <-
      model %>%
      magrittr::use_series(data)
  }

  blr_check_data(data)

  prob <- stats::predict.glm(model, newdata = data, type = "response")
  n    <- length(prob)

  data %>%
    dplyr::mutate(
      prob = stats::predict.glm(model, newdata = ., type = "response")
    ) %>%
    dplyr::arrange(prob) %>%
    dplyr::mutate(
      n      = seq_len(n()),
      prob_n = prob * n
    ) %>%
    dplyr::pull(prob_n) %>%
    sum() %>%
    magrittr::divide_by(prob %>%
      sum()) %>%
    magrittr::multiply_by(2) %>%
    magrittr::subtract(n %>%
      magrittr::add(1)) %>%
    magrittr::divide_by(n)

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

  blr_prep_lorenz_data(model, data, test_data) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = `cum_1s_per`, y = `cum_0s_per`),
                color = lorenz_curve_col) +
    ggplot2::geom_line(ggplot2::aes(x = `cum_1s_per`, y = `cum_1s_per`), color = diag_line_col) +
    ggplot2::ggtitle(label = title, subtitle = glue::glue("Gini Index = ", {g_index})) +
    ggplot2::xlab(xaxis_title) + 
    ggplot2::ylab(yaxis_title) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
          plot.subtitle = ggplot2::element_text(hjust = 0.5))

}


