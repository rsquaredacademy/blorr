#' Gini index
#'
#' Gini index is a measure of inequality and was developed to measure income
#'   inequality in labour market. In the predictive model, Gini Index is used
#'   for measuring discriminatory power.
#'
#' @details
#' The Gini index is computed as 2 * AUC - 1, where AUC is the area under the
#' receiver operating characteristic curve.
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
    data      <- model$model
    test_data <- FALSE
  } else {
    test_data <- TRUE
  }

  blr_check_data(data)

  resp_name <- names(model$model)[1]

  if (test_data) {
    response <- data[[resp_name]]
  } else {
    response <- model$y
  }

  if (is.null(response)) {
    stop("`data` must contain the response variable used to fit the model.",
      call. = FALSE
    )
  }

  prob     <- predict.glm(model, newdata = data, type = "response")
  complete <- stats::complete.cases(response, prob)
  response <- response[complete]
  prob     <- prob[complete]
  event    <- event_indicator(response)

  r   <- rank(prob)
  n1  <- sum(event)
  n0  <- sum(!event)
  auc <- (sum(r[event]) - n1 * (n1 + 1) / 2) / (n1 * n0)

  2 * auc - 1

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
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
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
                             lorenz_curve_col = "blue", print_plot = TRUE) {

  if (is.null(data)) {
    test_data <- FALSE
    data      <- model$model
  } else {
    test_data <- TRUE
    data      <- data
  }

  g_index <- round(blr_gini_index(model = model, data = data), 2)

  p <-
    ggplot(blr_prep_lorenz_data(model, data, test_data)) +
    geom_line(aes(x = `cum_1s_per`, y = `cum_0s_per`),
                color = lorenz_curve_col) +
    geom_line(aes(x = `cum_1s_per`, y = `cum_1s_per`), color = diag_line_col) +
    ggtitle(label = title, subtitle = paste0("Gini Index = ", g_index)) +
    xlab(xaxis_title) + ylab(yaxis_title) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

  if (print_plot) {
    print(p)
  }

  invisible(p)

}


