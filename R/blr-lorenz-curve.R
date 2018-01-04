#' @title Gini Index
#' @description Gini Index
#' @param model an object of class \code{glm}
#' @param data a tibble or a data.frame
#' @return Gini Index
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_gini_index(model)
#' @export
#'
blr_gini_index <- function(model, data = NULL) {

  if (is.null(data)) {
    data <- eval(model$call$data)
  }

  data$prob <- predict.glm(model, newdata = data, type = 'response')

  prob <- data %>%
    use_series(prob)

  n <- prob %>%
    length

  data %>%
    arrange(prob) %>%
    mutate(
      n = seq_len(n()),
      prob_n = prob * n
    ) %>%
    pull(prob_n) %>%
    sum %>%
    divide_by(prob %>%
                sum) %>%
    multiply_by(2) %>%
    subtract(n %>%
               add(1)) %>%
    divide_by(n)

}
