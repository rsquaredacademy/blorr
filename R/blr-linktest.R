#' @title Model Specification Error
#' @description Test for model specification error
#' @param model an object of class \code{glm}
#' @return an object of class \code{glm}
#' @examples
#'
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_linktest(model)
#'
#' @export
#'
blr_linktest <- function(model) {
  dat <- model %>%
    use_series(call) %>%
    use_series(data) %>%
    eval_tidy()

  fit <- model %>%
    predict.glm(newdata = dat)

  fit2 <- fit %>%
    raise_to_power(2)

  resp <- model %>%
    use_series(y)

  newdat <- tibble(fit = fit, fit2 = fit2, resp = resp)

  glm(
    resp ~ fit + fit2, data = newdat,
    family = binomial(link = "logit")
  ) %>%
    summary()
}
