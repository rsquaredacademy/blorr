#' @title Model Specification Error
#' @description Test for model specification error
#' @param model an object of class \code{glm}
#' @return an object of class \code{glm}
#' @examples
#'
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_linktest(model)
#'
#' @export
#'
blr_linktest <- function(model) {

  dat <-
    model %>%
    use_series(call) %>%
    use_series(data) %>%
    eval_tidy()

  fit    <- predict.glm(model, newdata = dat)
  fit2   <- fit ^ 2
  resp   <- model$y
  newdat <- tibble(fit = fit, fit2 = fit2, resp = resp)

  glm(resp ~ fit + fit2, data = newdat, family = binomial(link = "logit")) %>%
    summary()

}
