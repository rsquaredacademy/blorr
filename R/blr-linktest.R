#' Model specification error
#'
#' Test for model specification error.
#'
#' @param model An object of class \code{glm}.
#'
#' @return An object of class \code{glm}.
#'
#' @references
#' Pregibon,  D.  1979.  Data  analytic  methods  for  generalized  linear  models.  PhD  diss.,  University  of  Toronto.
#'
#' Pregibon,  D.  1980.  Goodness  of  link  tests  for  generalized  linear  models.
#'
#' Tukey,  J.  W.  1949.  One  degree  of  freedom  for  non-additivity.
#'
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

  blr_check_model(model)

  dat    <- model$model
  fit    <- predict.glm(model, newdata = dat)
  fit2   <- fit ^ 2
  resp   <- model$y
  newdat <- data.frame(fit = fit, fit2 = fit2, resp = resp)

  link_model <- glm(resp ~ fit + fit2, data = newdat, family = binomial(link = "logit"))
  summary(link_model)
}
