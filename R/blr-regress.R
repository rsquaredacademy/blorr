#' Binary logistic regression
#'
#' Binary logistic regression.
#'
#' @param object An object of class "formula" (or one that can be coerced to
#'   that class): a symbolic description of the model to be fitted or class
#'   \code{glm} with binomial family and logit link. Weighted models (any prior
#'   weight other than 1) and grouped binomial models
#'   (\code{cbind(successes, failures)} or proportions) are not supported; refit
#'   models stored without their model frame or response using
#'   \code{glm(..., model = TRUE, y = TRUE)}.
#' @param odd_conf_limit If TRUE, odds ratio confidence limits will be displayed.
#' @param ... Other inputs.
#'
#' @details
#' Odds ratio confidence limits are profile-likelihood intervals computed by
#' \code{\link[stats]{confint}}.
#'
#' @examples
#' # using formula
#' blr_regress(object = honcomp ~ female + read + science, data = hsb2)
#'
#' # using a model built with glm
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'              family = binomial(link = 'logit'))
#'
#' blr_regress(model)
#'
#' # odds ratio estimates
#' blr_regress(model, odd_conf_limit = TRUE)
#'
#' @export
#'
blr_regress <- function(object, ...) UseMethod("blr_regress")


#' @export
#'
blr_regress.default <- function(object, data, odd_conf_limit = FALSE, ...) {

	blr_check_data(data)
	blr_check_logic(odd_conf_limit)

  result <- blr_reg_comp(object, data, odd_conf_limit)
  
  class(result) <- "blr_regress"
  return(result)

}

#' @rdname blr_regress
#' @export
#'
blr_regress.glm <- function(object, odd_conf_limit = FALSE, ...) {

	blr_check_model(object)
	blr_check_logic(odd_conf_limit)

  result <- blr_reg_compute(object, odd_conf_limit)

  class(result) <- "blr_regress"
  return(result)
}

#' @export
#'
print.blr_regress <- function(x, ...) {
  print_blr_reg(x)
}
