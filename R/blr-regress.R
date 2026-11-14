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
#' The default method fits a binomial logit model with
#' \code{\link[stats]{glm}}. An \code{offset} may be supplied directly, and the
#' formula environment is kept. When a fitted \code{glm} object is supplied, the
#' fit is summarised directly, so an offset supplied through the model formula
#' and the formula environment are respected. Weighted models are rejected by
#' the shared model guard.
#'
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


#' @rdname blr_regress
#'
#' @param data A \code{data.frame} or \code{tibble} containing the variables in
#'   the model.
#' @param weights An optional vector of prior weights to be used in the fitting
#'   process.
#' @param offset An optional offset to be included in the model.
#'
#' @export
#'
blr_regress.default <- function(object, data, odd_conf_limit = FALSE,
                                weights = NULL, offset = NULL, ...) {

	blr_check_data(data)
	blr_check_logic(odd_conf_limit)

  result <- blr_reg_comp(
    formula        = object,
    data           = data,
    odd_conf_limit = odd_conf_limit,
    weights        = weights,
    offset         = offset
  )
  
  class(result) <- "blr_regress"
  return(result)

}

#' @rdname blr_regress
#' @export
#'
blr_regress.glm <- function(object, odd_conf_limit = FALSE, ...) {

	blr_check_model(object)
	blr_check_logic(odd_conf_limit)

  # summarise the supplied fit directly: refitting would lose the formula
  # environment, a transformed response or an offset supplied through the
  # formula. The fit already holds its own weights, offset and family.
  result <- blr_reg_compute(object, odd_conf_limit)

  class(result) <- "blr_regress"
  return(result)
}

#' @export
#'
print.blr_regress <- function(x, ...) {
  print_blr_reg(x)
}
