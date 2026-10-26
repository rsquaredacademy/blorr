#' Binary logistic regression
#'
#' Binary logistic regression.
#'
#' @param object An object of class "formula" (or one that can be coerced to
#'   that class): a symbolic description of the model to be fitted or class
#'   \code{glm}.
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

  formula <- formula(object)
  data    <- object$model
  weights <- object$prior.weights
  offset  <- object$offset

  blr_regress.default(
    object         = formula,
    data           = data,
    odd_conf_limit = odd_conf_limit,
    weights        = weights,
    offset         = offset,
    ...
  )
}

#' @export
#'
print.blr_regress <- function(x, ...) {
  print_blr_reg(x)
}
