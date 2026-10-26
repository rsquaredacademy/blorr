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
#' The default method fits a binomial logit model with
#' \code{\link[stats]{glm}}. Prior \code{weights} and an \code{offset} may be
#' supplied directly. When a fitted \code{glm} object is supplied, the model is
#' refitted with its prior weights and offset; this includes an offset specified
#' in the model formula.
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

  data    <- object$model
  weights <- object$prior.weights
  offset  <- object$offset

  blr_regress.default(
    object         = refit_formula(object),
    data           = data,
    odd_conf_limit = odd_conf_limit,
    weights        = weights,
    offset         = offset,
    ...
  )
}

# Remove offset() terms from the formula of a fitted glm. The offset is passed
# to the refit through the offset argument instead: object$offset already holds
# the sum of any formula offset and any offset argument, so keeping both would
# count the formula offset twice. The original formula environment is retained
# so that variables created in a function or local() remain visible.
refit_formula <- function(object) {
  form <- formula(object)
  trm  <- terms(object)

  if (is.null(attr(trm, "offset"))) {
    return(form)
  }

  stats::reformulate(
    termlabels = attr(trm, "term.labels"),
    response   = form[[2]],
    intercept  = attr(trm, "intercept") == 1,
    env        = environment(form)
  )
}

#' @export
#'
print.blr_regress <- function(x, ...) {
  print_blr_reg(x)
}
