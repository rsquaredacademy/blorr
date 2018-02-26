#' Binary logistic regression
#'
#' Binary logistic regression.
#'
#' @param object An object of class "formula" (or one that can be coerced to
#'   that class): a symbolic description of the model to be fitted or class
#'   \code{glm}.
#' @param ... Other inputs.
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
#' @export
#'
blr_regress <- function(object, ...) UseMethod("blr_regress")


#' @export
#'
blr_regress.default <- function(object, data, ...) {
  result <- blr_reg_comp(object, data)
  class(result) <- "blr_regress"
  return(result)
}

#' @rdname blr_regress
#' @export
#'
blr_regress.glm <- function(object, ...) {

  formula <- formula(object)
  data    <- eval(object$call$data)
  blr_regress.default(object = formula, data = data)
}

#' @export
#'
print.blr_regress <- function(x, ...) {
  print_blr_reg(x)
}
