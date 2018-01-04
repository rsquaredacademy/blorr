#' @title Binary Logistic Regression
#' @description Binary logistic regression
#' @param object an object of class "formula" (or one that can be coerced to
#' that class): a symbolic description of the model to be fitted or class
#' \code{glm}
#' @param ... other inputs
#' @examples
#' blr_regress(object = honcomp ~ female + read + science, data = hsb2)
#' @export
#'
blr_regress <- function(object, ...) UseMethod('blr_regress')


#' @export
#'
blr_regress.default <- function(object, data, ...) {

  result        <- blr_reg_comp(object, data)
  class(result) <- 'blr_regress'
  return(result)
}

#' @rdname blr_regress
#' @export
#'
blr_regress.glm <- function(object, ...) {
  formula <- formula(object)
  # data    <- model.frame(object)
  data <- eval(object$call$data)
  blr_regress.default(object = formula, data = data)
}

#' @export
#'
print.blr_regress <- function(x, ...) {
  print_blr_reg(x)
}
