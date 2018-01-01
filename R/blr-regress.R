#' @title Ordinary Least Squares Regression
#' @description Ordinary Least Squares Regression
#' @param object an object of class "formula" (or one that can be coerced to
#' that class): a symbolic description of the model to be fitted or class
#' \code{glm}
#' @param ... other inputs
#' @examples
#' blr_regress(formula = honcomp ~ female + read + science, data = hsb2)
#' @export
#'
blr_regress <- function(formula, ...) UseMethod('blr_regress')


#' @export
#'
blr_regress.default <- function(formula, data, ...) {

  result        <- blr_reg_comp(formula, data)
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
  blr_regress.default(formula = formula, data = data)
}

#' @export
#'
print.blr_regress <- function(x, ...) {
  print_blr_reg(x)
}
