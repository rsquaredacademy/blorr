#' Likelihood ratio test
#'
#' Performs the likelihood ratio test for full and reduced model.
#'
#' @param full_model An object of class \code{glm}; model with all predictors.
#' @param reduced_model An object of class \code{glm}; nested model. Optional if
#'   you are comparing the \code{full_model} with an intercept only model.
#'
#' @return Two tibbles with model information and test results.
#'
#' @examples
#' # compare full model with intercept only model
#' # full model
#' model_1 <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_test_lr(model_1)
#'
#' # compare full model with nested model
#' # nested model
#' model_2 <- glm(honcomp ~ female + read, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_test_lr(model_1, model_2)
#'
#'
#' @importFrom stats coefficients pchisq formula
#'
#' @family model fit statistics
#'
#' @export
#'
blr_test_lr <- function(full_model, reduced_model) UseMethod("blr_test_lr")

#' @rdname blr_test_lr
#' @export
#'
blr_test_lr.default <- function(full_model, reduced_model) {

  if (missing(reduced_model)) {
    reduced_model <- lr_reduced_model(full_model)
  }

  blr_check_model(full_model)
  blr_check_model(reduced_model)

  fm_class <- model_class(full_model)
  rm_class <- model_class(reduced_model)

  if (fm_class != "glm") {
    stop("full_model must be an object of class glm.", call. = FALSE)
  }

  if (rm_class != "glm") {
    stop("reduced_model must be an object of class glm.", call. = FALSE)
  }

  model_info <- lr_model_info(full_model, reduced_model)
  test_info  <- lr_test_result(full_model, reduced_model)

  result <- list(model_info = model_info, test_result = test_info)
  class(result) <- "blr_test_lr"

  invisible(result)

}

#' @export
#'
print.blr_test_lr <- function(x, ...) {
  print_blr_lr_test(x)
}

lr_reduced_model <- function(full_model) {
  i_model(full_model)
}

lr_test_result <- function(full_model, reduced_model) {

  lr <- deviance(reduced_model) - deviance(full_model)

  df   <- df.residual(reduced_model) - df.residual(full_model)
  pval <- pchisq(q = lr, df = df, lower.tail = FALSE)

  data.frame(lr_ratio = lr,
             d_f      = df,
             p_value  = pval)

}


lr_model_info <- function(full_model, reduced_model) {

  full_model_df    <- model_d_f(full_model)
  reduced_model_df <- model_d_f(reduced_model)
  full_model_ll    <- mll(full_model)
  reduced_model_ll <- mll(reduced_model)

  data.frame(model = c("full model", "reduced model"),
    log_lik    = c(full_model_ll, reduced_model_ll),
    d_f        = c(full_model_df, reduced_model_df)
  )

}
