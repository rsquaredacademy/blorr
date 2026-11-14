#' Likelihood ratio test
#'
#' Performs the likelihood ratio test for full and reduced model.
#'
#' @param full_model An object of class \code{glm}; model with all predictors. Only binary logistic regression models (binomial family with logit link) are supported. Weighted models (any prior weight other than 1) and grouped binomial models (\code{cbind(successes, failures)} or proportions) are not supported; the model must be fitted with \code{model = TRUE, y = TRUE}.
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

  # validate the supplied model before any null-model reconstruction
  blr_check_model(full_model)

  if (missing(reduced_model)) {
    reduced_model <- lr_reduced_model(full_model)
  }

  blr_check_model(reduced_model)

  fm_class <- model_class(full_model)
  rm_class <- model_class(reduced_model)

  if (fm_class != "glm") {
    stop("full_model must be an object of class glm.", call. = FALSE)
  }

  if (rm_class != "glm") {
    stop("reduced_model must be an object of class glm.", call. = FALSE)
  }

  lr_check_comparable(full_model, reduced_model)

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

# full and reduced models must describe the same observations, response,
# weights and offset, and the reduced model must be nested in the full model
lr_check_comparable <- function(full_model, reduced_model) {

  if (!identical(family(full_model)$family, family(reduced_model)$family) ||
      !identical(family(full_model)$link, family(reduced_model)$link)) {
    stop("full_model and reduced_model must use the same family and link.",
      call. = FALSE
    )
  }

  # exact equality (names dropped): no tolerance is used anywhere in the
  # comparability or nesting checks
  same <- function(a, b) identical(as.numeric(unname(a)), as.numeric(unname(b)))
  n_f  <- length(full_model$y)

  if (!identical(nobs(full_model), nobs(reduced_model)) ||
      !same(full_model$y, reduced_model$y)) {
    stop("models were not fitted to the same observations.", call. = FALSE)
  }

  if (!same(full_model$prior.weights, reduced_model$prior.weights)) {
    stop("models were not fitted with the same weights.", call. = FALSE)
  }

  off <- function(m) if (is.null(m$offset)) rep(0, n_f) else m$offset

  if (!same(off(full_model), off(reduced_model))) {
    stop("models were not fitted with the same offset.", call. = FALSE)
  }

  # nesting is a structural check (see design_nested()): same response, rows,
  # weights and offset (checked above), and every estimated column of the
  # reduced model is an estimated column of the full model with the same name
  # and values on the informative rows (positive prior weight). If that cannot
  # be established exactly the test stops with an error.
  if (nrow(stats::model.matrix(full_model)) !=
      nrow(stats::model.matrix(reduced_model))) {
    stop("reduced_model must be nested within full_model.", call. = FALSE)
  }

  rows        <- fitted_rows(full_model)
  full_design <- fitted_design(full_model, rows)
  red_design  <- fitted_design(reduced_model, rows)

  if (!design_nested(full_design, red_design)) {
    stop("reduced_model must be nested within full_model: its estimated ",
         "terms must be a subset, by name and values, of the full model's ",
         "terms.", call. = FALSE)
  }

  if (stats::df.residual(reduced_model) -
      stats::df.residual(full_model) < 1) {
    stop("reduced_model must be nested within full_model and estimate fewer parameters.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

lr_test_result <- function(full_model, reduced_model) {

  full_model_ll    <- mll(full_model)
  reduced_model_ll <- mll(reduced_model)
  lr               <- reduced_model_ll - full_model_ll

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
