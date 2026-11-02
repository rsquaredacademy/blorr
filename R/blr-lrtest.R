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

  if (!identical(nobs(full_model), nobs(reduced_model)) ||
      !isTRUE(all.equal(full_model$y, reduced_model$y))) {
    stop("models were not fitted to the same observations.", call. = FALSE)
  }

  if (!isTRUE(all.equal(full_model$prior.weights,
                        reduced_model$prior.weights))) {
    stop("models were not fitted with the same weights.", call. = FALSE)
  }

  if (!isTRUE(all.equal(full_model$offset, reduced_model$offset))) {
    stop("models were not fitted with the same offset.", call. = FALSE)
  }

  # matching term labels are not required either: nesting only requires the
  # reduced design to lie in the column space of the full design, which holds
  # for equivalent factor parameterizations such as treatment and sum
  # contrasts and for algebraically identical terms such as I(wt) and wt. The
  # designs are column-normalized before the projection residual is checked, so
  # mismatched rows, a reduced intercept the full model does not span, and a
  # same-named predictor fitted to different values are still rejected no
  # matter how any predictor is rescaled.
  full_design <- stats::model.matrix(full_model)
  red_design  <- stats::model.matrix(reduced_model)

  if (nrow(full_design) != nrow(red_design) || ncol(red_design) < 1L) {
    stop("reduced_model must be nested within full_model.", call. = FALSE)
  }

  # both designs are compared on the observations that carry information in
  # the full fit (the prior weights were checked to be equal above), and the
  # full design is reduced to its estimated, non-aliased columns
  rows        <- fitted_rows(full_model)
  full_design <- fitted_design(full_model, rows)
  red_design  <- red_design[rows, , drop = FALSE]

  nested <- design_contains(full_design, red_design)

  if (!nested) {
    if (attr(nested, "residual") < 1e-4) {
      stop("reduced_model must be nested within full_model; nesting could ",
           "not be confirmed to numerical precision, possibly because the ",
           "full design is nearly collinear.", call. = FALSE)
    }
    stop("reduced_model must be nested within full_model.", call. = FALSE)
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
