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
#' @seealso \code{\link[lmtest]{lrtest}}
#'
#' @importFrom magrittr multiply_by subtract %<>%
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
    stop(crayon::red("full_model must be an object of class glm."),
         call. = FALSE)
  }

  if (rm_class != "glm") {
    stop(crayon::red("reduced_model must be an object of class glm."),
      call. = FALSE)
  }

  model_info <- lr_model_info(full_model, reduced_model)
  test_info  <- lr_test_result(full_model, reduced_model)

  result <- list(model_info = model_info, test_result = test_info)
  class(result) <- "blr_test_lr"

  return(result)

}

#' @export
#'
print.blr_test_lr <- function(x, ...) {
  print_blr_lr_test(x)
}

lr_reduced_model <- function(full_model) {

  dep <- response_var(full_model)

  dat <-
    full_model %>%
    use_series(data)


  glm(paste0(dep, " ~ 1"), data = dat, family = binomial(link = "logit"))

}

lr_test_result <- function(full_model, reduced_model) {

  full_model_ll    <- mll(full_model)
  reduced_model_ll <- mll(reduced_model)
  lr               <- reduced_model_ll - full_model_ll

  df <-
    full_model %>%
    coefficients() %>%
    length() %>%
    subtract(1)

  pval <- pchisq(q = lr, df = df, lower.tail = FALSE)

  tibble(lr_ratio = lr,
         d_f      = df,
         p_value  = pval)

}


lr_model_info <- function(full_model, reduced_model) {

  full_model_formula <-
    full_model %>%
    use_series(formula)

  reduced_model_formula <-
    reduced_model %>%
    use_series(formula)

  full_model_df    <- model_d_f(full_model)
  reduced_model_df <- model_d_f(reduced_model)
  full_model_ll    <- mll(full_model)
  reduced_model_ll <- mll(reduced_model)

  tibble(model = c("full model", "reduced model"),
    formulas   = c(full_model    = full_model_formula,
                   reduced_model = reduced_model_formula),
    log_lik    = c(full_model_ll, reduced_model_ll),
    d_f        = c(full_model_df, reduced_model_df)
  )

}
