#' @importFrom magrittr multiply_by subtract %<>%
#' @importFrom stats coefficients pchisq formula
#' @title Likelihood Ratio Test
#' @description Performs the likelihood ratio test for full and reduced model.
#' @param full_model an object of class \code{glm}; model with all predictors
#' @param reduced_model an object of class \code{glm}; nested model. Optional if
#' you are comparing the \code{full_model} with an intercept only model.
#' @return two tibbles with model information and test results
#' @examples
#' # compare full model with intercept only model
#' # full model
#' model_1 <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_lr_test(model_1)
#'
#' # compare full model with nested model
#' # nested model
#' model_2 <- glm(honcomp ~ female + read, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_lr_test(model_1, model_2)
#' @seealso \code{\link[lmtest]{lrtest}}
#' @export
blr_lr_test <- function(full_model, reduced_model) {

  # create intercept only model
  if (missing(reduced_model)) {
    reduced_model <- i_model(full_model)
  }

  # error handling
  fm_class <- model_class(full_model)
  rm_class <- model_class(reduced_model)

  if (fm_class != 'glm') {
    stop(crayon::red('full_model must be an object of class glm.'),
         call. = FALSE)
  }

  if (rm_class != 'glm') {
    stop(crayon::red('reduced_model must be an object of class glm.'),
         call. = FALSE)
  }

  # -2 log likelihood
  full_model_ll <- mll(full_model)
  reduced_model_ll <- mll(reduced_model)

  # likelihood ratio, df and p value
  lr <- reduced_model_ll - full_model_ll

  df <- full_model %>%
    coefficients %>%
    length %>%
    subtract(1)

  pval <- pchisq(q = lr, df = df, lower.tail = FALSE)

  # model formula
  full_model_formula <- full_model %>%
    use_series(formula)
  reduced_model_formula <- reduced_model %>%
    use_series(formula)

  # dfs
  full_model_df <- model_d_f(full_model)
  reduced_model_df <- model_d_f(reduced_model)

  # output
  model_info <- tibble(
    model = c('full model', 'reduced model'),
    formulas = c(full_model = full_model_formula,
                 reduced_model = reduced_model_formula),
    log_lik = c(full_model_ll, reduced_model_ll),
    d_f = c(full_model_df, reduced_model_df)
  )

  test_info <- tibble(
    lr_ratio = lr,
    d_f = df,
    p_value = pval
  )

  result <- list(model_info = model_info, test_result = test_info)

  return(result)

}
