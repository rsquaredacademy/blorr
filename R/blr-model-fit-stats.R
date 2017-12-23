#' @importFrom stats AIC BIC logLik deviance
#' @importFrom magrittr divide_by
model_deviance <- function(model) {

  model %>%
    use_series(deviance)

}

# log likelihood intercept only model
null_ll <- function(model) {

  i_model(model) %>%
    logLik %>%
    extract2(1)

}

# log likelihood full model
model_ll <- function(model) {

  model %>%
    logLik %>%
    extract(1)

}


model_aic <- function(model) {

  model %>%
    AIC

}


model_bic <- function(model) {

  model %>%
    BIC

}

#' @title McFadden's R2
#' @description McFadden's pseudo r-squared for the model.
#' @param model an object of class \code{glm}
#' @return McFadden's r-squared
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' mcfadden_rsq(model)
#' @export
#'
mcfadden_rsq <- function(model) {

  f_model_ll <- model %>%
    extract_ll

  i_model_ll <- model %>%
    extract_ll(n = 2)

  1 %>%
    subtract(f_model_ll %>%
               divide_by(i_model_ll))

}
