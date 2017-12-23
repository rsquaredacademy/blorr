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

# mcfadden's R2
mcfadden_rsq <- function(model) {

  f_model_ll <- model %>%
    extract_ll

  i_model_ll <- model %>%
    extract_ll(n = 2)

  1 %>%
    subtract(f_model_ll %>%
               divide_by(i_model_ll))

}
