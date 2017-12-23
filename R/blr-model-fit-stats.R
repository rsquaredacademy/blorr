#' @importFrom stats AIC BIC logLik
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


