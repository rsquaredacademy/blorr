#' @importFrom magrittr %>% %<>% use_series extract2
#' @importFrom dplyr mutate if_else pull
#' @importFrom rlang sym eval_tidy !!
#' @importFrom glue glue
response_var <- function(model) {

  model %>%
    use_series(terms) %>%
    extract2(2)

}

# name of the data set
data_name <- function(model) {

  model %>%
    use_series(call) %>%
    extract2(4)

}

# number of observations
data_nrows <- function(model) {

  model %>%
    use_series(data) %>%
    nrow

}

# model convergence status
converge_status <- function(model) {

  model %>%
    use_series(converged)

}

# residual degrees of freedom
residual_df <- function(model) {

  model %>%
    use_series(df.residual)

}

# model degrees of freedom
model_df <- function(model) {

  model %>%
    use_series(df.null)

}

# log likelihood
model_loglik <- function(model) {

  model %>%
    use_series(deviance)

}

# null likelihood
null_ll <- function(model) {

  dep <- response_var(model)

  dat <- model %>%
    use_series(call) %>%
    use_series(data) %>%
    eval_tidy()

  glm(glue(dep, ' ~ 1'), data = dat,
      family = binomial(link = 'logit')) %>%
    logLik %>%
    extract2(1)

}

# response profile
resp_profile <- function(model) {

  resp <- model %>%
    response_var

  model %>%
    use_series(data) %>%
    pull(!!resp) %>%
    as.factor %>%
    table

}


#
