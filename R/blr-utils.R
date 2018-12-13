#' @importFrom magrittr %>% %<>% 
response_var <- function(model) {

  model %>%
    magrittr::use_series(terms) %>%
    magrittr::extract2(2)

}

# name of the data set
data_name <- function(model) {

  model %>%
    magrittr::use_series(call) %>%
    magrittr::extract2(4)

}

# number of observations
data_nrows <- function(model) {

  model %>%
    magrittr::use_series(data) %>%
    nrow()

}

# model convergence status
converge_status <- function(model) {

  model %>%
    magrittr::use_series(converged)

}

# residual degrees of freedom
residual_df <- function(model) {

  model %>%
    magrittr::use_series(df.residual)

}

# model degrees of freedom
model_df <- function(model) {

  model %>%
    magrittr::use_series(df.null)

}

# response profile
resp_profile <- function(model) {

  resp <- 
  	model %>%
    response_var()

  model %>%
    magrittr::use_series(data) %>%
    dplyr::pull(!! resp) %>%
    as.factor() %>%
    table()

}


# analysis of maximum likelihood estimates
predictor_names <- function(model) {

  model %>%
    magrittr::use_series(coefficients) %>%
    names()

}

# model df
predictor_df <- function(model) {

  model %>%
    magrittr::use_series(rank) %>%
    rep_len(x = 1)

}

# model estimate
predictor_est <- function(model) {

  model %>%
    magrittr::use_series(coefficients) %>%
    unname()

}

# extract columns from model summary
predictor_mine <- function(model, col_name = NULL) {

  model %>%
    summary() %>%
    magrittr::use_series(coefficients) %>%
    magrittr::extract(, col_name) %>%
    unname()

}

# standard error
predictor_se <- function(model) {

  predictor_mine(model, "Std. Error")

}

# z value
predictor_zval <- function(model) {

  predictor_mine(model, "z value")

}

# p values
predictor_pval <- function(model) {
  predictor_mine(model, "Pr(>|z|)")
}

# odds ratio estimate
# odds ratio effects
odds_effect <- function(model) {

  model %>%
    stats::coef() %>%
    names() %>%
    magrittr::extract(-1)

}

# odds ratio point estimates
odds_point <- function(model) {

  model %>%
    stats::coef() %>%
    exp(.) %>%
    magrittr::extract(-1) %>%
    unname()

}

# odds ratio confidence intervals
odds_conf_limit <- function(model) {

  model %>%
    stats::confint() %>%
    tibble::as_tibble() %>%
    dplyr::slice(2:n()) %>%
    exp(.)

}

# -2 log likelihood
mll <- function(model) {

  model %>%
    stats::logLik() %>%
    magrittr::extract(1) %>%
    magrittr::multiply_by(-2)

}

# model class
model_class <- function(model) {

  model %>%
    class() %>%
    magrittr::extract(1)

}

# create intercept only model
i_model <- function(model) {

  dep <- response_var(model)

  dat <-
    model %>%
	  magrittr::use_series(data) 

  stats::glm(
    glue::glue(dep, " ~ 1"), data = dat,
    family = stats::binomial(link = "logit")
  )

}

# model dfs
model_d_f <- function(model) {

  model %>%
    magrittr::use_series(coefficients) %>%
    length()

}

# extract log likelihood from blr_lr_test
extract_ll <- function(model, n = 1) {

  blr_test_lr(model) %>%
    magrittr::use_series(model_info) %>%
    dplyr::pull(log_lik) %>%
    magrittr::extract(n)

}

# log likelihood
model_ll <- function(model) {

  model %>%
    stats::logLik() %>%
    magrittr::extract(1)

}

# output formatting
fc <- function(x, w) {
  x <- as.character(x)
  ret <- format(x, width = w, justify = "centre")
  return(ret)
}

fs <- function() {
  x <- rep("  ")
  return(x)
}

fs1 <- function() {
  x <- rep("    ")
  return(x)
}

fs2 <- function() {
  x <- rep("     ")
  return(x)
}

fs3 <- function() {
  x <- rep("      ")
  return(x)
}

fs4 <- function() {
  x <- rep("         ")
  return(x)
}

f16 <- function() {
  x <- rep("                ")
  return(x)
}

fg <- function(x, w) {
  z <- as.character(x)
  y <- format(z, width = w, justify = "right")
  return(y)
}

fw <- function(x, w) {
  z <- format(as.character(x), width = w, justify = "right")
  return(z)
}

fl <- function(x, w) {
  x <- as.character(x)
  ret <- format(x, width = w, justify = "left")
  return(ret)
}

mod_sel_data <- function(model) {
  model %>%
    magrittr::use_series(data)
}
