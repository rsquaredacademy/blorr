#' @importFrom magrittr %>% %<>% use_series extract2 extract
#' @importFrom dplyr mutate if_else pull slice
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang sym eval_tidy !!
#' @importFrom stats coef confint binomial df.residual glm terms
#' @importFrom utils data
response_var <- function(model) {
  model$terms[[2]]
}

# name of the data set
data_name <- function(model) {
  model$call[[4]]
}

# number of observations
data_nrows <- function(model) {
  nrow(model$data)
}

# model convergence status
converge_status <- function(model) {
  model$converged
}

# residual degrees of freedom
residual_df <- function(model) {
  model$df.residual
}

# model degrees of freedom
model_df <- function(model) {
  model$df.null
}

# response profile
resp_profile <- function(model) {
  resp <- response_var(model)
  table(as.factor(model$data[[resp]]))
}


# analysis of maximum likelihood estimates
predictor_names <- function(model) {
  names(model$coefficients)
}

# model df
predictor_df <- function(model) {
  rep_len(1, model$rank)
}

# model estimate
predictor_est <- function(model) {
  unname(model$coefficients)
}

# extract columns from model summary
predictor_mine <- function(model, col_name = NULL) {
  unname(summary(model)$coefficients[, col_name])
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
  names(coef(model))[-1]
}

# odds ratio point estimates
odds_point <- function(model) {
  unname(exp(coef(model))[-1])
}

# odds ratio confidence intervals
odds_conf_limit <- function(model) {

  n   <- length(model$coefficients)
  out <- exp(data.frame(confint(model))[2:n, ])
  colnames(out) <- c('`2.5 %`', '`97.5 %`')
  return(out)

}

# -2 log likelihood
mll <- function(model) {
  (logLik(model)[1]) * -2
}

# model class
model_class <- function(model) {
  class(model)[1]
}

# create intercept only model
i_model <- function(model) {

  dep <- response_var(model)
  dat <- model$data

  glm(
    paste0(dep, " ~ 1"), data = dat,
    family = binomial(link = "logit")
  )

}

# model dfs
model_d_f <- function(model) {
  length(model$coefficients)
}

# extract log likelihood from blr_lr_test
extract_ll <- function(model, n = 1) {
  blr_test_lr(model)$model_info[['log_lik']][n]
}

# log likelihood
model_ll <- function(model) {
  logLik(model)[1]
}

# output formatting
fc <- function(x, w) {
  x   <- as.character(x)
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
  x   <- as.character(x)
  ret <- format(x, width = w, justify = "left")
  return(ret)
}

mod_sel_data <- function(model) {
  model$data
}

#' @importFrom utils packageVersion menu install.packages
check_suggests <- function(pkg) {

  pkg_flag <- tryCatch(utils::packageVersion(pkg), error = function(e) NA)

  if (is.na(pkg_flag)) {

    msg <- message(paste0('\n', pkg, ' must be installed for this functionality.'))

    if (interactive()) {
      message(msg, "\nWould you like to install it?")
      if (utils::menu(c("Yes", "No")) == 1) {
        utils::install.packages(pkg)
      } else {
        stop(msg, call. = FALSE)
      }
    } else {
      stop(msg, call. = FALSE)
    }
  }

}
