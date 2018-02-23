#' @importFrom stats AIC BIC logLik deviance
#' @importFrom magrittr divide_by raise_to_power add
#' @title Model Fit Statistics
#' @description Model Fit Statistics
#' @param model an object of class \code{glm}
#' @param ... other inputs
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_model_fit_stats(model)
#' @export
#'
blr_model_fit_stats <- function(model, ...) UseMethod("blr_model_fit_stats")

#' @export
#'
blr_model_fit_stats.default <- function(model, ...) {

  lr <- fit_stat(model)

  result <- list(
    loglik_null  = null_ll(model),
    loglik_model = model_ll(model),
    m_deviance   = model_deviance(model),
    lr_ratio     = lr$lr_ratio,
    lr_pval      = lr$lr_pval,
    mcfadden     = blr_mcfadden_rsq(model),
    dev_df       = lr$dev_df,
    adj_mcfadden = blr_mcfadden_adj_rsq(model),
    m_aic        = model_aic(model),
    cox_snell    = blr_cox_snell_rsq(model),
    m_bic        = model_bic(model),
    mckelvey     = blr_mckelvey_zavoina_rsq(model),
    lr_df        = lr$lr_df,
    effron       = blr_effron_rsq(model),
    nagelkerke   = blr_nagelkerke_rsq(model),
    count_r2     = blr_count_rsq(model),
    count_adj    = blr_adj_count_rsq(model))

  class(result) <- "blr_model_fit_stats"
  return(result)

}

#' @export
#'
print.blr_model_fit_stats <- function(x, ...) {
  print_model_fit_stats(x)
}

model_deviance <- function(model) {

  model %>%
    use_series(deviance)

}

null_ll <- function(model) {

  i_model(model) %>%
    logLik() %>%
    extract2(1)

}


model_ll <- function(model) {

  model %>%
    logLik() %>%
    extract(1)

}


model_aic <- function(model) {
  AIC(model)
}


model_bic <- function(model) {
  BIC(model)
}

#' @title McFadden's R2
#' @description McFadden's pseudo r-squared for the model.
#' @param model an object of class \code{glm}
#' @return McFadden's r-squared
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_mcfadden_rsq(model)
#' @export
#'
blr_mcfadden_rsq <- function(model) {

  i_model_ll <- imodel(model)
  f_model_ll <- model_ll(model)
  1 - (f_model_ll/ i_model_ll)

}

#' @title McFadden's Adjusted R2
#' @description McFadden's adjusted pseudo r-squared for the model.
#' @param model an object of class \code{glm}
#' @return McFadden's adjusted r-squared
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_mcfadden_adj_rsq(model)
#' @export
#'
blr_mcfadden_adj_rsq <- function(model) {

  i_model_ll <- imodel(model)
  f_model_ll <- model_ll(model) - model_d_f(model)
  1 - (f_model_ll / i_model_ll)

}

#' @title Cox Snell R2
#' @description Cox Snell pseudo r-squared
#' @param model an object of class \code{glm}
#' @return Cox Snell pseudo r-squared
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_cox_snell_rsq(model)
#' @export
#'
blr_cox_snell_rsq <- function(model) {

  1 - cox_snell_comp(model)

}


#' @title Cragg-Uhler (Nagelkerke) R2
#' @description Cragg-Uhler (Nagelkerke) R2 pseudo r-squared
#' @param model an object of class \code{glm}
#' @return Cragg-Uhler (Nagelkerke) R2 pseudo r-squared
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_nagelkerke_rsq(model)
#' @export
#'
blr_nagelkerke_rsq <- function(model) {

  n <-
    model %>%
    use_series(data) %>%
    nrow()

  pow <- 2 / n

  i_model_ll <-
    imodel(model) %>%
    exp() %>%
    raise_to_power(pow)

  blr_cox_snell_rsq(model) / (1 - i_model_ll)

}

#' @title McKelvey Zavoina R2
#' @description McKelvey Zavoina pseudo r-squared
#' @param model an object of class \code{glm}
#' @return Cragg-Uhler (Nagelkerke) R2 pseudo r-squared
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_mckelvey_zavoina_rsq(model)
#' @export
#'
blr_mckelvey_zavoina_rsq <- function(model) {

  predicted      <- predict(model)
  mean_predicted <- mean(predicted)

  ess <-
    predicted %>%
    subtract(mean_predicted) %>%
    raise_to_power(2) %>%
    sum()

  pi_val <-
    pi %>%
    raise_to_power(2) %>%
    divide_by(3)

  n <-
    model %>%
    use_series(y) %>%
    length() %>%
    multiply_by(pi_val) %>%
    add(ess)

  ess / n

}


#' @title  Effron R2
#' @description  Effron pseudo r-squared
#' @param model an object of class \code{glm}
#' @return Effron pseudo r-squared
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_effron_rsq(model)
#' @export
#'
blr_effron_rsq <- function(model) {

  predicted <- predict(model, type = "response")
  resp      <-model$y
  mean_resp <- mean(resp)

  den <-
    resp %>%
    subtract(mean_resp) %>%
    raise_to_power(2) %>%
    sum()

  num <-
    resp %>%
    subtract(predicted) %>%
    raise_to_power(2) %>%
    sum() %>%
    divide_by(den)

  1 - num

}


#' @title Count R2
#' @description  Count r-squared
#' @param model an object of class \code{glm}
#' @return Count r-squared
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_count_rsq(model)
#' @export
#'
blr_count_rsq <- function(model) {

  predicted <- predict(model, type = "response")
  zero_one  <- if_else(predicted >= 0.5, 1, 0)
  resp      <- model$y
  n         <- length(resp)

  if_else(zero_one == resp, 1, 0) %>%
    sum() %>%
    divide_by(n)

}

#' @title Adjusted Count R2
#' @description  Adjusted Count r-squared
#' @param model an object of class \code{glm}
#' @return Adjusted Count r-squared
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_adj_count_rsq(model)
#' @export
#'
blr_adj_count_rsq <- function(model) {

  n2 <-
    model %>%
    use_series(y) %>%
    table() %>%
    max()

  predicted <- predict(model, type = "response")
  zero_one  <- if_else(predicted >= 0.5, 1, 0)
  resp      <- model$y
  n         <- length(resp)
  den       <- n - n2

  if_else(zero_one == resp, 1, 0) %>%
    sum() %>%
    subtract(n2) %>%
    divide_by(den)

}

fit_stat <- function(model) {

  lr <- blr_lr_test(model)

  pred_n <-
    model %>%
    coefficients() %>%
    length()

  dev_df <-
    model %>%
    use_series(y) %>%
    length() %>%
    subtract(pred_n)

  lr_ratio <- extract_lr(lr, lr_ratio)
  lr_df    <- extract_lr(lr, d_f)
  lr_pval  <- extract_lr(lr, p_value)

  list(lr_ratio = lr_ratio,
       lr_df    = lr_df,
       lr_pval  = lr_pval,
       dev_df   = dev_df)

}

extract_lr <- function(lr, value) {

  vals <- enquo(value)

  lr %>%
    use_series(test_result) %>%
    pull(!! vals)

}

cox_snell_comp <- function(model) {

  f_model_ll <-
    model %>%
    model_ll() %>%
    exp()

  n <-
    model %>%
    use_series(data) %>%
    nrow()

  pow <- 2 / n

  imodel(model) %>%
    exp() %>%
    divide_by(f_model_ll) %>%
    raise_to_power(pow)

}

imodel <- function(model) {

  model %>%
    i_model() %>%
    model_ll()

}
