#' @importFrom stats AIC BIC logLik deviance
#' @importFrom magrittr divide_by raise_to_power add
#' @title Model Fit Statistics
#' @description Model Fit Statistics
#' @param object an object of class "formula" (or one that can be coerced to
#' that class): a symbolic description of the model to be fitted or class
#' \code{glm}
#' @param ... other inputs
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_model_fit_stats(model)
#' @export
#'
blr_model_fit_stats <- function(model, ...) UseMethod('blr_model_fit_stats')

#' @export
#'
blr_model_fit_stats.default <- function(model, ...) {

  lr <- blr_lr_test(model)
  n <- model %>%
    use_series(y) %>%
    length

  result <- list(
    loglik_null = null_ll(model),
    loglik_model = model_ll(model),
    m_deviance = model_deviance(model),
    lr_ratio = lr %>%
      use_series(test_result) %>%
      pull(lr_ratio),
    lr_df = lr %>%
      use_series(test_result) %>%
      pull(d_f),
    lr_pval = lr %>%
      use_series(test_result) %>%
      pull(p_value),
    mcfadden = blr_mcfadden_rsq(model),
    adj_mcfadden = blr_mcfadden_adj_rsq(model),
    cox_snell = blr_cox_snell_rsq(model),
    nagelkerke = blr_nagelkerke_rsq(model),
    mckelvey = blr_mckelvey_zavoina_rsq(model),
    effron = blr_effron_rsq(model),
    count_r2 = blr_count_rsq(model),
    count_adj = blr_adj_count_rsq(model),
    m_aic = model_aic(model),
    m_bic = model_bic(model),
    dev_df = n %>%
      subtract(model %>%
                 coefficients %>%
                 length)
  )

  class(result) <- 'blr_model_fit_stats'
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
#' blr_mcfadden_rsq(model)
#' @export
#'
blr_mcfadden_rsq <- function(model) {

  f_model_ll <- model %>%
    model_ll

  i_model_ll <- model %>%
    i_model %>%
    model_ll

  1 %>%
    subtract(f_model_ll %>%
               divide_by(i_model_ll))

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

  f_model_ll <- model %>%
    model_ll

  i_model_ll <- model %>%
    i_model %>%
    model_ll

  k <- model %>%
    model_d_f

  1 %>%
    subtract((f_model_ll - k) %>%
               divide_by(i_model_ll))

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

  f_model_ll <- model %>%
    model_ll %>%
    exp

  i_model_ll <- model %>%
    i_model %>%
    model_ll %>%
    exp

  n <- model %>%
    use_series(data) %>%
    nrow

  ratio <- i_model_ll %>%
    divide_by(f_model_ll)

  pow <- 2 %>%
    divide_by(n)

  ratio_pow <- ratio %>%
    raise_to_power(pow)

  1 %>%
    subtract(ratio_pow)

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

  cox_snell <- blr_cox_snell_rsq(model)

  i_model_ll <- model %>%
    i_model %>%
    model_ll %>%
    exp

  n <- model %>%
    use_series(data) %>%
    nrow

  pow <- 2 %>%
    divide_by(n)

  den <- 1 %>%
    subtract(i_model_ll %>%
    raise_to_power(pow))

  cox_snell %>%
    divide_by(den)

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

  predicted <- model %>%
    predict

  mean_predicted <- predicted %>%
    mean

  ess <- predicted %>%
    subtract(mean_predicted) %>%
    raise_to_power(2) %>%
    sum

  pi_val <- pi %>%
    raise_to_power(2) %>%
    divide_by(3)

  n <- model %>%
    use_series(y) %>%
    length

  ess %>%
    divide_by((n %>%
                multiply_by(pi_val)) %>%
                add(ess))

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

  predicted <- model %>%
    predict(type = 'response')

  resp <- model %>%
    use_series(y)

  num <- resp %>%
    subtract(predicted) %>%
    raise_to_power(2) %>%
    sum

  mean_resp <- resp %>%
    mean

  den <- resp %>%
    subtract(mean_resp) %>%
    raise_to_power(2) %>%
    sum

  1 %>%
    subtract(num %>%
               divide_by(den))

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

  predicted <- model %>%
    predict(type = 'response')

  zero_one <- if_else(predicted >= 0.5, 1, 0)

  resp <- model %>%
    use_series(y)

  n <- resp %>%
    length

  correct <- if_else(zero_one == resp, 1, 0)

  correct %>%
    sum %>%
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

  n2 <- model %>%
    use_series(y) %>%
    table %>%
    max

  predicted <- model %>%
    predict(type = 'response')

  zero_one <- if_else(predicted >= 0.5, 1, 0)

  resp <- model %>%
    use_series(y)

  n <- resp %>%
    length

  correct <- if_else(zero_one == resp, 1, 0)

  num <- correct %>%
    sum %>%
    subtract(n2)

  den <- n %>%
    subtract(n2)

  num / den
}


