#' @importFrom stats AIC BIC logLik deviance
#' @importFrom magrittr divide_by raise_to_power
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







