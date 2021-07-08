#' Model fit statistics
#'
#' Model fit statistics.
#'
#' @param model An object of class \code{glm}.
#' @param ... Other inputs.
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_model_fit_stats(model)
#'
#' @importFrom stats AIC BIC logLik deviance
#' 
#' @references
#' Menard, S. (2000). Coefficients of determination for multiple logistic regression analysis. 
#' The American Statistician, 54(1), 17-24.
#' 
#' Windmeijer,  F.  A.  G.  (1995).  Goodness-of-fit  measures  in  binary  choice  models. Econometric 
#' Reviews, 14, 101-116.
#' 
#' Hosmer,  D.W.,  Jr.,  &  Lemeshow,  S.  (2000), Applied  logistic  regression(2nd  ed.).  
#' New  York:  John Wiley & Sons. 
#' 
#' J. Scott Long & Jeremy Freese, 2000. "FITSTAT: Stata module to compute fit statistics for 
#' single equation regression models," Statistical Software Components S407201, Boston College 
#' Department of Economics, revised 22 Feb 2001. 
#' 
#' Freese, Jeremy and J. Scott Long.  Regression Models for Categorical Dependent Variables 
#' Using Stata. College Station: Stata Press, 2006.
#' 
#' Long, J. Scott. Regression Models for Categorical and Limited Dependent Variables.  
#' Thousand Oaks: Sage Publications, 1997.
#' 
#'
#' @family model fit statistics
#'
#' @export
#'
blr_model_fit_stats <- function(model, ...) UseMethod("blr_model_fit_stats")

#' @export
#'
blr_model_fit_stats.default <- function(model, ...) {

  blr_check_model(model)

  lr <- fit_stat(model)

  result <- list(
    loglik_null  = null_ll(model),
    loglik_model = model_ll(model),
    m_deviance   = model_deviance(model),
    lr_ratio     = lr$lr_ratio,
    lr_pval      = lr$lr_pval,
    mcfadden     = blr_rsq_mcfadden(model),
    dev_df       = lr$dev_df,
    adj_mcfadden = blr_rsq_mcfadden_adj(model),
    m_aic        = model_aic(model),
    cox_snell    = blr_rsq_cox_snell(model),
    m_bic        = model_bic(model),
    mckelvey     = blr_rsq_mckelvey_zavoina(model),
    lr_df        = lr$lr_df,
    effron       = blr_rsq_effron(model),
    nagelkerke   = blr_rsq_nagelkerke(model),
    count_r2     = blr_rsq_count(model),
    count_adj    = blr_rsq_adj_count(model))

  class(result) <- "blr_model_fit_stats"
  return(result)

}

#' @export
#'
print.blr_model_fit_stats <- function(x, ...) {
  print_model_fit_stats(x)
}

model_deviance <- function(model) {
  model$deviance
}

null_ll <- function(model) {

  logLik(i_model(model))[[1]]

}


model_ll <- function(model) {
  logLik(model)[1]
}


model_aic <- function(model) {
  AIC(model)
}


model_bic <- function(model) {
  BIC(model)
}

#' McFadden's R2
#'
#' McFadden's pseudo r-squared for the model.
#'
#' @param model An object of class \code{glm}.
#'
#' @return McFadden's r-squared.
#' 
#' @references
#' \url{https://eml.berkeley.edu/reprints/mcfadden/zarembka.pdf}
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_rsq_mcfadden(model)
#'
#' @family model fit statisitcs
#'
#' @export
#'
blr_rsq_mcfadden <- function(model) {

  blr_check_model(model)

  i_model_ll <- imodel(model)
  f_model_ll <- model_ll(model)
  1 - (f_model_ll/ i_model_ll)

}

#' McFadden's adjusted R2
#'
#' McFadden's adjusted pseudo r-squared for the model.
#'
#' @param model An object of class \code{glm}.
#'
#' @return McFadden's adjusted r-squared.
#' 
#' @references
#' \url{https://eml.berkeley.edu/reprints/mcfadden/zarembka.pdf}
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_rsq_mcfadden_adj(model)
#'
#' @family model fit statistics
#'
#' @export
#'
blr_rsq_mcfadden_adj <- function(model) {

  blr_check_model(model)

  i_model_ll <- imodel(model)
  f_model_ll <- model_ll(model) - model_d_f(model)
  1 - (f_model_ll / i_model_ll)

}

#' Cox Snell R2
#'
#' Cox Snell pseudo r-squared.
#' @param model An object of class \code{glm}.
#'
#' @return Cox Snell pseudo r-squared.
#' 
#' @references
#' Cox, D. R., & Snell, E. J. (1989). The analysis of binary data (2nd ed.).  London: Chapman and Hall. 
#' 
#' Maddala, G. S. (1983). Limited dependent and qualitative variables in economics. New York: Cambridge Press.
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_rsq_cox_snell(model)
#'
#' @family model fit statistics
#'
#' @export
#'
blr_rsq_cox_snell <- function(model) {

  blr_check_model(model)
  1 - cox_snell_comp(model)

}


#' Cragg-Uhler (Nagelkerke) R2
#'
#' Cragg-Uhler (Nagelkerke) R2 pseudo r-squared.
#'
#' @param model An object of class \code{glm}.
#'
#' @return Cragg-Uhler (Nagelkerke) R2 pseudo r-squared.
#' 
#' @references
#' Cragg, S. G., & Uhler, R. (1970). The demand for automobiles. Canadian Journal of Economics, 3, 386-406.
#' 
#' Maddala, G. S. (1983). Limited dependent and qualitative variables in economics. New York: Cambridge Press.
#' 
#' Nagelkerke, N. (1991). A note on a general definition of the coefficient of determination. 
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_rsq_nagelkerke(model)
#'
#' @family model fit statistics
#'
#' @export
#'
blr_rsq_nagelkerke <- function(model) {

  blr_check_model(model)

  n   <- nrow(model$data)
  num <- exp(((model_ll(model) * -2) - (imodel(model) * -2)) / n)
  den <- exp((imodel(model) * 2) / n )
  (1 - num) / (1 - den)

}

#' McKelvey Zavoina R2
#'
#' McKelvey Zavoina pseudo r-squared.
#'
#' @param model An object of class \code{glm}.
#'
#' @return Cragg-Uhler (Nagelkerke) R2 pseudo r-squared.
#' 
#' @references
#' McKelvey, R. D., & Zavoina,  W. (1975). A statistical model for the  analysis  of ordinal level dependent 
#' variables. Journal of Mathematical Sociology, 4, 103-12.
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_rsq_mckelvey_zavoina(model)
#'
#' @family model fit statistics
#'
#' @export
#'
blr_rsq_mckelvey_zavoina <- function(model) {

  blr_check_model(model)

  predicted      <- predict(model)
  mean_predicted <- mean(predicted)

  ess    <- sum((predicted - mean_predicted) ^ 2)
  pi_val <- (pi ^ 2) / 3
  n      <- (length(model$y) * pi_val) + ess
  ess / n

}


#' Effron R2
#'
#' Effron pseudo r-squared.
#'
#' @param model An object of class \code{glm}.
#'
#' @return Effron pseudo r-squared.
#' 
#' @references
#' Efron, B. (1978). Regression and ANOVA with zero-one data: Measures of residual variation. Journal of 
#' the American Statistical Association, 73, 113-121. 
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_rsq_effron(model)
#'
#' @family model fit statistics
#'
#' @export
#'
blr_rsq_effron <- function(model) {

  blr_check_model(model)

  predicted <- predict(model, type = "response")
  resp      <-model$y
  mean_resp <- mean(resp)
  den       <- sum((resp - mean_resp) ^ 2)
  num       <- sum((resp - predicted) ^ 2) / den
  1 - num

}


#' Count R2
#'
#' Count r-squared.
#'
#' @param model An object of class \code{glm}.
#'
#' @return Count r-squared.
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_rsq_count(model)
#'
#' @family model fit statistcs
#'
#' @export
#'
blr_rsq_count <- function(model) {

  blr_check_model(model)

  predicted <- predict(model, type = "response")
  zero_one  <- ifelse(predicted >= 0.5, 1, 0)
  resp      <- model$y
  n         <- length(resp)

  sum(ifelse(zero_one == resp, 1, 0)) / n

}

#' Adjusted count R2
#'
#' Adjusted count r-squared.
#'
#' @param model An object of class \code{glm}.
#'
#' @return Adjusted count r-squared.
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_rsq_adj_count(model)
#'
#' @family model fit statistics
#'
#' @export
#'
blr_rsq_adj_count <- function(model) {

  blr_check_model(model)

  resp      <- model$y
  n         <- length(resp)
  n2        <- max(table(resp))
  predicted <- predict(model, type = "response")
  zero_one  <- ifelse(predicted >= 0.5, 1, 0)
  den       <- n - n2

  (sum(ifelse(zero_one == resp, 1, 0)) - n2) / den 
  
}

fit_stat <- function(model) {

  lr <- blr_test_lr(model)

  pred_n   <- length(model$coefficients)
  dev_df   <- length(model$y) - pred_n
  lr_ratio <- extract_lr(lr, lr_ratio)
  lr_df    <- extract_lr(lr, d_f)
  lr_pval  <- extract_lr(lr, p_value)

  list(lr_ratio = lr_ratio,
       lr_df    = lr_df,
       lr_pval  = lr_pval,
       dev_df   = dev_df)

}

extract_lr <- function(lr, value) {

  vals <- deparse(substitute(value))
  lr$test_result[[vals]]

}

cox_snell_comp <- function(model) {

  n <- nrow(model$data)
  exp(((imodel(model) - model_ll(model)) * 2) / n)

}

imodel <- function(model) {
  model_ll(i_model(model))
}
