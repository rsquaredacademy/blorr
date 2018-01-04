#' @importFrom cli cat_line
#' @importFrom purrr map_int
#' @importFrom rlang prepend
print_blr_reg <- function(x) {

  w1 <- max(nchar('Data Set'), nchar(x$dataname))
  w2 <- max(nchar('Resp Var'), nchar(x$resp_name))
  w3 <- max(nchar('Obs.'), nchar(x$n_obs))
  w4 <- max(nchar('Df. Model'), nchar(x$mod_df))
  w5 <- max(nchar('Df. Residual'), nchar(x$resid_df))
  w6 <- max(nchar('Convergence'), nchar(x$converge))
  w7 <- sum(w1, w2, w3, w4, w5, w6,20)

  cat(fc('Model Overview', w7), '\n')
  cat(rep("-", w7), sep = "", '\n')
  cat(fg('Data Set', w1), fs(), fg('Resp Var', w2), fs(), fg('Obs.', w3),
      fs(), fg('Df. Model', w4), fs(), fc('Df. Residual', w5), fs(),
      fc('Convergence', w6), '\n')
  cat(rep("-", w7), sep = "", '\n')
  cat(fc(x$dataname, w1), fs(), fc(x$resp_name, w2), fs(), fc(x$n_obs, w3),
      fs(), fc(x$mod_df, w4), fs(), fc(x$resid_df, w5), fs(),
      fc(x$converge, w6), '\n')
  cat(rep("-", w7), sep = "", '\n\n')

  w8 <- nchar('Binary Outcome')
  w9 <- c('Frequency', x$resp_prof[[1]], x$resp_prof[[2]]) %>%
    map_int(nchar) %>% max
  w10 <- sum(w8, w9, 4)

  cat(fc('Response Summary', w10), '\n')
  cat(rep("-", w10), sep = "", '\n')
  cat(fc('Binary Outcome', w8), fs(), fc('Frequency', w9), '\n')
  cat(rep("-", w10), sep = "", '\n')
  cat(fc(0, w8), fs(), fc(x$resp_prof[[1]], w9), '\n')
  cat(fc(1, w8), fs(), fc(x$resp_prof[[2]], w9), '\n')
  cat(rep("-", w10), sep = "", '\n\n')

  w11 <- c('Parameter', x$parameter) %>%
    map_int(nchar) %>% max
  w12 <- 2
  w13 <- c('Estimate', format(round(x$est, 4), nsmall = 4)) %>%
    map_int(nchar) %>% max
  w14 <- c('Std.Error', format(round(x$se, 4), nsmall = 4)) %>%
    map_int(nchar) %>% max
  w15 <- c('z value', format(round(x$zval, 4), nsmall = 4)) %>%
    map_int(nchar) %>% max
  w16 <- c('Pr(>|z|)', format(round(x$pval, 4), nsmall = 4)) %>%
    map_int(nchar) %>% max
  w17 <- sum(w11, w12, w13, w14, w15, w16, 20)

  mln <- length(x$parameter)

  cat(fc('Maximum Likelihood Estimates', w17), '\n')
  cat(rep("-", w17), sep = "", '\n')
  cat(fc('Parameter', w11), fs(), fc('DF', w12), fs(), fc('Estimate', w13),
      fs(), fg('Std. Error', w14), fs(), fl('z value', w15), fs(),
      fg('Pr(>|z|)', w16), '\n')
  cat(rep("-", w17), sep = "", '\n')
  for (i in seq_len(mln)) {
    cat(fc(x$parameter[i], w11), fs(), fc(x$df[i], w12), fs(),
        fc(format(round(x$est[i], 4), nsmall = 4), w13),
        fs(), fg(format(round(x$se[i], 4), nsmall = 4), w14), fs(),
        fw(format(round(x$zval[i], 4), nsmall = 4), w15), fs(),
        fg(format(round(x$pval[i], 4), nsmall = 4), w16), '\n')
  }
  cat(rep("-", w17), sep = "", '\n\n')

  # odds ration estimates
  w18 <- c('Odds Ratio', x$blr_effects) %>%
    nchar %>%
    max
  w19 <- c('Estimate', x$blr_odds_point) %>%
    nchar %>%
    max
  w20 <- x$blr_conf %>%
    use_series(`2.5 %`) %>%
    format(round(4), nsmall = 4) %>%
    prepend('95% Wald') %>%
    nchar %>%
    max
  w21 <- x$blr_conf %>%
    use_series(`97.5 %`) %>%
    format(round(4), nsmall = 4) %>%
    prepend('Conf. Limit') %>%
    nchar %>%
    max

  w22 <- sum(w18, w19, w20, w21, 12)

  oddn <- length(x$blr_effects)

  cat(fc('Odds Ratio Estimates', w22), '\n')
  cat(rep("-", w22), sep = "", '\n')
  cat(fc('Effects', w18), fs(), fc('Estimate', w19), fs(), ' ', fg('95% Wald', w20),
      fl('Conf. Limit', w21), '\n')
  cat(rep("-", w22), sep = "", '\n')
  for (i in seq_len(oddn)) {
    cat(fc(x$blr_effects[i], w18), fs(),
        fc(format(round(x$blr_odds_point[i], 4), nsmall = 4), w19), fs(),
        fc(format(round(x$blr_conf$`2.5 %`[i], 4), nsmall = 4), w20),
        fs(), fg(format(round(x$blr_conf$`97.5 %`[i], 4), nsmall = 4), w21), '\n')
  }
  cat(rep("-", w22), sep = "", '\n\n')

  w23 <- 12
  w24 <- x$blr_cord %>%
    extract(1:4) %>%
    unlist(use.names = FALSE) %>%
    format(round(4), nsmall = 4) %>%
    nchar %>%
    max
  w25 <- 9
  w26 <- x$blr_cord %>%
    extract(5:8) %>%
    unlist(use.names = FALSE) %>%
    format(round(4), nsmall = 4) %>%
    nchar %>%
    max
  w27 <- sum(w23, w24, w25, w26, 21)

  cat(fc('Association of Predicted Probabilities and Observed Responses', w27), '\n')
  cat(rep("-", w27), sep = "", '\n')
  cat(fl('% Concordant', w23), fs2(),
      fc(format(round(x$blr_cord[[2]], 4), nsmall = 4), w24), fs2(),
      fl("Somers' D", w25), fs2(),
      fc(format(round(x$blr_cord[[5]], 4), nsmall = 4), w26),'\n')
  cat(fl('% Discordant', w23), fs2(),
      fc(format(round(x$blr_cord[[3]], 4), nsmall = 4), w24), fs2(),
      fl('Gamma', w25), fs2(),
      fc(format(round(x$blr_cord[[6]], 4), nsmall = 4), w26),'\n')
  cat(fl('% Tied', w23), fs2(),
      fc(format(round(x$blr_cord[[4]], 4), nsmall = 4), w24), fs2(),
      fl('Tau-a', w25), fs2(),
      fc(format(round(x$blr_cord[[7]], 4), nsmall = 4), w26),'\n')
  cat(fl('Pairs', w23), fs2(),
      fc(x$blr_cord[[1]], w24), fs2(),
      fl('c', w25), fs2(),
      fc(format(round(x$blr_cord[[8]], 4), nsmall = 4), w26),'\n')
  cat(rep("-", w27), sep = "", '\n\n')

  # model fit stats
  # w28 <- 24
  # w30 <- 27
  # w29 <- c(x$modfit$loglik_null, x$modfit$m_deviance, x$modfit$m_bic) %>%
  #   format(round(3), nsmall = 3) %>%
  #   nchar %>%
  #   max
  # w31 <- c(x$modfit$loglik_model, x$modfit$lr_ratio, x$modfit$m_aic) %>%
  #   format(round(3), nsmall = 3) %>%
  #   nchar %>%
  #   max
  # w <- sum(w28, w29, w30, w31, 12)
  #
  # cat(fc('Model Fit Statistics', w), '\n')
  # cat(rep("-", w), sep = "", '\n')
  # col1names <- c('Log-Lik Intercept Only:', glue('Deviance(', x$modfit$dev_df, '):'), '',
  #                "MCFadden's R2", 'ML (Cox-Snell) R2:',
  #                "McKelvey & Zavoina's R2:", 'Count R2:', 'BIC:')
  # col3names <- c('Log-Lik Full Model:', glue('LR(', x$modfit$lr_df, '):'), 'Prob > LR:',
  #                "McFadden's Adj R2:", "Cragg-Uhler(Nagelkerke) R2:",
  #                "Efron's R2:", "Adj Count R2:", "AIC:")
  # col2vals <- c(x$modfit$loglik_null, x$modfit$m_deviance, x$modfit$mcfadden, x$modfit$cox_snell,
  #               x$modfit$mckelvey, x$modfit$count_r2, x$modfit$m_bic) %>%
  #   round(3) %>%
  #   format(nsmall = 3) %>%
  #   prepend(value = '', before = 3)
  # col4vals <- c(x$modfit$loglik_model, x$modfit$lr_ratio, x$modfit$lr_pval, x$modfit$adj_mcfadden,
  #               x$modfit$nagelkerke, x$modfit$effron, x$modfit$count_adj, x$modfit$m_aic) %>%
  #   round(3) %>%
  #   format(nsmall = 3)
  # nlen <- length(col3names)
  # for (i in seq_len(nlen)) {
  #   cat(fl(col1names[i], w28), fs(),
  #       fg(col2vals[i], w29), fs(),
  #       fl(col3names[i], w30), fs(),
  #       fg(col4vals[i], w31), '\n')
  # }
  # cat(rep("-", w), sep = "", '\n\n')

}


print_model_fit_stats <- function(x) {

  w1 <- 24
  w3 <- 27
  w2 <- c(x$loglik_null, x$m_deviance, x$m_bic) %>%
    format(round(3), nsmall = 3) %>%
    nchar %>%
    max
  w4 <- c(x$loglik_model, x$lr_ratio, x$m_aic) %>%
    format(round(3), nsmall = 3) %>%
    nchar %>%
    max
  w <- sum(w1, w2, w3, w4, 12)

  cat(fc('Model Fit Statistics', w), '\n')
  cat(rep("-", w), sep = "", '\n')
  col1names <- c('Log-Lik Intercept Only:', glue('Deviance(', x$dev_df, '):'), '',
                 "MCFadden's R2", 'ML (Cox-Snell) R2:',
                 "McKelvey & Zavoina's R2:", 'Count R2:', 'BIC:')
  col3names <- c('Log-Lik Full Model:', glue('LR(', x$lr_df, '):'), 'Prob > LR:',
                 "McFadden's Adj R2:", "Cragg-Uhler(Nagelkerke) R2:",
                 "Efron's R2:", "Adj Count R2:", "AIC:")
  col2vals <- c(x$loglik_null, x$m_deviance, x$mcfadden, x$cox_snell,
                x$mckelvey, x$count_r2, x$m_bic) %>%
    round(3) %>%
    format(nsmall = 3) %>%
    prepend(values = '', before = 3)
  col4vals <- c(x$loglik_model, x$lr_ratio, x$lr_pval, x$adj_mcfadden,
                x$nagelkerke, x$effron, x$count_adj, x$m_aic) %>%
    round(3) %>%
    format(nsmall = 3)
  nlen <- length(col3names)
  for (i in seq_len(nlen)) {
    cat(fl(col1names[i], w1), fs(),
        fg(col2vals[i], w2), fs(),
        fl(col3names[i], w3), fs(),
        fg(col4vals[i], w4), '\n')
  }
  cat(rep("-", w), sep = "", '\n\n')
}
