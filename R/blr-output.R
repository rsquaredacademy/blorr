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
    extract(2:4) %>%
    unlist(use.names = FALSE) %>%
    format(round(4), nsmall = 4) %>%
    nchar %>%
    max
  w25 <- 9
  w26 <- x$blr_cord %>%
    extract(5:7) %>%
    unlist(use.names = FALSE) %>%
    format(round(4), nsmall = 4) %>%
    nchar %>%
    max
  w27 <- sum(w23, w24, w25, w26, 21)

  cat(fc('Association of Predicted Probabilities and Observed Responses', w27), '\n')
  cat(rep("-", w27), sep = "", '\n')
  cat(fc('% Concordant', w23), fs2(),
      fc(format(round(x$blr_cord[[2]], 4), nsmall = 4), w24), fs2(),
      fc("Somers' D", w25), fs2(),
      fc(format(round(x$blr_cord[[5]], 4), nsmall = 4), w26),'\n')
  cat(fc('% Discordant', w23), fs2(),
      fc(format(round(x$blr_cord[[3]], 4), nsmall = 4), w24), fs2(),
      fc('Gamma', w25), fs2(),
      fc(format(round(x$blr_cord[[6]], 4), nsmall = 4), w26),'\n')
  cat(fc('% Tied', w23), fs2(),
      fc(format(round(x$blr_cord[[4]], 4), nsmall = 4), w24), fs2(),
      fc('Tau-a', w25), fs2(),
      fc(format(round(x$blr_cord[[7]], 4), nsmall = 4), w26),'\n')
  cat(rep("-", w27), sep = "", '\n\n')

}
