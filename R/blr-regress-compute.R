blr_reg_comp <- function(formula, data, odd_conf_limit) {

  model <- glm(formula = formula, data = data,
               family = binomial(link = "logit"))

  if (interactive()) {
    cat(paste("-", "Creating model overview."), "\n")
  }
  
  dataname       <- data_name(model)
  resp_name      <- response_var(model)
  n_obs          <- data_nrows(model)
  converge       <- converge_status(model)
  resid_df       <- residual_df(model)
  mod_df         <- model_df(model)

  if (interactive()) {
    cat(paste("-", "Creating response profile."), "\n")
  }
  
  resp_prof      <- resp_profile(model)

  if (interactive()) {
    cat(paste("-", "Extracting maximum likelihood estimates."), "\n")
  }

  parameter      <- predictor_names(model)
  df             <- predictor_df(model)
  est            <- predictor_est(model)
  se             <- predictor_se(model)
  zval           <- predictor_zval(model)
  pval           <- predictor_pval(model)

  if (odd_conf_limit) {

    if (interactive()) {
      cat(paste("-", "Computing odds ratio estimates."), "\n")
    }

    blr_effects    <- odds_effect(model)
    blr_odds_point <- odds_point(model)
    blr_conf       <- suppressMessages(odds_conf_limit(model))

  }

  if (interactive()) {
    cat(paste("-", "Estimating concordant and discordant pairs."), "\n")
  }

  blr_cord       <- blr_pairs(model)

  if (odd_conf_limit) {

    result <- list(
      dataname       = dataname,
      resp_name      = resp_name,
      n_obs          = n_obs,
      converge       = converge,
      resid_df       = resid_df,
      mod_df         = mod_df,
      resp_prof      = resp_prof,
      parameter      = parameter,
      df             = df,
      est            = est,
      se             = se,
      zval           = zval,
      pval           = pval,
      blr_effects    = blr_effects,
      blr_odds_point = blr_odds_point,
      blr_conf       = blr_conf,
      blr_cord       = blr_cord,
      odds_conf      = odd_conf_limit
    )

  } else {

    result <- list(
      dataname       = dataname,
      resp_name      = resp_name,
      n_obs          = n_obs,
      converge       = converge,
      resid_df       = resid_df,
      mod_df         = mod_df,
      resp_prof      = resp_prof,
      parameter      = parameter,
      df             = df,
      est            = est,
      se             = se,
      zval           = zval,
      pval           = pval,
      blr_cord       = blr_cord,
      odds_conf      = odd_conf_limit
    )

  }


  return(result)
}
