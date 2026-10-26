blr_reg_comp <- function(formula, data, odd_conf_limit, weights = NULL, offset = NULL) {

  form <- as.formula(formula)

  # Keep the formula's environment so variables created in a function or
  # local() remain visible, but make the weights/offset arguments visible to
  # model.frame() as well: it evaluates them in the formula environment, not in
  # this function's frame. Use names that cannot clash with the model data:
  # model.frame() looks in data first, so a data column named weights or offset
  # would otherwise be used instead of the vectors supplied to glm().
  .blr_weights <- weights
  .blr_offset  <- offset

  fit_env <- new.env(parent = environment(form))
  if (!is.null(.blr_weights)) {
    fit_env$.blr_weights <- .blr_weights
  }
  if (!is.null(.blr_offset)) {
    fit_env$.blr_offset <- .blr_offset
  }
  environment(form) <- fit_env

  if (is.null(weights) && is.null(offset)) {
    model <- glm(
      formula = form,
      data    = data,
      family  = binomial(link = "logit")
    )
  } else if (is.null(offset)) {
    model <- glm(
      formula = form,
      data    = data,
      weights = .blr_weights,
      family  = binomial(link = "logit")
    )
  } else if (is.null(weights)) {
    model <- glm(
      formula = form,
      data    = data,
      offset  = .blr_offset,
      family  = binomial(link = "logit")
    )
  } else {
    model <- glm(
      formula = form,
      data    = data,
      weights = .blr_weights,
      offset  = .blr_offset,
      family  = binomial(link = "logit")
    )
  }

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
