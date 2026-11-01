#' @importFrom stats coef confint binomial df.residual glm terms nobs
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
  unname(nobs(model))
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

# event indicator: missing responses stay missing; for factor/character
# responses unused levels are dropped before the event (second) level is
# identified, as in glm(). When `event` is supplied it identifies the event
# value explicitly, which lets validation functions use the fitted model's
# event class even when a test sample has different or unused levels.
event_indicator <- function(resp, event = NULL) {
  missing <- is.na(resp)

  if (!is.null(event)) {
    if (is.logical(resp)) {
      out <- resp %in% TRUE
    } else {
      out <- as.character(resp) == as.character(event)
    }

    out[missing] <- NA
    return(out)
  }

  if (is.factor(resp)) {
    resp <- droplevels(resp)
    lvls <- levels(resp)

    if (length(lvls) < 2) {
      out <- rep(FALSE, length(resp))
    } else {
      out <- resp == lvls[2]
    }

    out[missing] <- NA
    return(out)
  }

  if (is.logical(resp)) {
    out        <- resp %in% TRUE
    out[missing] <- NA
    return(out)
  }

  if (is.numeric(resp)) {
    out        <- resp == 1
    out[missing] <- NA
    return(out)
  }

  lvls <- levels(droplevels(factor(resp)))

  if (length(lvls) < 2) {
    out <- rep(FALSE, length(resp))
  } else {
    out <- as.character(resp) == lvls[2]
  }

  out[missing] <- NA
  out
}

# event value used by the fitted model: the second declared response level for
# factors, TRUE for logical responses and 1 for numeric 0/1 responses
model_event_level <- function(model) {
  resp <- model$model[[1]]

  if (is.null(resp)) {
    return(NULL)
  }

  if (is.factor(resp)) {
    lvls <- levels(resp)

    if (length(lvls) < 2) {
      return(NULL)
    }

    return(lvls[2])
  }

  if (is.logical(resp)) {
    return(TRUE)
  }

  if (is.numeric(resp)) {
    return(1)
  }

  lvls <- levels(droplevels(factor(resp)))

  if (length(lvls) < 2) {
    return(NULL)
  }

  lvls[2]
}

# event indicator aligned with the fitted model's response encoding
model_event_indicator <- function(model, resp) {
  event_indicator(resp, event = model_event_level(model))
}

# labels, in non-event/event order, of the response used to fit the model
model_response_levels <- function(model) {
  resp <- model$model[[1]]

  if (is.null(resp)) {
    return(NULL)
  }

  if (is.factor(resp)) {
    lvls <- levels(resp)

    if (length(lvls) < 2) {
      return(NULL)
    }

    return(lvls[1:2])
  }

  if (is.logical(resp)) {
    return(c("FALSE", "TRUE"))
  }

  if (is.numeric(resp)) {
    return(c("0", "1"))
  }

  lvls <- levels(droplevels(factor(resp)))

  if (length(lvls) < 2) {
    return(NULL)
  }

  lvls[1:2]
}

# response profile
resp_profile <- function(model) {
  table(model$model[[1]])
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

# formula for a variable selection model; an intercept-only model is used
# when no predictors are left
selection_formula <- function(response, predictors) {
  if (length(predictors) == 0) {
    paste0(response, " ~ 1")
  } else {
    paste(response, "~", paste(predictors, collapse = " + "))
  }
}

# fit a variable selection model, including the intercept-only model. When the
# original fitted model is supplied, its weights, offset, family/link and
# contrasts are carried over so selection compares comparable models.
selection_model <- function(response, predictors, data, fit = NULL) {

  form <- stats::as.formula(selection_formula(response, predictors))

  if (is.null(fit)) {
    return(glm(
      form, data = data,
      family = binomial(link = "logit")
    ))
  }

  args <- list(
    formula  = form,
    data     = data,
    family   = fit$family,
    weights  = fit$prior.weights
  )

  ctr <- selection_contrasts(fit, form)

  if (!is.null(ctr)) {
    args$contrasts <- ctr
  }

  if (!is.null(fit$offset)) {
    args$offset <- fit$offset
  }

  do.call(stats::glm, args)
}

# keep only the contrasts of variables that remain in a selection formula
selection_contrasts <- function(fit, form) {
  if (is.null(fit$contrasts) || length(fit$contrasts) == 0) {
    return(NULL)
  }

  keep <- intersect(names(fit$contrasts), all.vars(form))

  if (length(keep) == 0) {
    return(NULL)
  }

  fit$contrasts[keep]
}

# create intercept only model, preserving the original fit's weights, offset,
# family/link, contrasts and (already complete) fitted rows
i_model <- function(model) {

  dat  <- model$model
  resp <- names(dat)[1]
  form <- stats::as.formula(paste0("`", resp, "` ~ 1"))

  args <- list(
    formula   = form,
    data      = dat,
    family    = model$family,
    weights   = model$prior.weights
  )

  ctr <- selection_contrasts(model, form)

  if (!is.null(ctr)) {
    args$contrasts <- ctr
  }

  if (!is.null(model$offset)) {
    args$offset <- model$offset
  }

  do.call(stats::glm, args)

}

# model dfs: number of estimated parameters (fitted rank), not the number of
# coefficient names (which includes aliased coefficients)
model_d_f <- function(model) {
  model$rank
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
