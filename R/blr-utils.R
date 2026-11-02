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
# event class even when a test sample has different or unused levels. When the
# fitted response classes are supplied as `levels`, any other observed label is
# an error rather than silently becoming a non-event.
event_indicator <- function(resp, event = NULL, levels = NULL) {
  missing <- is.na(resp)

  if (!is.null(event)) {
    labels <- as.character(resp)

    if (!is.null(levels)) {
      levels <- as.character(levels)
      bad    <- !missing & !(labels %in% levels)

      if (any(bad)) {
        stop("response contains values not present in the fitted model: ",
          paste(unique(labels[bad]), collapse = ", "),
          call. = FALSE
        )
      }
    }

    if (is.logical(resp)) {
      out <- resp %in% TRUE
    } else {
      out <- labels == as.character(event)
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
  event_indicator(
    resp,
    event  = model_event_level(model),
    levels = model_response_levels(model)
  )
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

# normalize each column of a design matrix to unit 2-norm; zero columns are
# left as zero so they do not affect the column-space comparison. Each column is
# first divided by its largest absolute value, so the 2-norm is computed on
# values of order one and cannot overflow (very large predictors) or underflow
# to zero (very small predictors).
column_normalize <- function(x) {

  if (any(!is.finite(x))) {
    stop("model design contains non-finite values.", call. = FALSE)
  }

  mx <- apply(abs(x), 2, max)
  mx[mx == 0] <- 1
  x  <- sweep(x, 2, mx, "/")

  nrm <- sqrt(colSums(x * x))
  nrm[nrm == 0] <- 1
  sweep(x, 2, nrm, "/")
}

# is every column of `reduced` in the column space of `full`? Predictor
# rescaling must not change the answer, so both designs are column-normalized
# to unit 2-norm before each normalized reduced column is projected onto the
# normalized full column space. Each residual is then measured relative to the
# column's own (unit) scale with a tolerance that depends only on the number of
# observations, so an unrelated large predictor cannot make a non-nested design
# appear nested.
design_contains <- function(full, reduced) {

  full    <- column_normalize(full)
  reduced <- column_normalize(reduced)

  qr_full <- qr(full)
  tol     <- sqrt(.Machine$double.eps) * max(1, sqrt(nrow(full)))

  for (j in seq_len(ncol(reduced))) {
    if (max(abs(qr.resid(qr_full, reduced[, j, drop = FALSE]))) > tol) {
      return(FALSE)
    }
  }

  TRUE
}

# formula for a variable selection model; an intercept-only model is used
# when no predictors are left. Candidates are refit on the stored model frame,
# whose columns are the evaluated variables of the fitted formula (for example
# a `log(wt)` column, or separate `x` and `z` columns for an `x:z` term). Each
# term label is therefore rebuilt as a language object from the model frame
# columns it uses: a variable is referred to by its column name as a symbol and
# an interaction joins its variables with `:`. Building calls rather than
# pasting strings keeps interactions grouped and never quotes a name twice.
selection_term_exprs <- function(fit) {

  tt   <- stats::terms(fit)
  labs <- attr(tt, "term.labels")

  if (length(labs) == 0) {
    return(list())
  }

  fac  <- attr(tt, "factors")
  vars <- names(fit$model)[seq_len(nrow(fac))]

  out <- lapply(seq_along(labs), function(j) {
    v <- lapply(vars[fac[, j] > 0], as.name)
    Reduce(function(a, b) call(":", a, b), v)
  })

  names(out) <- labs
  out
}

selection_formula <- function(fit, predictors) {

  resp <- as.name(names(fit$model)[1])

  if (length(predictors) == 0) {
    rhs <- 1
  } else {
    ex <- selection_term_exprs(fit)
    if (!all(predictors %in% names(ex))) {
      stop("selection candidate is not a term of the fitted model.",
           call. = FALSE)
    }
    rhs <- Reduce(function(a, b) call("+", a, b), ex[predictors])
  }

  form <- eval(call("~", resp, rhs))
  environment(form) <- environment(stats::terms(fit))
  form
}

# the candidate refit above uses stored model frame column names, so its terms
# say `log(wt)` is a column. The returned model should behave like a glm fitted
# directly on the original expressions (for example `predict(newdata)` on raw
# data computes log(wt) again). The refit's coefficients are kept and its
# terms, formula, call, model frame and coefficient names are replaced by the
# original-expression versions, after checking that the original terms
# reproduce exactly the same design matrix from the stored model frame. If the
# check cannot be made, the stored-frame refit is returned unchanged: its
# estimates are the same, only its formula refers to stored column names.
selection_restore <- function(lit, fit, predictors) {

  out <- tryCatch(selection_restore_impl(lit, fit, predictors),
                  error = function(e) NULL)

  if (is.null(out)) lit else out
}

selection_restore_impl <- function(lit, fit, predictors) {

  tt       <- stats::terms(fit)
  env      <- environment(tt)
  vars_all <- as.list(attr(tt, "variables"))[-1]
  mf_names <- names(fit$model)
  resp     <- vars_all[[attr(tt, "response")]]

  rhs_parts <- lapply(predictors, str2lang)
  off       <- attr(tt, "offset")
  if (!is.null(off)) {
    rhs_parts <- c(rhs_parts, vars_all[off])
  }

  rhs <- if (length(rhs_parts) == 0) {
    1
  } else {
    Reduce(function(a, b) call("+", a, b), rhs_parts)
  }

  form_call <- call("~", resp, rhs)
  form      <- eval(form_call)
  environment(form) <- env
  new_tt    <- stats::terms(form)

  # map each variable of the new terms to the fitted model's variables
  vars_new <- as.list(attr(new_tt, "variables"))[-1]
  idx <- vapply(vars_new, function(v) {
    hit <- which(vapply(vars_all, identical, logical(1), v))
    if (length(hit) != 1L) NA_integer_ else hit
  }, integer(1))

  if (anyNA(idx)) {
    return(NULL)
  }

  pv <- attr(tt, "predvars")
  if (!is.null(pv)) {
    attr(new_tt, "predvars") <- as.call(c(quote(list), as.list(pv)[-1][idx]))
  }

  dc <- attr(tt, "dataClasses")
  if (!is.null(dc)) {
    attr(new_tt, "dataClasses") <- dc[mf_names[idx]]
  }

  # model frame: original variable columns plus the refit's (weights)/(offset)
  fr     <- fit$model[idx]
  extras <- names(lit$model)[startsWith(names(lit$model), "(")]
  for (nm in extras) {
    fr[[nm]] <- lit$model[[nm]]
  }
  attr(fr, "terms")     <- new_tt
  attr(fr, "na.action") <- attr(lit$model, "na.action")

  x_new <- stats::model.matrix(new_tt, fr, contrasts.arg = lit$contrasts)
  x_lit <- stats::model.matrix(lit)

  if (!identical(dim(x_new), dim(x_lit)) ||
      !isTRUE(all.equal(unname(x_new), unname(x_lit),
                        check.attributes = FALSE, tolerance = 0))) {
    return(NULL)
  }

  cn <- colnames(x_new)
  names(lit$coefficients) <- cn
  colnames(lit$qr$qr)     <- cn
  if (!is.null(lit$R)) {
    dimnames(lit$R) <- list(cn, cn)
  }
  rk <- seq_len(lit$rank)
  names(lit$effects)[rk] <- cn[lit$qr$pivot[rk]]

  lit$terms   <- new_tt
  lit$model   <- fr
  lit$formula <- form

  cl <- fit$call
  if (is.null(cl)) {
    cl <- lit$call
  }
  cl$formula <- form_call
  lit$call   <- cl

  lit
}

# weights to pass back to glm() when refitting from a stored model frame. For a
# grouped binomial response the trial counts are part of the response, and
# glm() multiplies the supplied weights by them, so the trial counts stored in
# prior.weights are divided out to keep them applied exactly once.
selection_weights <- function(model) {

  w <- model$prior.weights

  if (is.null(w)) {
    return(NULL)
  }

  resp <- model$model[[1]]

  if (is.matrix(resp) && ncol(resp) == 2L &&
      identical(model$family$family, "binomial")) {
    trials <- rowSums(resp)
    w      <- ifelse(trials > 0, w / trials, 0)
  }

  w
}

# data used to refit variable selection candidates. The stored model frame
# already holds every term evaluated as a column named by its term label (for
# example a `log(wt)` column), so candidates are always refit on it rather than
# re-evaluating the original data. This keeps selection aligned with the fitted
# response, predictors and weights even when the source data object is edited
# or reassigned after fitting, and it also works for predictors built from
# locally defined functions or for models fitted without a `data` argument.
selection_data <- function(model) {
  model$model
}

# fit a variable selection model, including the intercept-only model. When the
# original fitted model is supplied, its weights, offset, family/link and
# contrasts are carried over so selection compares comparable models.
selection_model <- function(response, predictors, data, fit) {

  form <- selection_formula(fit, predictors)

  args <- list(
    formula  = form,
    data     = data,
    family   = fit$family,
    weights  = selection_weights(fit)
  )

  ctr <- selection_contrasts(fit, form)

  if (!is.null(ctr)) {
    args$contrasts <- ctr
  }

  if (!is.null(fit$offset)) {
    args$offset <- fit$offset
  }

  selection_restore(do.call(stats::glm, args), fit, predictors)
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
    weights   = selection_weights(model)
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
