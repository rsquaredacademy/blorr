#' @importFrom stats coef confint binomial df.residual glm terms nobs
#' @importFrom utils data
response_var <- function(model) {
  paste(deparse(model$terms[[2]]), collapse = " ")
}

# Supported models: unweighted 0/1 (Bernoulli) binomial models with logit link.
# One shared guard, called by blr_check_model(), rejects other families/links
# and stops on weighted models (any prior weight other
# than 1, zero weights included) and on grouped binomial models (a matrix
# response such as cbind(successes, failures), or any response value other than
# 0/1). The check uses the stored original response (model frame column 1) as
# well as the initialized response, so it does not depend on which of them
# glm() normalized; a fit stored without its model frame or response is
# declined. Responses with more than two levels are rejected by
# blr_check_binary_response(), see below.
blr_unsupported_message <- function() {
  paste("blorr supports unweighted 0/1 (Bernoulli) models; weighted and",
        "grouped binomial models are not supported.")
}

blr_check_supported <- function(model) {

  fam <- family(model)
  if (!isTRUE(fam$family == "binomial") || !isTRUE(fam$link == "logit")) {
    stop("blorr supports binary logistic regression only (binomial family, ",
         "logit link). This model uses the '", fam$family,
         "' family and the '", fam$link, "' link.", call. = FALSE)
  }

  if (is.null(model$model) || is.null(model$y)) {
    stop("blorr needs the model frame and the response of the fit: refit ",
         "with glm(..., model = TRUE, y = TRUE).", call. = FALSE)
  }

  resp <- model$model[[1]]
  bad  <- is.matrix(resp) || any(!(model$y %in% c(0, 1)))

  if (!bad && is.numeric(resp)) {
    bad <- any(!(resp %in% c(0, 1)))
  }

  w1 <- model$prior.weights
  w2 <- tryCatch(stats::weights(model, "prior"), error = function(e) NULL)

  if (!bad) {
    bad <- is.null(w1) || any(w1 != 1, na.rm = TRUE) || (!is.null(w2) && any(w2 != 1, na.rm = TRUE))
  }

  if (bad) {
    stop(blr_unsupported_message(), call. = FALSE)
  }

  blr_check_binary_response(resp, names(model$model)[1])

  invisible(TRUE)
}

# Responses supported by blorr are binary: exactly two observed levels, one
# event class and one non-event class. glm() silently recodes a factor response
# with more levels as "first level" versus "everything else", so a three-level
# response used to be fitted without complaint and the response profile and its
# printed summary showed only the first two levels of the table (dropping the
# rest). Count the levels the fitted model actually saw (glm() drops unused
# factor levels) and name the response in the error. `resp` may also be a
# response profile table, whose length is its number of outcomes, so
# print_blr_reg() can reuse the check.
#
# A one-level response is rejected for every response type. Numeric 1 and
# factor 1 used to disagree (numeric 1 counted every observation as an event
# while factor 1 counted every observation as a non-event), so no degenerate
# encoding is given a meaning: the data must contain both events and
# non-events.
blr_check_binary_response <- function(resp, name = "response") {

  if (is.null(resp)) {
    return(invisible(TRUE))
  }

  n <- if (is.table(resp)) {
    length(resp)
  } else if (is.factor(resp)) {
    nlevels(resp)
  } else {
    length(unique(resp[!is.na(resp)]))
  }

  if (n < 2L) {
    stop("`", name, "` has ", n, if (n == 1L) " level" else " levels",
         "; blorr needs both events and non-events.", call. = FALSE)
  }

  if (n > 2L) {
    stop("The response `", name, "` has ", n,
         " levels; blorr supports a binary response with exactly two levels.",
         call. = FALSE)
  }

  invisible(TRUE)
}

# Segment and two-way tables need a value for every observation. A missing
# predictor used to reach the width calculation of the printers and fail with
# "invalid 'width' argument"; reject it at construction with the column name.
blr_check_predictor_missing <- function(x, name) {
  n <- sum(is.na(x))

  if (n > 0L) {
    stop("The predictor `", name, "` has ", n,
         " missing value", if (n == 1L) "" else "s",
         "; blorr's segment tables need a value for every observation.",
         call. = FALSE)
  }

  invisible(TRUE)
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

# model degrees of freedom: estimated (non-aliased) coefficients, excluding
# the intercept when the model has one
model_df <- function(model) {
  est <- names(stats::coef(model))[!is.na(stats::coef(model))]
  length(setdiff(est, "(Intercept)"))
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

  resp <- model$model[[1]]

  blr_check_binary_response(resp, response_var(model))

  table(model$model[[1]])

}


# analysis of maximum likelihood estimates
predictor_names <- function(model) {
  names(model$coefficients)
}

# model df: one per estimated coefficient, zero for an aliased coefficient,
# so the vector has one entry per coefficient name
predictor_df <- function(model) {
  unname(ifelse(is.na(model$coefficients), 0, 1))
}

# model estimate
predictor_est <- function(model) {
  unname(model$coefficients)
}

# extract columns from model summary, matched to the coefficients by name.
# summary.glm omits aliased coefficients, so matching by position would shift
# the statistics of every later coefficient; aliased rows are NA instead.
predictor_mine <- function(model, col_name = NULL) {
  # a model with no coefficients has an empty summary table whose columns are
  # named for a t statistic; there is nothing to match
  if (length(model$coefficients) == 0) {
    return(numeric(0))
  }
  sm  <- summary(model)$coefficients
  idx <- match(names(model$coefficients), rownames(sm))
  unname(sm[idx, col_name])
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

# odds ratio effects: every estimated coefficient except the intercept,
# identified by name so a model without an intercept keeps its first predictor
# and an intercept-only model has no effects
odds_index <- function(model) {
  which(names(coef(model)) != "(Intercept)")
}

odds_effect <- function(model) {
  names(coef(model))[odds_index(model)]
}

# odds ratio point estimates
odds_point <- function(model) {
  unname(exp(coef(model))[odds_index(model)])
}

# odds ratio confidence intervals (profile likelihood)
odds_conf_limit <- function(model) {

  idx <- odds_index(model)

  if (length(idx) == 0) {
    out <- data.frame(numeric(0), numeric(0))
  } else {
    ci <- confint(model)
    if (is.null(dim(ci))) {
      ci <- matrix(ci, nrow = 1, dimnames = list(names(coef(model)), names(ci)))
    }
    out <- exp(as.data.frame(ci[idx, , drop = FALSE]))
  }

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

# is the reduced model structurally nested in the full model? This is a
# documented, purely structural rule with exact comparison: the reduced
# model's estimated (non-aliased) design columns must all appear, by name, among
# the full model's estimated columns, and the columns that share a name must
# hold identical values (identical(), no tolerance) on the observations that
# carry information. Both designs
# are taken from fitted_design(). Algebraically equivalent parameterizations
# under different names (for example other factor contrasts, or I(wt) in place
# of wt) are not recognised as nested; the caller stops with an error.
design_nested <- function(full, reduced) {

  if (nrow(full) == 0L || ncol(full) == 0L || ncol(reduced) == 0L ||
      nrow(full) != nrow(reduced)) {
    return(FALSE)
  }

  fn <- colnames(full)
  rn <- colnames(reduced)

  if (is.null(fn) || is.null(rn) || anyDuplicated(fn) || anyDuplicated(rn) ||
      !all(rn %in% fn)) {
    return(FALSE)
  }

  for (nm in rn) {
    if (!identical(as.numeric(full[, nm]), as.numeric(reduced[, nm]))) {
      return(FALSE)
    }
  }

  TRUE
}

# rows of a fitted model that carry information: positive prior weight (zero
# case weights and zero-trial binomial groups contribute nothing to the fit)
fitted_rows <- function(model) {
  w <- model$prior.weights
  if (is.null(w)) {
    rep(TRUE, NROW(stats::model.matrix(model)))
  } else {
    w > 0
  }
}

# the fitted model's estimable design: model matrix columns with a
# non-aliased coefficient, on the rows that carry information
fitted_design <- function(model, rows = fitted_rows(model)) {
  mm <- stats::model.matrix(model)
  mm[rows, !is.na(stats::coef(model)), drop = FALSE]
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
# reproduce exactly the same design matrix, total offset and prior weights from
# the stored model frame. If the check fails, selection stops with an error.
selection_restore <- function(lit, fit, predictors) {

  out <- tryCatch(selection_restore_impl(lit, fit, predictors),
                  error = function(e) NULL)

  # the stored-frame refit cannot be returned as it is: its formula names
  # stored columns and its call holds the training offset and weights, so
  # predict(newdata) would fail or silently reuse training offsets. If the
  # original terms cannot be restored exactly, stop instead.
  if (is.null(out)) {
    stop("variable selection could not rebuild the selected model from the ",
         "terms of the original model (",
         paste(c(names(fit$model)[1], predictors), collapse = ", "),
         "); refit the model with simpler terms.", call. = FALSE)
  }

  # the refit uses the already complete stored rows; carry over the original
  # omission map so na.exclude fits keep their padded residuals and fitted
  # values, as an ordinary glm() fit with the same call would
  if (!is.null(fit$na.action)) {
    out$na.action <- fit$na.action
    attr(out$model, "na.action") <- fit$na.action
  }

  out
}

# are two design matrices equal column by column up to rounding, relative to
# each column's own scale? Restoration re-evaluates the stored variables, so
# exact identity is not required, but any real difference fails.
design_equal <- function(a, b) {
  a <- unname(a)
  b <- unname(b)
  if (any(is.na(a) != is.na(b))) {
    return(FALSE)
  }
  a[is.na(a)] <- 0
  b[is.na(b)] <- 0
  scale <- pmax(apply(abs(a), 2, max), apply(abs(b), 2, max))
  scale[scale == 0] <- 1
  all(apply(abs(a - b), 2, max) <= 64 * .Machine$double.eps * scale)
}

selection_restore_impl <- function(lit, fit, predictors) {

  tt       <- stats::terms(fit)
  env      <- environment(tt)
  vars_all <- as.list(attr(tt, "variables"))[-1]
  mf_names <- names(fit$model)
  resp     <- vars_all[[attr(tt, "response")]]

  # rebuild each selected term from the original variables' language objects
  # and the factor incidence of the fitted terms, never by re-parsing a term
  # label (labels are display strings and can round numeric constants)
  fac  <- attr(tt, "factors")
  labs <- attr(tt, "term.labels")
  rhs_parts <- lapply(predictors, function(p) {
    j <- match(p, labs)
    if (is.na(j)) {
      stop("unknown term", call. = FALSE)
    }
    Reduce(function(a, b) call(":", a, b), vars_all[fac[, j] > 0])
  })
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

  # model frame: original variable columns (including any formula offsets)
  # plus the original fit's own extra columns such as (weights) and an
  # argument-supplied (offset). The refit's (offset) column holds the total
  # offset, so copying it would count formula offsets twice.
  fr     <- fit$model[idx]
  extras <- names(fit$model)[startsWith(names(fit$model), "(")]
  for (nm in extras) {
    fr[[nm]] <- fit$model[[nm]]
  }
  attr(fr, "terms")     <- new_tt
  attr(fr, "na.action") <- attr(fit$model, "na.action")

  # the restored frame must imply the same total offset and prior weights as
  # the refit, since profiling and refitting read them from the frame
  n       <- NROW(lit$y)
  off_new <- stats::model.offset(fr)
  off_lit <- lit$offset
  if (is.null(off_new)) off_new <- rep(0, n)
  if (is.null(off_lit)) off_lit <- rep(0, n)
  if (!isTRUE(all.equal(as.numeric(off_new), as.numeric(off_lit),
                        tolerance = 0))) {
    return(NULL)
  }

  # compare the prior weights glm() would derive from the restored frame (case
  # weights times binomial trials for a grouped response) with the refit's
  # prior weights. Comparing effective weights keeps zero-trial groups, whose
  # case weight cannot be recovered from a zero product, consistent.
  w_new  <- stats::model.weights(fr)
  if (is.null(w_new)) w_new <- rep(1, n)
  resp_n <- fr[[1]]
  if (is.matrix(resp_n) && ncol(resp_n) == 2L &&
      identical(fit$family$family, "binomial")) {
    w_new <- w_new * rowSums(resp_n)
  }
  if (!isTRUE(all.equal(as.numeric(w_new), as.numeric(lit$prior.weights)))) {
    return(NULL)
  }

  x_new <- stats::model.matrix(new_tt, fr, contrasts.arg = lit$contrasts)
  x_lit <- stats::model.matrix(lit)

  if (!identical(dim(x_new), dim(x_lit)) || !design_equal(x_new, x_lit)) {
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

# Pick exactly one candidate from a tied set of p values. On large data the
# p values can underflow to 0 or 1, so several candidates can tie; the Wald
# chi-square statistic from the same candidate fits breaks the tie, largest for
# entry and smallest for removal. Any remaining tie keeps candidate order.
selection_p_index <- function(pvals, chisq, enter = TRUE) {

  if (enter) {
    candidates <- which(pvals == min(pvals))
    candidates[which.max(chisq[candidates])]
  } else {
    candidates <- which(pvals == max(pvals))
    candidates[which.min(chisq[candidates])]
  }
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
