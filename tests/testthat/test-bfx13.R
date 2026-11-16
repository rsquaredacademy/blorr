logit_only_message <- function(model) {
  paste0("blorr supports binary logistic regression only (binomial family, ",
         "logit link). This model uses the '", family(model)$family,
         "' family and the '", family(model)$link, "' link.")
}

# Enumerate exports independently of the guard implementation.
bfx13_entry_points <- function() {
  exports <- getNamespaceExports("blorr")
  sort(exports[vapply(exports, function(nm) {
    fun <- getExportedValue("blorr", nm)
    is.function(fun) && length(formals(fun)) > 0L &&
      names(formals(fun))[1L] %in% c("model", "full_model", "object")
  }, logical(1))])
}

test_that("the audited non-logit R2 and odds cases reject (BFXX12-1)", {
  for (link in c("probit", "cloglog")) {
    m <- glm(honcomp ~ female + read + science, hsb2,
             family = binomial(link = link))
    expected <- logit_only_message(m)
    expect_error(blr_rsq_mckelvey_zavoina(m), expected, fixed = TRUE)
    expect_error(blr_model_fit_stats(m), expected, fixed = TRUE)
    expect_error(blr_multi_model_fit_stats(m, m), expected, fixed = TRUE)
    expect_error(blr_regress(m, odd_conf_limit = TRUE), expected, fixed = TRUE)
  }
})

test_that("every public GLM entry point rejects other links and families", {
  eps <- bfx13_entry_points()
  expect_length(eps, 51L)
  families <- list(binomial("probit"), binomial("cloglog"), binomial("cauchit"),
                   quasibinomial(), gaussian(), poisson())
  d <- hsb2
  d$honcomp <- as.numeric(as.character(d$honcomp))
  for (fam in families) {
    m <- glm(honcomp ~ female + read + science, d, family = fam)
    expected <- logit_only_message(m)
    for (ep in eps) {
      fun <- getExportedValue("blorr", ep)
      actual <- tryCatch({ fun(m); NA_character_ },
                         error = function(e) conditionMessage(e))
      expect_identical(actual, expected,
                       info = paste(ep, fam$family, fam$link))
    }
    ok <- glm(honcomp ~ female + read + science, hsb2, family = binomial())
    expect_error(blr_test_lr(ok, m), expected, fixed = TRUE)
    expect_error(blr_test_lr(m, ok), expected, fixed = TRUE)
    expect_error(blr_multi_model_fit_stats(ok, m), expected, fixed = TRUE)
    expect_error(blr_multi_model_fit_stats(ok, ok, m), expected, fixed = TRUE)
  }
})

test_that("selection rejects non-logit fits before any refit", {
  eps <- grep("^blr_step_", bfx13_entry_points(), value = TRUE)
  expect_length(eps, 6L)
  local_mocked_bindings(selection_data = function(...) {
    stop("unexpected refit preparation")
  }, .package = "blorr")
  for (link in c("probit", "cloglog", "cauchit")) {
    m <- glm(honcomp ~ female + read + science, hsb2,
             family = binomial(link = link))
    for (ep in eps) {
      expect_error(getExportedValue("blorr", ep)(m), logit_only_message(m),
                   fixed = TRUE)
    }
  }
})

test_that("ordinary logit R2 and odds retain their GLM values", {
  m <- glm(honcomp ~ female + read + science, hsb2, family = binomial())
  eta <- m$linear.predictors
  v <- mean((eta - mean(eta))^2)
  expect_equal(blr_rsq_mckelvey_zavoina(m), v / (v + pi^2 / 3))
  expect_equal(blr_model_fit_stats(m)$mckelvey, v / (v + pi^2 / 3))
  r <- blr_regress(m, odd_conf_limit = TRUE)
  expect_equal(r$blr_odds_point, unname(exp(coef(m)[-1L])))
  for (ep in bfx13_entry_points()) {
    expect_no_error(capture.output(getExportedValue("blorr", ep)(m)))
  }
})
