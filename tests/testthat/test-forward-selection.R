# term labels of a candidate model are taken from the stored model frame, so a
# transformed term such as log(wt) is stored as a quoted column name and the
# label gains backticks; strip them before comparing with the source formula
sel_terms <- function(model) {
  gsub("`", "", attr(stats::terms(model), "term.labels"), fixed = TRUE)
}

test_that("output from forward variable selection is as expected", {
  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  actual   <- blr_step_aic_forward(model)$predictors 
  expected <- c("read", "female", "science")

  expect_equal(actual, expected)
})

test_that("output from forward variable p selection is as expected", {
  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  actual   <- blr_step_p_forward(model)$predictors
  expected <- c("read", "female", "science")

  expect_equal(actual, expected)
})

test_that("forward p selection keeps weights and prints the returned model", {
  set.seed(1)
  d <- mtcars
  d$w <- rep(c(1, 2), 16)
  d$noise <- rnorm(32)
  m <- glm(am ~ wt + hp + noise, d, family = binomial, weights = w)

  out <- capture.output(k <- blr_step_p_forward(m, penter = 0.03))

  expected <- glm(am ~ wt + hp, data = d, family = binomial, weights = w)

  expect_equal(k$predictors, c("wt", "hp"))
  expect_equal(unname(coef(k$model)), unname(coef(expected)))
  for (b in coef(k$model)) {
    expect_true(any(grepl(sprintf("%.4f", b), out, fixed = TRUE)))
  }
})

test_that("backward p elimination prints the weighted model it returns", {
  set.seed(1)
  d <- mtcars
  d$w <- rep(c(1, 2), 16)
  d$noise <- rnorm(32)
  m <- glm(am ~ wt + hp + noise, d, family = binomial, weights = w)

  out <- capture.output(k <- blr_step_p_backward(m))

  expected <- glm(reformulate(setdiff(k$indvar, k$removed), response = "am"),
                  data = d, family = binomial, weights = w)
  expect_equal(unname(coef(k$model)), unname(coef(expected)))
  for (b in coef(k$model)) {
    expect_true(any(grepl(sprintf("%.4f", b), out, fixed = TRUE)))
  }
})

test_that("stepwise p selection prints the weighted model it returns", {
  set.seed(1)
  d <- mtcars
  d$w <- rep(c(1, 2), 16)
  d$noise <- rnorm(32)
  m <- glm(am ~ wt + hp + noise, d, family = binomial, weights = w)

  out <- capture.output(k <- blr_step_p_both(m))

  expected <- glm(reformulate(k$predictors, response = "am"), data = d,
                  family = binomial, weights = w)
  expect_equal(unname(coef(k$model)), unname(coef(expected)))
  for (b in coef(k$model)) {
    expect_true(any(grepl(sprintf("%.4f", b), out, fixed = TRUE)))
  }
})

test_that("p-value selection reevaluates transformed predictors on fitted rows", {
  set.seed(1)
  d <- mtcars
  d$noise <- rnorm(32)
  m <- glm(am ~ log(wt) + hp + noise, data = d, family = binomial)

  expected <- glm(am ~ log(wt) + hp, data = d, family = binomial)

  out <- capture.output(fwd <- blr_step_p_forward(m))
  expect_equal(sel_terms(fwd$model), c("log(wt)", "hp"))
  expect_equal(unname(coef(fwd$model)), unname(coef(expected)))
  expect_equal(fwd$predictors, c("log(wt)", "hp"))

  out <- capture.output(bwd <- blr_step_p_backward(m))
  expect_equal(sel_terms(bwd$model), c("log(wt)", "hp"))
  expect_equal(unname(coef(bwd$model)), unname(coef(expected)))

  out <- capture.output(both <- blr_step_p_both(m))
  expect_equal(sel_terms(both$model), c("log(wt)", "hp"))
  expect_equal(unname(coef(both$model)), unname(coef(expected)))
  expect_equal(both$predictors, c("log(wt)", "hp"))
})

test_that("AIC selection procedures print the weighted model they return", {
  set.seed(1)
  d <- mtcars
  d$w <- rep(c(1, 2), 16)
  d$noise <- rnorm(32)
  m <- glm(am ~ wt + hp + noise, d, family = binomial, weights = w)

  runs <- list(
    function() blr_step_aic_forward(m, progress = TRUE),
    function() blr_step_aic_backward(m, progress = TRUE),
    function() blr_step_aic_both(m, details = TRUE)
  )

  for (run in runs) {
    out <- capture.output(k <- run())
    expected <- glm(formula(k$model), data = d, family = binomial,
                    weights = w)
    expect_equal(unname(coef(k$model)), unname(coef(expected)))
    for (b in coef(k$model)) {
      expect_true(any(grepl(sprintf("%.4f", b), out, fixed = TRUE)))
    }
  }
})

test_that("all six procedures use the stored fit after source-data mutation", {
  set.seed(1)
  d <- mtcars
  d$noise <- rnorm(32)
  m <- glm(am ~ wt + hp + noise, d, family = binomial)
  original  <- d
  reference <- glm(am ~ wt + hp, original, family = binomial)

  procedures <- list(
    p_forward    = function(x) { capture.output(k <- blr_step_p_forward(x)); k },
    p_backward   = function(x) { capture.output(k <- blr_step_p_backward(x)); k },
    p_both       = function(x) { capture.output(k <- blr_step_p_both(x)); k },
    aic_forward  = function(x) { capture.output(k <- blr_step_aic_forward(x)); k },
    aic_backward = function(x) { capture.output(k <- blr_step_aic_backward(x)); k },
    aic_both     = function(x) { capture.output(k <- blr_step_aic_both(x)); k }
  )

  mutations <- list(
    doubled  = function(d) { d$wt <- 2 * d$wt; d },
    reversed = function(d) { d$wt <- rev(d$wt); d }
  )

  for (mutate in mutations) {
    d <- mutate(original)

    for (name in names(procedures)) {
      k <- procedures[[name]](m)

      expect_equal(unname(coef(k$model)), unname(coef(reference)))
      expect_equal(sort(sel_terms(k$model)), c("hp", "wt"))
      expect_equal(predict(k$model, original, type = "response"),
                   reference$fitted.values)
    }
  }
})

test_that("selection evaluates locally defined transformations from the model frame", {
  makefit <- function() {
    log2wt <- function(x) log(x)
    d <- mtcars
    set.seed(1)
    d$noise <- rnorm(32)
    list(
      model     = glm(am ~ log2wt(wt) + hp + noise, d, family = binomial),
      reference = glm(am ~ log2wt(wt) + hp, d, family = binomial)
    )
  }
  fit <- makefit()

  out <- capture.output(k <- blr_step_p_forward(fit$model))

  expect_equal(sel_terms(k$model), c("log2wt(wt)", "hp"))
  expect_equal(unname(coef(k$model)), unname(coef(fit$reference)))
})

test_that("selection works for a no-data fit with locally scoped variables", {
  makefit <- function() {
    set.seed(1)
    y_local     <- mtcars$am
    x_local     <- mtcars$wt
    hp_local    <- mtcars$hp
    noise_local <- rnorm(32)
    list(
      model = glm(y_local ~ log(x_local) + hp_local + noise_local,
                  family = binomial),
      reference = glm(y_local ~ log(x_local) + hp_local, family = binomial)
    )
  }
  fit <- makefit()

  out <- capture.output(k <- blr_step_p_forward(fit$model))

  expect_equal(sel_terms(k$model), c("log(x_local)", "hp_local"))
  expect_equal(unname(coef(k$model)), unname(coef(fit$reference)))
})


selection_procedures <- list(
  p_forward    = function(x) { capture.output(k <- blr_step_p_forward(x)); k },
  p_backward   = function(x) { capture.output(k <- blr_step_p_backward(x)); k },
  p_both       = function(x) { capture.output(k <- blr_step_p_both(x)); k },
  aic_forward  = function(x) { capture.output(k <- blr_step_aic_forward(x)); k },
  aic_backward = function(x) { capture.output(k <- blr_step_aic_backward(x)); k },
  aic_both     = function(x) { capture.output(k <- blr_step_aic_both(x)); k }
)

# the returned model must equal an independent glm on its own formula: same
# coefficients and names, likelihood, and predictions on raw new data
expect_matches_reference <- function(model, data, newdata) {
  ref <- glm(formula(model), data = data, family = family(model))
  expect_equal(coef(model), coef(ref))
  expect_equal(as.numeric(logLik(model)), as.numeric(logLik(ref)))
  expect_equal(
    predict(model, newdata, type = "response"),
    predict(ref, newdata, type = "response")
  )
}

test_that("selected transformed models predict from raw new data (bfx5)", {
  set.seed(1)
  d <- transform(mtcars, noise = rnorm(32))
  m <- glm(am ~ log(wt) + hp + noise, d, family = binomial)
  expected <- glm(am ~ log(wt) + hp, d, family = binomial)

  for (run in selection_procedures) {
    k <- run(m)
    expect_equal(attr(terms(k$model), "term.labels"), c("log(wt)", "hp"))
    expect_equal(unname(coef(k$model)),
                 c(19.76650039746535, -22.66901763616302, 0.0331005632650925),
                 tolerance = 1e-8)
    expect_equal(coef(k$model), coef(expected))
    expect_equal(predict(k$model, mtcars, type = "response"),
                 predict(expected, mtcars, type = "response"))
  }

  m2 <- glm(am ~ I(wt^2) + hp, d, family = binomial)
  capture.output(k2 <- blr_step_aic_backward(m2))
  expect_matches_reference(k2$model, d, mtcars)
})

test_that("selection keeps interaction terms grouped (bfx5)", {
  set.seed(103)
  d <- data.frame(x = rnorm(300), z = rnorm(300), noise = rnorm(300))
  d$y <- rbinom(300, 1, plogis(.2 + 1.2 * d$x + .8 * d$z + .6 * d$x * d$z))
  m <- glm(y ~ x * z + noise, d, family = binomial)

  for (nm in names(selection_procedures)) {
    k <- selection_procedures[[nm]](m)
    expect_matches_reference(k$model, d, d[1:25, ])
  }

  capture.output(k <- blr_step_p_forward(m))
  expect_equal(attr(terms(k$model), "term.labels"), c("x", "z", "noise", "x:z"))
  expect_equal(as.numeric(logLik(k$model)), -155.495848540222, tolerance = 1e-8)

  set.seed(5)
  d5 <- data.frame(x = rnorm(200), g = factor(sample(letters[1:3], 200, TRUE)),
                   w = rnorm(200), noise = rnorm(200))
  d5$y <- rbinom(200, 1, plogis(d5$x + (d5$g == "b") - d5$w^2 / 2))
  m5 <- glm(y ~ g * x + poly(w, 2) + noise, d5, family = binomial)
  for (run in selection_procedures) {
    expect_matches_reference(run(m5)$model, d5, d5[1:15, ])
  }
})

test_that("selection works with non-syntactic predictor names (bfx5)", {
  set.seed(1)
  d <- transform(mtcars, noise = rnorm(32))
  names(d)[names(d) == "wt"] <- "body weight"
  m <- glm(am ~ `body weight` + hp + noise, d, family = binomial)
  expected <- glm(am ~ `body weight` + hp, d, family = binomial)

  for (run in selection_procedures) {
    k <- run(m)
    expect_equal(coef(k$model), coef(expected))
    expect_equal(predict(k$model, d, type = "response"),
                 predict(expected, d, type = "response"))
  }
})

test_that("selected locally transformed models predict from raw data (bfx5)", {
  set.seed(1)
  d  <- transform(mtcars, noise = rnorm(32))
  lt <- function(z) 2 * log(z)
  m  <- glm(am ~ lt(wt) + hp + noise, d, family = binomial)
  expected <- glm(am ~ lt(wt) + hp, d, family = binomial)

  for (run in selection_procedures) {
    k <- run(m)
    expect_equal(coef(k$model), coef(expected))
    expect_equal(predict(k$model, mtcars, type = "response"),
                 predict(expected, mtcars, type = "response"))
  }
})

test_that("selected models keep offsets once for profile intervals (bfx6)", {
  set.seed(6)
  d <- data.frame(x = rnorm(200), z = rnorm(200), noise = rnorm(200),
                  off = rnorm(200, .5, .5))
  d$y <- rbinom(200, 1, plogis(.2 + 2 * d$x + d$off))
  d$off2 <- runif(200)

  fits <- list(
    formula = list(f = y ~ x + z + noise + offset(off), off = NULL),
    argument = list(f = y ~ x + z + noise, off = d$off),
    both = list(f = y ~ x + z + noise + offset(off), off = d$off2)
  )

  for (cs in fits) {
    m <- glm(cs$f, d, family = binomial, offset = cs$off)
    for (run in selection_procedures) {
      k   <- run(m)
      ref <- glm(formula(k$model), d, family = binomial, offset = cs$off)
      expect_equal(coef(k$model), coef(ref))
      expect_equal(as.numeric(stats::model.offset(k$model$model)),
                   as.numeric(k$model$offset))
      expect_equal(suppressMessages(confint(k$model)),
                   suppressMessages(confint(ref)))
      capture.output(r1 <- blr_regress(k$model, odd_conf_limit = TRUE))
      capture.output(r2 <- blr_regress(ref, odd_conf_limit = TRUE))
      expect_equal(r1$blr_conf, r2$blr_conf)
    }
  }

  # the audit case: x profile interval of the AIC forward selection
  m <- glm(y ~ x + z + noise + offset(off), d, family = binomial)
  capture.output(k <- blr_step_aic_forward(m))
  expect_equal(unname(suppressMessages(confint(k$model))["x", ]),
               c(1.338478644906690, 2.377497411266178), tolerance = 1e-6)

  # intercept-only selection with a formula offset
  set.seed(11)
  dn <- data.frame(y = rbinom(100, 1, .4), a = rnorm(100), b = rnorm(100),
                   o = rnorm(100))
  mn <- glm(y ~ a + b + offset(o), dn, family = binomial)
  capture.output(k <- blr_step_aic_forward(mn))
  ref <- glm(y ~ 1 + offset(o), dn, family = binomial)
  expect_equal(suppressMessages(confint(k$model)),
               suppressMessages(confint(ref)))
})

test_that("selected na.exclude models keep their row padding (bfx6)", {
  set.seed(1)
  d <- transform(mtcars, noise = rnorm(32), w = rep(c(1, 2), 16))
  d$hp[c(2, 9, 30)] <- NA
  m   <- glm(am ~ log(wt) + hp + noise, d, family = binomial,
             na.action = na.exclude)
  ref <- glm(am ~ log(wt) + hp, d, family = binomial, na.action = na.exclude)

  for (run in selection_procedures) {
    k <- run(m)
    expect_equal(coef(k$model), coef(ref))
    expect_equal(predict(k$model), predict(ref))
    expect_equal(residuals(k$model), residuals(ref))
    expect_equal(fitted(k$model), fitted(ref))
    expect_equal(unname(which(is.na(predict(k$model)))), c(2L, 9L, 30L))
  }

  # weighted fit with a matrix-valued term
  set.seed(7)
  dw <- data.frame(x = rnorm(150), z = rnorm(150), noise = rnorm(150),
                   w = sample(1:3, 150, TRUE))
  dw$y <- rbinom(150, 1, plogis(.3 + dw$x - .6 * dw$z^2))
  dw$x[c(4, 40, 99)] <- NA
  mw <- glm(y ~ x + poly(z, 2) + noise, dw, family = binomial, weights = w,
            na.action = na.exclude)
  expect_true(mw$converged)
  for (run in selection_procedures) {
    k    <- run(mw)
    refw <- glm(formula(k$model), dw[!is.na(dw$x), ], family = binomial,
                weights = w, na.action = na.exclude)
    expect_true("x" %in% attr(terms(k$model), "term.labels"))
    expect_equal(as.numeric(logLik(k$model)), as.numeric(logLik(refw)))
    expect_equal(length(residuals(k$model)), 150L)
    expect_equal(unname(which(is.na(residuals(k$model)))), c(4L, 40L, 99L))
    expect_equal(names(predict(k$model)), rownames(dw))
    expect_equal(predict(k$model)[!is.na(dw$x)], predict(refw),
                 ignore_attr = TRUE)
  }
})

test_that("intercept-only selections report empty odds ratio tables (bfx7)", {
  m <- glm(am ~ 1, mtcars, family = binomial)
  r <- blr_regress(m, odd_conf_limit = TRUE)
  expect_equal(r$blr_effects, character(0))
  expect_equal(nrow(r$blr_conf), 0L)
  expect_equal(ncol(r$blr_conf), 2L)
  expect_equal(r$est, unname(coef(m)))
  out <- capture.output(print(r))
  expect_true(any(grepl("Odds Ratio", out)))

  # intercept-only result of a selection procedure
  mn <- glm(y ~ a + b + c, noise_data(), family = binomial)
  for (run in selection_procedures) {
    k <- run(mn)
    expect_equal(attr(terms(k$model), "term.labels"), character(0))
    rk <- blr_regress(k$model, odd_conf_limit = TRUE)
    expect_equal(nrow(rk$blr_conf), 0L)
    expect_equal(rk$est, unname(coef(k$model)))
  }

  # a model without an intercept keeps every predictor's odds ratio
  m0 <- glm(am ~ 0 + wt + hp, mtcars, family = binomial)
  r0 <- blr_regress(m0, odd_conf_limit = TRUE)
  expect_equal(r0$blr_effects, c("wt", "hp"))
  expect_equal(r0$blr_odds_point, unname(exp(coef(m0))))
  expect_equal(unname(as.matrix(r0$blr_conf)),
               unname(exp(suppressMessages(confint(m0)))))
})

test_that("selection keeps high-precision constants in terms (bfx8)", {
  set.seed(1)
  d <- transform(mtcars, noise = rnorm(32))
  # reference formulas are written out: update() would deparse and round
  forms <- list(
    am ~ I(wt / 3.141592653589793) + hp + noise,
    am ~ I(wt^0.3333333333333333) + hp + noise,
    am ~ I(wt + 0.12345678901234567) + hp + noise
  )
  refs <- list(
    am ~ I(wt / 3.141592653589793) + hp,
    am ~ I(wt^0.3333333333333333) + hp,
    am ~ I(wt + 0.12345678901234567) + hp
  )

  for (i in seq_along(forms)) {
    m   <- glm(forms[[i]], d, family = binomial)
    ref <- glm(refs[[i]], d, family = binomial)
    for (run in selection_procedures) {
      k <- run(m)
      expect_identical(attr(terms(k$model), "variables"),
                       attr(terms(ref), "variables"))
      expect_equal(coef(k$model), coef(ref))
      expect_equal(as.numeric(logLik(k$model)), as.numeric(logLik(ref)))
      expect_equal(predict(k$model, mtcars, type = "response"),
                   predict(ref, mtcars, type = "response"))
    }
  }

  m <- glm(forms[[1]], d, family = binomial)
  capture.output(k <- blr_step_aic_forward(m))
  expect_equal(unname(coef(k$model)),
               c(18.86629871720412, -25.39498624864346, .03625559608221653),
               tolerance = 1e-8)
  # mtcars is nearly separated, so profiling warns about fitted 0/1 values
  expect_equal(suppressWarnings(suppressMessages(confint(k$model))),
               suppressWarnings(suppressMessages(confint(
                 glm(am ~ I(wt / 3.141592653589793) + hp, d,
                     family = binomial)))))
})

test_that("selection stops when a model cannot be restored exactly (bfx8)", {
  m   <- glm(am ~ wt + hp, mtcars, family = binomial)
  lit <- selection_model("am", "wt", m$model, fit = m)
  expect_error(selection_restore(lit, m, "not_a_term"),
               "could not rebuild the selected model")
  expect_null(selection_restore_impl(lit, m, "hp"))
})

test_that("p-both summary labels the deviance column as Deviance (bfx9)", {
  set.seed(1)
  d <- transform(mtcars, noise = rnorm(32))
  m <- glm(am ~ wt + hp + noise, d, family = binomial)
  invisible(capture.output(k <- blr_step_p_both(m)))
  out <- capture.output(print(k))
  expect_true(any(grepl("Deviance", out)))
  expect_false(any(grepl("C(p)", out, fixed = TRUE)))
  expect_equal(k$dev[1], deviance(glm(am ~ wt, d, family = binomial)),
               tolerance = 1e-6)
})
