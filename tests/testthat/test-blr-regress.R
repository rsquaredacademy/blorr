test_that("output from blr_regress is as expected", {
  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  k <- blr_regress(model, odd_conf_limit = TRUE)

  expect_snapshot(k)
})

test_that("odds ratio confidence limits are labelled as profile-likelihood intervals", {
  model <- glm(
    honcomp ~ female + read, data = hsb2,
    family = binomial(link = "logit")
  )

  k   <- blr_regress(model, odd_conf_limit = TRUE)
  out <- capture.output(print(k))

  expect_true(any(grepl("95% Profile", out)))
  expect_false(any(grepl("95% Wald", out)))
  expect_equal(
    unname(as.matrix(k$blr_conf)),
    unname(suppressMessages(exp(confint(model)))[-1, ])
  )
})

test_that("blr_regress response profile keeps factor response labels", {
  d <- hsb2
  d$hc_lab <- factor(ifelse(d$honcomp == 1, "yes", "no"), levels = c("no", "yes"))

  k <- blr_regress(hc_lab ~ female + read, data = d)

  expect_equal(names(k$resp_prof), c("no", "yes"))
  expect_equal(as.integer(k$resp_prof), c(147L, 53L))
})

test_that("blr_regress prints factor response labels in the response summary", {
  d <- hsb2
  d$hc_lab <- factor(ifelse(d$honcomp == 1, "yes", "no"), levels = c("no", "yes"))

  out <- capture.output(blr_regress(hc_lab ~ female + read, data = d))
  start <- grep("Response Summary", out)
  end <- grep("Maximum Likelihood Estimates", out)
  summary_lines <- out[start:end]

  expect_true(any(grepl("no", summary_lines)))
  expect_true(any(grepl("yes", summary_lines)))
})

test_that("blr_regress reports nobs and response profile on data with missing values", {
  d <- hsb2
  d$read[which(d$honcomp == "0")[1:10]] <- NA

  model <- glm(
    honcomp ~ female + read, data = d,
    family = binomial(link = "logit")
  )

  k <- blr_regress(honcomp ~ female + read, data = d)

  expect_equal(k$n_obs, unname(nobs(model)))
  expect_equal(as.integer(k$resp_prof[[1]]), sum(model$y == 0))
  expect_equal(as.integer(k$resp_prof[[2]]), sum(model$y == 1))
})

test_that("blr_regress can summarise a fit with a transformed response", {
  model <- glm(I(am == 1) ~ wt, data = mtcars, family = binomial)

  k <- blr_regress(model)

  expect_equal(k$est, unname(coef(model)))
  expect_equal(k$n_obs, unname(nobs(model)))
})

test_that("aliased coefficients do not shift summary statistics (bfx8)", {
  m  <- glm(am ~ I(0 * wt) + wt + hp, mtcars, family = binomial)
  r  <- blr_regress(m)
  sm <- summary(m)$coefficients

  expect_equal(r$parameter, c("(Intercept)", "I(0 * wt)", "wt", "hp"))
  expect_equal(r$df, c(1, 0, 1, 1))
  expect_true(all(is.na(c(r$est[2], r$se[2], r$zval[2], r$pval[2]))))
  for (nm in rownames(sm)) {
    i <- match(nm, r$parameter)
    expect_equal(r$est[i], sm[nm, "Estimate"])
    expect_equal(r$se[i], sm[nm, "Std. Error"])
    expect_equal(r$zval[i], sm[nm, "z value"])
    expect_equal(r$pval[i], sm[nm, "Pr(>|z|)"])
  }
  expect_equal(r$se[3], 3.068675113054696, tolerance = 1e-8)
  expect_equal(r$zval[3], -2.634190614723622, tolerance = 1e-8)
  expect_equal(r$pval[4], .040914646459029938, tolerance = 1e-8)
  expect_output(print(r), "wt")

  # aliased models print with odds limits, with and without an intercept
  for (f in list(am ~ wt + I(2 * wt), am ~ 0 + wt + I(2 * wt))) {
    ma <- glm(f, mtcars, family = binomial)
    ra <- blr_regress(ma, odd_conf_limit = TRUE)
    expect_output(print(ra), "Odds Ratio Estimates")
    expect_equal(ra$blr_odds_point, unname(exp(coef(ma))[ra$blr_effects]))
  }
})

test_that("overview Df. Model is the estimated model df excluding the intercept (bfx9)", {
  m1 <- glm(am ~ wt + hp, mtcars, family = binomial)
  m2 <- glm(am ~ 0 + wt + hp, mtcars, family = binomial)
  m3 <- glm(am ~ 0 + wt + I(2 * wt), mtcars, family = binomial)
  m4 <- glm(am ~ 1, mtcars, family = binomial)

  expect_equal(blr_regress(m1)$mod_df, 2)
  expect_equal(blr_regress(m1)$resid_df, 29)
  expect_equal(blr_regress(m2)$mod_df, 2)
  expect_equal(blr_regress(m3)$mod_df, 1)
  expect_equal(blr_regress(m4)$mod_df, 0)
})

test_that("blr_regress keeps the offset of the fitted model and rejects weighted fits", {
  set.seed(42)
  w <- sample(1:4, nrow(hsb2), replace = TRUE)
  m_w <- glm(honcomp ~ female + read + science, data = hsb2, family = binomial(link = "logit"), weights = w)
  expect_error(blr_regress(m_w), "unweighted 0/1")

  # offset model
  off <- rnorm(nrow(hsb2))
  m_off <- glm(honcomp ~ female + read + science, data = hsb2, family = binomial(link = "logit"), offset = off)
  res_off <- blr_regress(m_off)
  s_off <- summary(m_off)$coefficients

  expect_equal(res_off$est, unname(s_off[, "Estimate"]), tolerance = 1e-6)
  expect_equal(res_off$se, unname(s_off[, "Std. Error"]), tolerance = 1e-6)
  expect_equal(res_off$zval, unname(s_off[, "z value"]), tolerance = 1e-6)
  expect_equal(res_off$pval, unname(s_off[, "Pr(>|z|)"]), tolerance = 1e-6)
})

test_that("blr_regress estimates match glm for formula and argument offsets", {
  set.seed(4)
  off <- rnorm(nrow(hsb2))

  m_form <- glm(honcomp ~ female + read + science + offset(off),
                data = hsb2, family = binomial(link = "logit"))
  m_arg  <- glm(honcomp ~ female + read + science, data = hsb2,
                family = binomial(link = "logit"), offset = off)
  m_both <- glm(honcomp ~ female + read + science + offset(off),
                data = hsb2, family = binomial(link = "logit"), offset = off)

  expect_equal(blr_regress(m_form)$est, unname(coef(m_form)), tolerance = 1e-8)
  expect_equal(blr_regress(m_arg)$est, unname(coef(m_arg)), tolerance = 1e-8)
  expect_equal(blr_regress(m_both)$est, unname(coef(m_both)), tolerance = 1e-8)

  # weights with a formula offset: rejected by the shared guard
  set.seed(3)
  w <- sample(1:4, nrow(hsb2), replace = TRUE)
  m_wform <- glm(honcomp ~ female + read + science + offset(off),
                 data = hsb2, family = binomial(link = "logit"), weights = w)
  expect_error(blr_regress(m_wform), "unweighted 0/1")
})

test_that("blr_regress uses the model offset when the data has columns with those names", {
  set.seed(6)
  off <- rnorm(200)
  dd  <- hsb2
  dd$weights <- runif(200, 1, 3)
  dd$offset  <- rnorm(200)

  mc <- suppressWarnings(glm(
    honcomp ~ female + read + weights + offset,
    data = dd, family = binomial, offset = off
  ))

  res <- suppressWarnings(blr_regress(mc))

  expect_equal(res$est, unname(coef(mc)), tolerance = 1e-8)
  expect_equal(
    res$se,
    unname(summary(mc)$coefficients[, 2]),
    tolerance = 1e-8
  )
})

test_that("blr_regress.default uses the supplied offset and rejects weights", {
  set.seed(7)
  w   <- sample(1:4, 200, replace = TRUE)
  off <- rnorm(200)
  dd  <- hsb2
  dd$weights <- rnorm(200)
  dd$offset  <- rnorm(200)

  mc <- suppressWarnings(glm(
    honcomp ~ female + read,
    data = dd, family = binomial, offset = off
  ))

  res <- suppressWarnings(blr_regress(
    honcomp ~ female + read,
    data = dd, offset = off
  ))

  expect_equal(res$est, unname(coef(mc)), tolerance = 1e-8)
  expect_equal(
    res$se,
    unname(summary(mc)$coefficients[, 2]),
    tolerance = 1e-8
  )

  # a weighted formula fit is rejected by the same guard as a fitted model
  expect_error(
    blr_regress(honcomp ~ female + read, data = dd, weights = w),
    "unweighted 0/1"
  )
})

test_that("blr_regress keeps the formula environment", {
  f <- local({
    zz <- rnorm(nrow(hsb2))
    honcomp ~ female + read + science + zz
  })

  m <- glm(f, data = hsb2, family = binomial(link = "logit"))

  expect_equal(blr_regress(f, data = hsb2)$est, unname(coef(m)),
               tolerance = 1e-8)
  expect_equal(blr_regress(m)$est, unname(coef(m)), tolerance = 1e-8)
})

test_that("blr_regress errors on non-logit models", {
  m_prob <- glm(honcomp ~ female + read + science, data = hsb2, family = binomial(link = "probit"))
  expect_error(blr_regress(m_prob), "logit")
})

test_that("blr_check_model errors on gaussian glm", {
  m_gauss <- glm(read ~ female + science, data = hsb2, family = gaussian())
  expect_error(blr_check_model(m_gauss), "binomial")
})
