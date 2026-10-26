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

test_that("blr_regress keeps weights and offset of the fitted model", {
  set.seed(42)
  w <- runif(nrow(hsb2), 0.5, 2)
  m_w <- glm(honcomp ~ female + read + science, data = hsb2, family = binomial(link = "logit"), weights = w)
  res_w <- blr_regress(m_w)
  s_w <- summary(m_w)$coefficients

  expect_equal(res_w$est, unname(s_w[, "Estimate"]), tolerance = 1e-6)
  expect_equal(res_w$se, unname(s_w[, "Std. Error"]), tolerance = 1e-6)
  expect_equal(res_w$zval, unname(s_w[, "z value"]), tolerance = 1e-6)
  expect_equal(res_w$pval, unname(s_w[, "Pr(>|z|)"]), tolerance = 1e-6)

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

test_that("blr_regress errors on non-logit models", {
  m_prob <- glm(honcomp ~ female + read + science, data = hsb2, family = binomial(link = "probit"))
  expect_error(blr_regress(m_prob), "logit")
})

test_that("blr_check_model errors on gaussian glm", {
  m_gauss <- glm(read ~ female + science, data = hsb2, family = gaussian())
  expect_error(blr_check_model(m_gauss), "binomial")
})

test_that("blr_regress keeps weights and offset of the fitted model", {
  set.seed(42)
  w <- runif(nrow(hsb2), 0.5, 2)
  m_w <- glm(honcomp ~ female + read + science, data = hsb2, family = binomial(link = "logit"), weights = w)
  res_w <- blr_regress(m_w)
  s_w <- summary(m_w)$coefficients

  expect_equal(res_w$est, unname(s_w[, "Estimate"]), tolerance = 1e-6)
  expect_equal(res_w$se, unname(s_w[, "Std. Error"]), tolerance = 1e-6)
  expect_equal(res_w$zval, unname(s_w[, "z value"]), tolerance = 1e-6)
  expect_equal(res_w$pval, unname(s_w[, "Pr(>|z|)"]), tolerance = 1e-6)

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

test_that("blr_regress errors on non-logit models", {
  m_prob <- glm(honcomp ~ female + read + science, data = hsb2, family = binomial(link = "probit"))
  expect_error(blr_regress(m_prob), "logit")
})

test_that("blr_check_model errors on gaussian glm", {
  m_gauss <- glm(read ~ female + science, data = hsb2, family = gaussian())
  expect_error(blr_check_model(m_gauss), "binomial")
})
