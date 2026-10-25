test_that("output from stepwise variable selection is as expected", {

  model    <- glm(y ~ ., data = stepwise, family = binomial(link = 'logit'))
  actual   <- blr_step_aic_both(model)$predictors
  expected <- c("x6", "x1", "x3", "x2", "x6", "x5")

  expect_equal(actual, expected)
})

test_that("output from stepwise variable p selection is as expected", {

   model   <- glm(y ~ ., data = stepwise, family = binomial(link = 'logit'))
  actual   <- blr_step_p_both(model)$orders
  expected <- c("x1", "x2", "x3", "x6", "x6", "x5")

  expect_equal(actual, expected)
})

noise_data <- function() {
  set.seed(11)
  data.frame(
    y = rbinom(120, 1, 0.5),
    a = rnorm(120),
    b = rnorm(120),
    c = rnorm(120)
  )
}

test_that("stepwise p-value selection returns the intercept-only model when nothing enters", {
  model <- glm(y ~ a + b + c, data = noise_data(), family = binomial)

  invisible(capture.output(k <- blr_step_p_both(model, pent = 0.1, prem = 0.3)))

  expect_equal(k$steps, 0)
  expect_equal(k$predictors, character(0))
  expect_equal(names(coef(k$model)), "(Intercept)")
})

test_that("stepwise p-value selection removes the first predictor when it exceeds prem", {
  model <- glm(y ~ a + b + c, data = noise_data(), family = binomial)

  invisible(capture.output(k <- blr_step_p_both(model, pent = 0.5, prem = 0.3)))

  expect_true(k$steps >= 2)
  expect_equal(k$orders[1:2], c("b", "b"))
  expect_equal(k$predictors, character(0))
  expect_equal(names(coef(k$model)), "(Intercept)")
})

test_that("forward p-value selection returns the intercept-only model when nothing enters", {
  model <- glm(y ~ a + b + c, data = noise_data(), family = binomial)

  invisible(capture.output(k <- blr_step_p_forward(model, penter = 0.1)))

  expect_equal(k$steps, 0)
  expect_equal(k$predictors, character(0))
  expect_equal(names(coef(k$model)), "(Intercept)")
})

test_that("forward selection documentation describes AIC", {
  rd <- system.file("man", "blr_step_aic_forward.Rd", package = "blorr")
  skip_if_not(nzchar(rd) && file.exists(rd))

  expect_true(any(grepl("Akaike information criterion", readLines(rd))))
})

