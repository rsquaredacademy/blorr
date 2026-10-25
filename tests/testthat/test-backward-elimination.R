test_that("output from backward variable elimination is as expected", {
  model <- glm(
    honcomp ~ female + read + science + math + prog + socst,
    data = hsb2, family = binomial(link = "logit")
  )

  actual   <- blr_step_aic_backward(model)$predictors
  expected <- c("prog", "socst")

  expect_equal(actual, expected)
})

test_that("output from backward variable p elimination is as expected", {
  model <- glm(
    honcomp ~ female + read + science + math + prog + socst,
    data = hsb2, family = binomial(link = "logit")
  )

  actual   <- blr_step_p_backward(model)$removed
  expected <- c("prog", "socst")

  expect_equal(actual, expected)
})

test_that("backward p-value elimination returns the intercept-only model when all predictors are removed", {
  model <- glm(y ~ a + b + c, data = noise_data(), family = binomial)

  invisible(capture.output(k <- blr_step_p_backward(model, prem = 0.3)))

  expect_equal(k$steps, 3)
  expect_equal(k$removed, c("c", "a", "b"))
  expect_equal(names(coef(k$model)), "(Intercept)")
})

test_that("backward AIC elimination prints details when the null model wins", {
  model <- glm(y ~ a + b + c, data = noise_data(), family = binomial)

  out <- capture.output(k <- blr_step_aic_backward(model, details = TRUE))

  expect_equal(k$steps, 3)
  expect_equal(names(coef(k$model)), "(Intercept)")
  expect_true(any(grepl("Step 3", out, fixed = TRUE)))
})

