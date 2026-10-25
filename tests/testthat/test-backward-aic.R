test_that("output from backward variable elimination is as expected", {

  model <- glm(
    honcomp ~ female + read + science + math + prog + socst,
    data = hsb2, family = binomial(link = "logit")
  )

  expect_snapshot(blr_step_aic_backward(model))

})

test_that("backward AIC elimination returns the intercept-only model when all predictors are removed", {
  model <- glm(y ~ a + b + c, data = noise_data(), family = binomial)

  invisible(capture.output(k <- blr_step_aic_backward(model)))

  expect_equal(k$steps, 3)
  expect_equal(names(coef(k$model)), "(Intercept)")
})
