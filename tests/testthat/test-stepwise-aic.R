test_that("output from forward variable selection is as expected", {

  model <- glm(y ~ ., data = stepwise, family = binomial(link = 'logit'))
  expect_snapshot(blr_step_aic_both(model))

})

test_that("stepwise AIC selection returns the intercept-only model when the null model wins", {
  model <- glm(y ~ a + b + c, data = noise_data(), family = binomial)

  invisible(capture.output(k <- blr_step_aic_both(model)))

  expect_equal(k$steps, 0)
  expect_equal(k$predictors, character(0))
  expect_equal(names(coef(k$model)), "(Intercept)")
})
