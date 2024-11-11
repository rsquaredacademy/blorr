test_that("output from stepwise variable selection is as expected", {

  model    <- glm(y ~ ., data = stepwise, family = binomial(link = 'logit'))
  actual   <- blr_step_aic_both(model)$predictors
  expected <- c("x6", "x1", "x3", "x2", "x6", "x5")

  expect_equal(actual, expected)
})

test_that("output from stepwise variable p selection is as expected", {

   model   <- glm(y ~ ., data = stepwise, family = binomial(link = 'logit'))
  actual   <- blr_step_p_both(model)$orders
  expected <- c("x1", "x2", "x3", "x6", "x5", "x6")

  expect_equal(actual, expected)
})

