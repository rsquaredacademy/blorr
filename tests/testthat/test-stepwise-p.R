test_that("output from forward variable selection is as expected", {

  model <- glm(y ~ ., data = stepwise, family = binomial(link = 'logit'))
  expect_snapshot(blr_step_p_both(model))

})

