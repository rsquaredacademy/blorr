test_that("output is as expected when details == TRUE", {

  model <- glm(
    honcomp ~ female + read + science + math + prog + socst,
    data = hsb2, family = binomial(link = "logit")
  )

  expect_snapshot(blr_step_aic_backward(model, details = TRUE))

})
