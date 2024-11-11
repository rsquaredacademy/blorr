test_that("output from backward variable p elimination is as expected", {

  model <- glm(
    honcomp ~ female + read + science + math + prog + socst,
    data = hsb2, family = binomial(link = "logit")
  )

  expect_snapshot(blr_step_p_backward(model))

})
