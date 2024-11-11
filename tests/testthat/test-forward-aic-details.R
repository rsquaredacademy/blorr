test_that("output from forward variable selection is as expected", {

  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  expect_snapshot(blr_step_aic_forward(model, details = TRUE))

})
