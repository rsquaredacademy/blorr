test_that("output from forward variable p selection is as expected", {

  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  expect_snapshot(blr_step_p_forward(model))

})

