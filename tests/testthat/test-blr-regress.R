test_that("output from blr_regress is as expected", {
  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  k <- blr_regress(model, odd_conf_limit = TRUE)

  expect_snapshot(k)
})
