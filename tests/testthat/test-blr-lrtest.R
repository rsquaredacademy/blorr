context("test-blr-lrtest.R")

test_that("blr_lr_test throws an error when the model is not an object of\n          class glm", {
  model1 <- glm(
    honcomp ~ 1, data = hsb2,
    family = binomial(link = "logit")
  )


  model2 <- lm(write ~ 1, data = hsb2)
  expect_error(blr_lr_test(model1, model2))
  expect_error(blr_lr_test(model2, model1))
})

test_that("output is as expected when reduced model is not specified", {
  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  actual <- blr_lr_test(model) %>%
    use_series(test_result) %>%
    pull(lr_ratio) %>%
    round(4)

  expected <- 71.0525

  expect_equal(actual, expected)
})
