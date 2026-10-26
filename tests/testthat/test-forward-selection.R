test_that("output from forward variable selection is as expected", {
  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  actual   <- blr_step_aic_forward(model)$predictors 
  expected <- c("read", "female", "science")

  expect_equal(actual, expected)
})

test_that("output from forward variable p selection is as expected", {
  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  actual   <- blr_step_p_forward(model)$predictors
  expected <- c("read", "female", "science")

  expect_equal(actual, expected)
})


test_that("blr_step_aic_forward throws an error on weighted models", {
  set.seed(42)
  w <- sample(1:4, nrow(hsb2), replace = TRUE)
  m_w <- glm(honcomp ~ female + read + science, data = hsb2, family = binomial(link = "logit"), weights = w)
  expect_error(blr_step_aic_forward(m_w), "Weights are not supported")
})

