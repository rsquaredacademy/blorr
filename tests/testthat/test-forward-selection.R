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


test_that("output from forward variable selection is as expected", {

  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  expect_snapshot(blr_step_aic_forward(model))

})

test_that("output from forward variable p selection is as expected", {

  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  expect_snapshot(blr_step_p_forward(model))

})

test_that("output from forward variable selection is as expected", {

  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  expect_snapshot(blr_step_aic_forward(model, details = TRUE))
  
})


