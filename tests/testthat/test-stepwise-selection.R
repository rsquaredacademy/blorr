context("test-stepwise-selection.R")

test_that("output from stepwise variable selection is as expected", {

  model <- glm(y ~ ., data = stepwise)

  actual <-
    model %>%
    blr_stepwise_selection %>%
    use_series(predictors)

  expected <- c("x6", "x1", "x3", "x2", "x6", "x5")

  expect_equivalent(actual, expected)

})
