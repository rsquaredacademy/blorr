context("test-forward-selection.R")

test_that("output from forward variable selection is as expected", {

  model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
               family = binomial(link = 'logit'))

  actual <-
    model %>%
    blr_forward_selection %>%
    use_series(predictors)

  expected <- c("read", "female", "science")

  expect_equivalent(actual, expected)

})
