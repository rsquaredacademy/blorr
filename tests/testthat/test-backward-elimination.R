context("test-backward-elimination.R")

test_that("output from backward variable elimination is as expected", {

  model <- glm(honcomp ~ female + read + science + math + prog + socst,
               data = blorr::hsb2, family = binomial(link = 'logit'))

  actual <-
    model %>%
    blr_backward_elimination %>%
    use_series(predictors)

  expected <- c("prog", "socst")

  expect_equivalent(actual, expected)

})
