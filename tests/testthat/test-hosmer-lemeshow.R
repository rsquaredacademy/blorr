context("test-hosmer-lemeshow.R")

test_that("output from blr_hosmer_lemeshow_test is as expected", {

  model <- glm(honcomp ~ race + read + science, data = hsb2,
               family = binomial(link = 'logit'))
  actual <- model %>%
    blr_hosmer_lemeshow_test %>%
    use_series(pvalue) %>%
    round(2)
  expected <- 0.66
  expect_equal(actual, expected)

})
