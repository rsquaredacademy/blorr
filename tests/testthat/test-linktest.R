context("test-linktest.R")

test_that("output from blr_linktest is as expected", {
  model <- stats::glm(
    honcomp ~ female + read + science, data = hsb2,
    family = stats::binomial(link = "logit")
  )
  actual <- 
    model %>%
    blr_linktest() %>%
    magrittr::use_series(coefficients) %>%
    `[`(, 1) %>%
    unname() %>%
    round(2)
  expected <- c(0.04, 0.94, -0.04)
  expect_equivalent(actual, expected)
})
