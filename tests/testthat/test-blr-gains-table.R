context("test-blr-gains-table.R")

model <- stats::glm(y ~ ., data = bank_marketing, family = stats::binomial(link = 'logit'))

test_that("output from blr_gains_table is as expected", {

  skip_on_cran()
  skip_on_travis()
  
  gtable <- blr_gains_table(model, data = bank_marketing)
  actual <- 
    gtable %>%
    magrittr::use_series(gains_table) %>%
    dplyr::select(sensitivity, specificity, accuracy) %>%
    dplyr::summarise_all(mean) %>%
    unlist() %>%
    unname() %>%
    round(2)
  expected <- c(90.77, 49.63, 54.34)
  expect_equivalent(actual, expected)
})


