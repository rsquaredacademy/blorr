context("test-blr-gains-table.R")

model <- stats::glm(
  honcomp ~ female + read + science, data = hsb2,
  family = stats::binomial(link = "logit")
)

test_that("output from blr_gains_table is as expected", {
  gtable <- blr_gains_table(model, hsb2)
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


