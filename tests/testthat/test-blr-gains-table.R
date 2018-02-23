context("test-blr-gains-table.R")

model <- glm(
  honcomp ~ female + read + science, data = hsb2,
  family = binomial(link = "logit")
)

test_that("output from blr_gains_table is as expected", {
  gtable <- blr_gains_table(model, hsb2)
  actual <- gtable %>%
    use_series(gains_table) %>%
    select(sensitivity, specificity, accuracy) %>%
    summarise_all(mean) %>%
    unlist() %>%
    unname() %>%
    round(2)
  expected <- c(80.94, 54.35, 61.40)
  expect_equivalent(actual, expected)
})


