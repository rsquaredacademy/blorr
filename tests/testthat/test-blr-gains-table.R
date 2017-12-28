context("test-blr-gains-table.R")

test_that("output from blr_gains_tableis as expected", {

  model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
               family = binomial(link = 'logit'))

  gtable <- blr_gains_table(model, hsb2)
  actual <- gtable %>%
    use_series(gains_table) %>%
    select(sensitivity, specificity, accuracy) %>%
    summarise_all(mean) %>%
    unlist %>%
    unname %>%
    round(2)
  expected <- c(80.94, 54.35, 61.40)
  expect_equivalent(actual, expected)

})
