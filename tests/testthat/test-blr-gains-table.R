context("test-blr-gains-table.R")

model <- glm(honcomp ~ female + read + science, data = hsb2,
             family = binomial(link = 'logit'))

test_that("output from blr_gains_table is as expected", {

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


test_that("decile lift chart is as expected", {

  skip_on_cran()

  p <-
    model %>%
    blr_gains_table %>%
    blr_decile_lift_chart

  vdiffr::expect_doppelganger('decile lift chart', p$plot)

})

test_that("decile wise capture rate chart is as expected", {

  skip_on_cran()

  p <-
    model %>%
    blr_gains_table %>%
    blr_decile_capture_rate

  vdiffr::expect_doppelganger('decile capture rate chart', p$plot)

})
