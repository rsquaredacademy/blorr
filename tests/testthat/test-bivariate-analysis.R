context("test-bivariate-analysis.R")

test_that("output from blr_bivariate_analysis is as expected", {

  k <- blr_bivariate_analysis(hsb2, honcomp, female, prog)
  expect_equivalent(k$iv, c(0.1023, 0.4329))
  expect_equivalent(round(k$likelihood_ratio, 2), c(3.94, 16.15))

})

test_that("output from blr_segment is as expected", {

  k <- blr_segment(hsb2, honcomp, race)
  actual <-
    k %>%
    use_series(segment_data) %>%
    pull(`1s%`)
  expected <- c(0.01, 0.02, 0.01, 0.22)
  expect_equivalent(actual, expected)

})

test_that("output from blr_twoway_segment is as expected", {

  k <- blr_twoway_segment(hsb2, honcomp, prog, race)
  actual <-
    k %>%
    use_series(twoway_segment) %>%
    `[`(, 1) %>%
    unname
  expected <- c(0.00, 0.01, 0.00)
  expect_equivalent(actual, expected)

})

test_that("output from blr_segment_dist is as expected", {

  k <- blr_segment_dist(hsb2, honcomp, race)
  actual <-
    k %>%
    use_series(dist_table) %>%
    pull(`1s%`)
  expected <- c(0.01, 0.02, 0.01, 0.22)
  expect_equivalent(actual, expected)

})


test_that("output from blr_woe_iv is as expected", {

  k <- blr_woe_iv(hsb2, prog, honcomp)
  actual <-
    k %>%
    use_series(woe_iv_table) %>%
    pull(iv) %>%
    sum
  expected <- 0.4329
  expect_equal(actual, expected)

})
