test_that("output from blr_bivariate_analysis is as expected", {
  k <- blr_bivariate_analysis(hsb2, honcomp, female, prog)
  expect_equal(k$iv, c(0.1023, 0.4329))
  expect_equal(round(k$likelihood_ratio, 2), c(3.94, 16.15))
})


test_that("blr_bivariate_analysis prints the correct output", {
  k <- blr_bivariate_analysis(hsb2, honcomp, female, prog)
  expect_snapshot(k)
})

test_that("output from blr_segment is as expected", {
  
  k        <- blr_segment(hsb2, honcomp, race)
  actual   <- rev(k$segment_data[['1s%']])[1]
  expected <- c(0.22)

  expect_equal(actual, expected)
})

test_that("blr_segment prints the correct output", {
  k <- blr_segment(hsb2, honcomp, race)
  expect_snapshot(k)
})

test_that("output from blr_twoway_segment is as expected", {

  k        <- blr_segment_twoway(hsb2, honcomp, prog, race)
  actual   <- unname(round(k$twoway_segment[, 4], 3))
  expected <- c(0.035, 0.165, 0.020)
  expect_equal(actual, expected)
})

test_that("blr_twoway_segment prints the correct output", {
  k <- blr_segment_twoway(hsb2, honcomp, prog, race)
  expect_snapshot(k)
})

test_that("output from blr_segment_dist is as expected", {
  
  k        <- blr_segment_dist(hsb2, honcomp, race)
  actual   <- rev(k$dist_table[['1s%']])[1]
  expected <- c(0.22)
  expect_equal(actual, expected)
})

test_that("blr_twoway_segment prints the correct output", {
  k <- blr_segment_dist(hsb2, honcomp, race)
  expect_snapshot(k)
})


test_that("output from blr_woe_iv is as expected", {
  k        <- blr_woe_iv(hsb2, prog, honcomp)
  actual   <- sum(k$woe_iv_table[['iv']])
  expected <- 0.4329
  expect_equal(actual, expected)
})

test_that("blr_woe_iv prints the correct output", {
  k <- blr_woe_iv(hsb2, prog, honcomp)
  expect_snapshot(k)
})


test_that("output from blr_woe_iv_stats is as expected", {
  k <- blr_woe_iv_stats(hsb2, honcomp, prog, race, female, schtyp)
  expect_snapshot(k)
})
