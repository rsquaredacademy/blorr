context("test-blr-woe-iv.R")

test_that('output from blr_woe_iv is as expected', {

  actual <- blr_woe_iv(hsb2, female, honcomp) %>%
    pull(iv)
  expected <- c(0.06, 0.04)
  expect_equivalent(actual, expected)

})
