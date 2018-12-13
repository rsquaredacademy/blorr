context("test-blr-pairs.R")

test_that("output from blr_pairs is as expected", {
  model <- stats::glm(
    honcomp ~ female + read + science, data = blorr::hsb2,
    family = stats::binomial(link = "logit")
  )

  actual <- 
  	model %>%
  	blr_pairs() %>%
    dplyr::pull(gamma) %>%
    round(2)

  expected <- 0.71

  expect_equal(actual, expected)
})
