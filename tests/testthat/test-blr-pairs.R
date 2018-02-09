context("test-blr-pairs.R")

test_that("output from blr_pairs is as expected", {
  model <- glm(
    honcomp ~ female + read + science, data = blorr::hsb2,
    family = binomial(link = "logit")
  )

  actual <- blr_pairs(model) %>%
    pull(gamma) %>%
    round(2)

  expected <- 0.71

  expect_equal(actual, expected)
})
