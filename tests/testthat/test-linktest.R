test_that("output from blr_linktest is as expected", {

   model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  actual   <- round(unname(blr_linktest(model)$coefficients[, 1]), 2)
  expected <- c(0.04, 0.94, -0.04)
  expect_equal(actual, expected)
})

test_that("blr_linktest throws an error on weighted models", {
  set.seed(42)
  w <- sample(1:4, nrow(hsb2), replace = TRUE)
  m_w <- glm(honcomp ~ female + read + science, data = hsb2, family = binomial(link = "logit"), weights = w)
  expect_error(blr_linktest(m_w), "Weights are not supported")
})

