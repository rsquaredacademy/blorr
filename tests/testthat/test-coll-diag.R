context("test-coll-diag.R")

test_that("output from blr_vif_tol is as expected", {

  model <- glm(honcomp ~ female + read + science, data = hsb2,
               family = binomial(link = 'logit'))

  actual <-
    model %>%
    blr_vif_tol %>%
    pull(VIF) %>%
    round(2)

  expected <- c(1.02, 1.66, 1.68)

  expect_equivalent(actual, expected)

})

test_that("output from blr_eigen_cindex is as expected", {

  model <- glm(honcomp ~ female + read + science, data = hsb2,
               family = binomial(link = 'logit'))

  actual <-
    model %>%
    blr_eigen_cindex %>%
    pull(Eigenvalue) %>%
    round(2)

  expected <- c(3.57, 0.39, 0.02, 0.01)

  expect_equivalent(actual, expected)

})
