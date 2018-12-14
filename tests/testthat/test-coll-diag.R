context("test-coll-diag.R")

test_that("output from blr_vif_tol is as expected", {
  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  actual <-
    model %>%
    blr_vif_tol() %>%
    pull(VIF) %>%
    round(2)

  expected <- c(1.02, 1.66, 1.68)

  expect_equivalent(actual, expected)
})

test_that("output from blr_eigen_cindex is as expected", {
  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  actual <-
    model %>%
    blr_eigen_cindex() %>%
    pull(Eigenvalue) %>%
    round(2)

  expected <- c(3.57, 0.39, 0.02, 0.01)

  expect_equivalent(actual, expected)
})

test_that("blr_coll_diag prints the correct output", {
  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  k <- blr_coll_diag(model)

  x <- cat(
    "Tolerance and Variance Inflation Factor
---------------------------------------
    # A tibble: 3 x 3
    Variable Tolerance   VIF
    <chr>        <dbl> <dbl>
    1 female1      0.982  1.02
    2 read         0.602  1.66
    3 science      0.594  1.68


    Eigenvalue and Condition Index
    ------------------------------
    Eigenvalue Condition Index   intercept    female1        read     science
    1 3.57391760        1.000000 0.002062502 0.02386799 0.001675904 0.001561977
    2 0.39409893        3.011408 0.003037479 0.91508462 0.003889972 0.004232601
    3 0.01888407       13.757025 0.961793702 0.04701680 0.291423043 0.090184221
    4 0.01309940       16.517583 0.033106318 0.01403058 0.703011081 0.904021201"
  )

  expect_output(print(k), x)
})
