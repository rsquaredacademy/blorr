test_that("output from blr_vif_tol is as expected", {
  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  actual   <- round(blr_vif_tol(model)[['VIF']], 2)
  expected <- c(1.02, 1.66, 1.68)

  expect_equal(actual, expected)
})

test_that("output from blr_eigen_cindex is as expected", {
  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  actual   <- round(blr_eigen_cindex(model)[['Eigenvalue']], 2)
  expected <- c(3.57, 0.39, 0.02, 0.01)

  expect_equal(actual, expected)
})

test_that("blr_coll_diag prints the correct output", {
  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  k <- blr_coll_diag(model)

  expect_snapshot(k)
})

test_that("variance decomposition proportions sum to one for each coefficient", {
  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  k     <- blr_eigen_cindex(model)
  props <- as.matrix(k[, -(1:2)])

  expect_equal(unname(colSums(props)), rep(1, ncol(props)))

  z     <- scale(model.matrix(model), center = FALSE, scale = TRUE)
  eig   <- eigen(crossprod(z))
  phi2  <- sweep(eig$vectors ^ 2, 2, eig$values, "/")
  pi_mat <- sweep(phi2, 1, rowSums(phi2), "/")

  expect_equal(unname(props), unname(t(pi_mat)))
})
