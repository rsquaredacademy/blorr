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

test_that("constant predictors get explicit unavailable collinearity diagnostics", {
  d <- mtcars
  d$z <- 1
  model <- glm(am ~ wt + z, data = d, family = binomial)

  v <- blr_vif_tol(model)
  expect_equal(v$Variable, c("wt", "z"))
  expect_equal(v$Tolerance[v$Variable == "wt"], 1)
  expect_true(is.infinite(v$VIF[v$Variable == "z"]))

  e <- blr_eigen_cindex(model)
  expect_false(any(is.nan(as.matrix(e))))
  expect_true(is.infinite(e[["Condition Index"]][nrow(e)]))
})

test_that("VIF works for a single-predictor model", {
  v <- blr_vif_tol(glm(am ~ wt, data = mtcars, family = binomial))

  expect_equal(v$Variable, "wt")
  expect_equal(v$Tolerance, 1)
  expect_equal(v$VIF, 1)
})

