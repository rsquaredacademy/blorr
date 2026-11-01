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

test_that("VIF assesses every predictor in a model without an intercept", {
  model <- glm(am ~ 0 + wt + hp, data = mtcars, family = binomial)

  v <- blr_vif_tol(model)

  tol_wt <- 1 - summary(lm(wt ~ hp, data = mtcars))$r.squared
  tol_hp <- 1 - summary(lm(hp ~ wt, data = mtcars))$r.squared

  expect_equal(v$Variable, c("wt", "hp"))
  expect_equal(v$Tolerance, c(tol_wt, tol_hp))
  expect_equal(v$VIF, 1 / c(tol_wt, tol_hp))
  expect_equal(v$VIF[v$Variable == "hp"], 1.7666245792683, tolerance = 1e-10)

  v1 <- blr_vif_tol(glm(am ~ 0 + wt, data = mtcars, family = binomial))

  expect_equal(v1$Variable, "wt")
  expect_equal(v1$Tolerance, 1)
  expect_equal(v1$VIF, 1)
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

test_that("nearly collinear full-rank designs keep a finite condition index", {
  set.seed(1)
  d <- mtcars
  d$wt2 <- d$wt + rnorm(32, sd = 1e-4)
  model <- glm(am ~ wt + wt2, data = d, family = binomial)

  e <- blr_eigen_cindex(model)
  last <- nrow(e)

  expect_equal(model$rank, 3)
  expect_gt(e[["Eigenvalue"]][last], 0)
  expect_true(is.finite(e[["Condition Index"]][last]))
  expect_equal(e[["Condition Index"]][last], 89785.8298103, tolerance = 1e-6)

  z  <- scale(model.matrix(model), center = FALSE, scale = TRUE)
  s  <- svd(z)$d
  expect_equal(e[["Condition Index"]][last], max(s) / min(s),
               tolerance = 1e-8)
})

test_that("condition indices follow the scaled-design singular values", {
  set.seed(2)
  d <- mtcars
  d$wt2 <- d$wt + rnorm(32, sd = 1e-8)
  model <- glm(am ~ wt + wt2, data = d, family = binomial)

  z <- scale(model.matrix(model), center = FALSE, scale = TRUE)
  s <- svd(z)$d

  e    <- blr_eigen_cindex(model)
  last <- nrow(e)

  expect_equal(model$rank, 3)
  expect_equal(e[["Eigenvalue"]], s ^ 2 / (nrow(z) - 1))
  expect_equal(e[["Eigenvalue"]][last], min(s) ^ 2 / (nrow(z) - 1),
               tolerance = 1e-8)
  expect_gt(e[["Eigenvalue"]][last], 0)
  expect_equal(e[["Condition Index"]][last], max(s) / min(s),
               tolerance = 1e-8)
  expect_true(is.finite(e[["Condition Index"]][last]))

  set.seed(1)
  d <- mtcars
  d$wt2 <- d$wt + rnorm(32, sd = 1e-8)
  model <- glm(am ~ wt + wt2, data = d, family = binomial)

  z <- scale(model.matrix(model), center = FALSE, scale = TRUE)
  s <- svd(z)$d
  e <- blr_eigen_cindex(model)

  expect_equal(e[["Condition Index"]][nrow(e)], max(s) / min(s),
               tolerance = 1e-8)
})

test_that("zero columns get unavailable rather than broken collinearity diagnostics", {
  model <- glm(am ~ wt + z, data = transform(mtcars, z = 0),
               family = binomial)

  e <- blr_eigen_cindex(model)

  expect_false(any(is.nan(as.matrix(e))))
  expect_true(is.infinite(e[["Condition Index"]][nrow(e)]))
  expect_true(all(is.na(as.matrix(e[nrow(e), -(1:2)]))))

  v <- blr_vif_tol(model)
  expect_equal(v$Variable, c("wt", "z"))
  expect_true(is.infinite(v$VIF[v$Variable == "z"]))
})

