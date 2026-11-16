test_that("delta deviance uses the squared deviance residual plus CBAR", {
  model <- glm(am ~ wt + hp, data = mtcars, family = binomial())
  h     <- hatvalues(model)
  cbar  <- residuals(model, type = "pearson") ^ 2 * h / (1 - h)
  expected <- residuals(model, type = "deviance") ^ 2 + cbar

  actual <- blr_residual_diagnostics(model)
  expect_equal(unname(actual$difdev), unname(expected), tolerance = 1e-10)
  expect_equal(unname(round(actual$difdev[1:3], 5)),
               c(0.38538, 2.27269, 0.06331))
})

test_that("delta deviance approximates the deviance change from deleting each case", {
  model  <- glm(am ~ wt + hp, data = mtcars, family = binomial())
  approx <- blr_residual_diagnostics(model)$difdev[1:3]
  exact  <- vapply(1:3, function(i) {
    refit <- suppressWarnings(update(model, data = mtcars[-i, ]))
    deviance(model) - deviance(refit)
  }, numeric(1))
  # exact refits: 0.383, 2.305, 0.063; the one-step approximation is close
  expect_equal(approx, exact, tolerance = 0.02)
})

test_that("C, CBAR and delta chi-square follow their definitions", {
  model <- glm(honcomp ~ female + read + science, data = hsb2,
               family = binomial(link = "logit"))
  h  <- hatvalues(model)
  rp <- residuals(model, type = "pearson")
  d  <- blr_residual_diagnostics(model)
  expect_equal(unname(d$c), unname(rp ^ 2 * h / (1 - h) ^ 2))
  expect_equal(unname(d$cbar), unname(rp ^ 2 * h / (1 - h)))
  expect_equal(unname(d$difchisq), unname(rp ^ 2 / (1 - h)))
})
