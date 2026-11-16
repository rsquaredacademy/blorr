test_that("output from backward variable elimination is as expected", {

  model <- glm(
    honcomp ~ female + read + science + math + prog + socst,
    data = hsb2, family = binomial(link = "logit")
  )

  expect_snapshot(blr_step_aic_backward(model))

})

test_that("backward AIC elimination returns the intercept-only model when all predictors are removed", {
  model <- glm(y ~ a + b + c, data = noise_data(), family = binomial)

  invisible(capture.output(k <- blr_step_aic_backward(model)))

  expect_equal(k$steps, 3)
  expect_equal(names(coef(k$model)), "(Intercept)")
})

test_that("backward AIC baseline row comes from one refitted full model (bfx9)", {
  set.seed(1)
  d <- data.frame(x = rnorm(240), z = rnorm(240), v = rnorm(240))
  d$y <- rbinom(240, 1, plogis(2 + 1.4*d$x + 1.2*d$z + .8*d$v))
  m0 <- glm(y ~ 0 + x + z + v, d, family = binomial)
  m1 <- glm(y ~ x + z + v, d, family = binomial)
  invisible(capture.output(k0 <- blr_step_aic_backward(m0)))
  invisible(capture.output(k1 <- blr_step_aic_backward(m1)))

  expect_equal(k1$result$aic[1], AIC(m1))
  expect_equal(k1$result$bic[1], BIC(m1))
  expect_equal(k1$result$deviance[1], deviance(m1))
  expect_equal(AIC(m1), 187.4759410925487, tolerance = 1e-9)

  # the no-intercept input is refitted with an intercept: same triple, no
  # removals, same final model as the intercept input
  expect_equal(k0$result$aic[1], AIC(m1))
  expect_equal(k0$result$bic[1], BIC(m1))
  expect_equal(k0$result$deviance[1], deviance(m1))
  expect_equal(nrow(k0$result), 1)
  expect_equal(nrow(k1$result), 1)
  expect_equal(sort(names(coef(k0$model))), sort(names(coef(m1))))
  expect_equal(unname(coef(k0$model)), unname(coef(m1)), tolerance = 1e-6)

  # every removal is worse than the baseline
  for (t in c("x", "z", "v")) {
    r <- glm(reformulate(setdiff(c("x", "z", "v"), t), "y"), d,
             family = binomial)
    expect_gt(AIC(r), AIC(m1))
  }
})
