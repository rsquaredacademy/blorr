test_that("output from forward variable selection is as expected", {

  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  expect_snapshot(blr_step_aic_forward(model))

})

test_that("forward selection reports the BIC and deviance of each step", {
  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  k       <- blr_step_aic_forward(model)
  n_steps <- length(k$predictors)
  aics    <- bics <- devs <- numeric(n_steps)

  for (i in seq_len(n_steps)) {
    m <- glm(
      as.formula(paste("honcomp ~", paste(k$predictors[seq_len(i)], collapse = " + "))),
      data = hsb2, family = binomial(link = "logit")
    )

    aics[i] <- AIC(m)
    bics[i] <- BIC(m)
    devs[i] <- deviance(m)
  }

  expect_equal(k$aics, aics)
  expect_equal(k$bics, bics)
  expect_equal(k$devs, devs)
})

test_that("forward AIC selection does not enter an inferior first predictor", {
  model <- glm(y ~ a + b + c, data = noise_data(), family = binomial)
  null  <- update(model, . ~ 1)

  k <- blr_step_aic_forward(model)

  expect_equal(k$steps, 0)
  expect_equal(k$predictors, character(0))
  expect_equal(deparse(formula(k$model)), "y ~ 1")
  expect_equal(AIC(k$model), AIC(null))
  expect_equal(k$aics, AIC(null))
  expect_equal(k$bics, BIC(null))
  expect_equal(k$devs, deviance(null))
})

test_that("forward AIC selection still enters a predictor that improves AIC", {
  set.seed(4)
  d <- data.frame(y = rbinom(120, 1, 0.5),
                  x1 = rnorm(120), x2 = rnorm(120), x3 = rnorm(120))
  d$y <- as.integer(d$x1 + rnorm(120, sd = 0.3) > 0)
  model <- glm(y ~ x1 + x2 + x3, data = d, family = binomial)
  null  <- update(model, . ~ 1)

  k <- blr_step_aic_forward(model)

  expect_gt(k$steps, 0)
  expect_equal(k$predictors[1], "x1")
  expect_true("x1" %in% k$predictors)
  expect_lt(AIC(k$model), AIC(null))
})

