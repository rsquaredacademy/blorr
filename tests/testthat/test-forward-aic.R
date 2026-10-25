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
