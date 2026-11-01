test_that("blr_test_lr throws an error when the model is not an object of class glm", {

  model1 <- glm(
    honcomp ~ 1, data = hsb2,
    family = binomial(link = "logit")
  )

  model2 <- lm(write ~ 1, data = hsb2)
  expect_error(blr_test_lr(model1, model2))
  expect_error(blr_test_lr(model2, model1))

})

test_that("output is as expected when reduced model is not specified", {

   model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  actual   <- round(blr_test_lr(model)$test_result[['lr_ratio']], 4)
  expected <- 71.0525

  expect_equal(actual, expected)

})

test_that("LR test df for nested models matches anova", {

  full_model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  reduced_model <- glm(
    honcomp ~ female + read, data = hsb2,
    family = binomial(link = "logit")
  )

  k <- blr_test_lr(full_model, reduced_model)
  a <- anova(reduced_model, full_model, test = "LRT")

  expect_equal(k$test_result[['lr_ratio']], a[['Deviance']][2])
  expect_equal(k$test_result[['d_f']], a[['Df']][2])
  expect_equal(k$test_result[['p_value']], a[['Pr(>Chi)']][2])

})

test_that("LR test uses the model frame when the data has missing values", {

  d <- hsb2
  d$read[which(d$honcomp == "0")[1:10]] <- NA

  model <- glm(
    honcomp ~ female + read + science, data = d,
    family = binomial(link = "logit")
  )

  k   <- blr_test_lr(model)
  ll0 <- as.numeric(logLik(glm(honcomp ~ 1, data = model.frame(model),
                               family = binomial(link = "logit"))))

  expect_equal(k$test_result[['d_f']], 3)
  expect_equal(k$test_result[['lr_ratio']], -2 * (ll0 - as.numeric(logLik(model))))

})

test_that("LR test rejects models fitted to different observations", {
  d <- mtcars
  d$hp[1:5] <- NA
  full    <- glm(am ~ wt + hp, data = d, family = binomial)
  reduced <- glm(am ~ wt, data = d, family = binomial)

  expect_error(
    blr_test_lr(full, reduced),
    "same observations"
  )
})

test_that("LR test rejects a non-nested reduced model", {
  full    <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  reduced <- suppressWarnings(glm(am ~ wt + qsec, data = mtcars,
                                  family = binomial,
                                  weights = rep(1, nrow(mtcars))))

  expect_error(
    blr_test_lr(full, reduced),
    "nested"
  )
})

test_that("LR test rejects a reduced intercept absent from the full design", {
  full    <- glm(am ~ 0 + wt + hp, data = mtcars, family = binomial)
  reduced <- glm(am ~ wt, data = mtcars, family = binomial)

  expect_error(
    blr_test_lr(full, reduced),
    "nested"
  )
})

test_that("LR test rejects a reduced design fitted to different predictor values", {
  full    <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  reduced <- glm(am ~ wt, data = transform(mtcars, wt = rev(wt)),
                 family = binomial)

  expect_error(
    blr_test_lr(full, reduced),
    "nested"
  )
})

test_that("LR test accepts a nested design with different factor contrasts", {
  d    <- transform(mtcars, g = factor(cyl))
  full <- glm(am ~ g + hp, data = d, family = binomial,
              contrasts = list(g = "contr.treatment"))
  red  <- glm(am ~ g, data = d, family = binomial,
              contrasts = list(g = "contr.sum"))

  k <- blr_test_lr(full, red)
  a <- anova(red, full, test = "Chisq")

  expect_equal(k$test_result[["lr_ratio"]], a[["Deviance"]][2])
  expect_equal(k$test_result[["d_f"]], a[["Df"]][2])
  expect_equal(k$test_result[["p_value"]], a[["Pr(>Chi)"]][2])
  expect_equal(k$test_result[["lr_ratio"]], 6.7312378514618,
               tolerance = 1e-9)

  # the same containment holds when the full model uses the sum coding
  full2 <- glm(am ~ g + hp, data = d, family = binomial,
               contrasts = list(g = "contr.sum"))
  red2  <- glm(am ~ g, data = d, family = binomial,
               contrasts = list(g = "contr.treatment"))

  k2 <- blr_test_lr(full2, red2)
  a2 <- anova(red2, full2, test = "Chisq")

  expect_equal(k2$test_result[["lr_ratio"]], a2[["Deviance"]][2])
  expect_equal(k2$test_result[["d_f"]], a2[["Df"]][2])
})

test_that("LR test accepts a nested design with a different term expression", {
  full <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  red  <- glm(am ~ I(wt), data = mtcars, family = binomial)

  k <- blr_test_lr(full, red)
  a <- anova(red, full, test = "Chisq")

  expect_equal(k$test_result[["lr_ratio"]], a[["Deviance"]][2])
  expect_equal(k$test_result[["d_f"]], a[["Df"]][2])
  expect_equal(k$test_result[["p_value"]], a[["Pr(>Chi)"]][2])
})

test_that("LR test rejects comparisons with no additional parameters", {
  model <- glm(am ~ wt + hp, data = mtcars, family = binomial)

  expect_error(
    blr_test_lr(model, model),
    "nested"
  )
})

test_that("LR nesting check is invariant to predictor scale", {
  for (s in c(1, 1e6, 1e8)) {
    d    <- transform(mtcars, big = hp * s)
    full <- glm(am ~ wt + big, data = d, family = binomial)
    red  <- glm(am ~ wt, data = transform(d, wt = rev(wt)),
                family = binomial)

    expect_error(blr_test_lr(full, red), "nested")
  }
})

