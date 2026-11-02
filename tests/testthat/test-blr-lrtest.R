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


test_that("LR nesting check survives column norm overflow and underflow", {
  for (s in c(1, 1e155, 1e-200)) {
    # non-nested: a rescaled reversed predictor is not in span(1, wt, hp)
    d       <- transform(mtcars, x = rev(wt) * s)
    full    <- glm(am ~ wt + hp, data = d, family = binomial)
    reduced <- glm(am ~ x, data = d, family = binomial)
    expect_error(blr_test_lr(full, reduced), "nested")

    # nested control at the same scale: x is a rescaled wt
    d2   <- transform(mtcars, x = wt * s)
    f2   <- glm(am ~ x + hp, data = d2, family = binomial)
    r2   <- glm(am ~ x, data = d2, family = binomial)
    k    <- blr_test_lr(f2, r2)
    a    <- anova(r2, f2, test = "Chisq")
    expect_equal(k$test_result[["lr_ratio"]], a[["Deviance"]][2])
    expect_equal(k$test_result[["lr_ratio"]], 9.116974335178096,
                 tolerance = 1e-6)
    expect_equal(k$test_result[["d_f"]], 1)
  }
})

test_that("LR nesting keeps nearly collinear directions the fit estimates (bfx6)", {
  # exact dyadic data: v = 2^27 * (b - a), so am ~ v is nested in am ~ a + b + hp
  set.seed(12)
  d <- transform(mtcars, a = seq_len(32) / 8, v = sample(-16:15) / 8)
  d$b <- d$a + 2^-27 * d$v
  full    <- glm(am ~ a + b + hp, d, family = binomial)
  reduced <- glm(am ~ v, d, family = binomial)
  expect_equal(full$rank, 4)

  k <- blr_test_lr(full, reduced)
  a <- anova(reduced, full, test = "Chisq")
  expect_equal(k$test_result[["lr_ratio"]], a[["Deviance"]][2])
  expect_equal(k$test_result[["lr_ratio"]], 9.7601884568512, tolerance = 1e-6)
  expect_equal(k$test_result[["d_f"]], 2)

  # near-collinear but estimable at several scales
  for (s in c(1, 1e155, 1e-200)) {
    set.seed(12)
    dd   <- mtcars
    b0   <- dd$wt + rnorm(32, sd = 1e-6)
    dd$v <- (b0 - dd$wt) / 1e-6
    dd$a <- dd$wt * s
    dd$b <- b0 * s
    full <- glm(am ~ a + b + hp, dd, family = binomial)
    red  <- glm(am ~ v, dd, family = binomial)
    k    <- blr_test_lr(full, red)
    expect_equal(k$test_result[["lr_ratio"]], 32.80829168332131,
                 tolerance = 1e-6)
    expect_equal(k$test_result[["d_f"]], 2)

    # invalid reversed-predictor control at the same scale still errors
    dr <- transform(mtcars, x = rev(wt) * s)
    expect_error(
      blr_test_lr(glm(am ~ wt + hp, dr, family = binomial),
                  glm(am ~ x, dr, family = binomial)),
      "nested"
    )
  }
})

test_that("LR nesting uses the weighted fit's informative rows (bfx7)", {
  for (s in c(1, 1e155, 1e-200)) {
    d <- mtcars
    d$wt <- d$wt * s
    d$w  <- 1
    d$w[1:8] <- 0
    d$z  <- c(1:8, rep(0, 24))
    full    <- glm(am ~ wt + hp + z, d, family = binomial, weights = w)
    reduced <- glm(am ~ wt, d, family = binomial, weights = w)
    expect_equal(full$rank, 3)

    k <- blr_test_lr(full, reduced)
    a <- anova(reduced, full, test = "Chisq")
    expect_equal(k$test_result[["lr_ratio"]], a[["Deviance"]][2])
    expect_equal(k$test_result[["lr_ratio"]], 8.912477493356727,
                 tolerance = 1e-6)
    expect_equal(k$test_result[["d_f"]], 1)

    fs <- blr_model_fit_stats(full)
    expect_equal(fs$lr_ratio, 26.49604410128582, tolerance = 1e-6)
    expect_equal(fs$lr_df, 2)
    expect_equal(fs$lr_pval, 1.763831633771082e-6, tolerance = 1e-6)

    # invalid control: reversed weight on the informative rows
    d$v <- rev(d$wt)
    expect_error(
      blr_test_lr(full, glm(am ~ v, d, family = binomial, weights = w)),
      "nested"
    )
  }

  # zero-trial groups without a weights argument
  d <- mtcars
  d$s <- d$am
  d$fail <- 1 - d$am
  d$s[1:8] <- 0
  d$fail[1:8] <- 0
  d$z <- c(1:8, rep(0, 24))
  full    <- glm(cbind(s, fail) ~ wt + hp + z, d, family = binomial)
  reduced <- glm(cbind(s, fail) ~ wt, d, family = binomial)
  k <- blr_test_lr(full, reduced)
  a <- anova(reduced, full, test = "Chisq")
  expect_equal(k$test_result[["lr_ratio"]], a[["Deviance"]][2])
  expect_equal(k$test_result[["d_f"]], 1)
})

test_that("LR nesting is invariant to translating predictors (bfx8)", {
  full <- glm(am ~ wt + hp, mtcars, family = binomial)
  ref  <- anova(glm(am ~ wt, mtcars, family = binomial), full,
                test = "Chisq")[["Deviance"]][2]

  for (sh in c(0, 1e6, 1e7, 1e8, 1e9)) {
    # the varying part stays well above glm's own aliasing tolerance
    for (s in c(1, 1e3)) {
      d <- transform(mtcars, z = rev(wt) * s + sh, zv = wt * s + sh)

      # invalid: a translated, rescaled reversed predictor
      expect_error(
        blr_test_lr(full, glm(am ~ z, d, family = binomial)),
        "nested"
      )

      # valid: a translated, rescaled wt is nested in the intercept + wt space
      k <- blr_test_lr(full, glm(am ~ zv, d, family = binomial))
      expect_equal(k$test_result[["lr_ratio"]], ref, tolerance = 1e-6)
      expect_equal(k$test_result[["d_f"]], 1)

      # valid: the translated column in the full model instead (glm itself
      # stops converging for this full fit beyond a shift of 1e8)
      if (sh <= 1e8) {
        fs <- glm(am ~ zv + hp, d, family = binomial)
        k2 <- blr_test_lr(fs, glm(am ~ wt, d, family = binomial))
        expect_equal(k2$test_result[["lr_ratio"]], ref, tolerance = 1e-6)
      }
    }

    # without an intercept, a translated predictor does not span the
    # constant, so the null comparison is unavailable
    d  <- transform(mtcars, zv = wt + sh)
    m0 <- glm(am ~ 0 + zv + hp, d, family = binomial)
    if (sh > 0) {
      expect_warning(fs0 <- blr_model_fit_stats(m0), "not nested")
      expect_true(is.na(fs0$lr_ratio))
    }
  }
})
