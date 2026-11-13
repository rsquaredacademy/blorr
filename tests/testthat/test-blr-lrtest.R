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

test_that("LR test stops when nesting is not structural: contrasts, I(wt) (bfx9)", {
  # nesting is by name: equivalent but differently named columns are not
  # recognised, so the test stops instead of inferring
  d    <- transform(mtcars, g = factor(cyl))
  full <- glm(am ~ g + hp, data = d, family = binomial,
              contrasts = list(g = "contr.treatment"))
  red  <- glm(am ~ g, data = d, family = binomial,
              contrasts = list(g = "contr.sum"))
  expect_error(blr_test_lr(full, red), "nested")

  full1 <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  red1  <- glm(am ~ I(wt), data = mtcars, family = binomial)
  expect_error(blr_test_lr(full1, red1), "nested")

  # same contrasts and same names: accepted, value equals anova
  red2 <- glm(am ~ g, data = d, family = binomial,
              contrasts = list(g = "contr.treatment"))
  k <- blr_test_lr(full, red2)
  a <- anova(red2, full, test = "Chisq")
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

test_that("LR nesting is structural for near-collinear designs (bfx9)", {
  # exact dyadic data: v = 2^27 * (b - a) is a hidden relationship that the
  # structural rule does not look for: am ~ v is declined, not tested
  set.seed(12)
  d <- transform(mtcars, a = seq_len(32) / 8, v = sample(-16:15) / 8)
  d$b <- d$a + 2^-27 * d$v
  full <- glm(am ~ a + b + hp, d, family = binomial)
  expect_error(blr_test_lr(full, glm(am ~ v, d, family = binomial)), "nested")

  # but a subset of the full model's own columns is accepted, at any scale
  for (s in c(1, 1e155, 1e-200)) {
    set.seed(12)
    dd   <- mtcars
    dd$a <- dd$wt * s
    dd$b <- (dd$wt + rnorm(32, sd = 1e-6)) * s
    full <- glm(am ~ a + b + hp, dd, family = binomial)
    red  <- glm(am ~ a + b, dd, family = binomial)
    k    <- blr_test_lr(full, red)
    a    <- anova(red, full, test = "Chisq")
    expect_equal(k$test_result[["lr_ratio"]], a[["Deviance"]][2])
    expect_equal(k$test_result[["d_f"]], 1)
  }
})

test_that("LR nesting works on translated predictors by name (bfx8, bfx9)", {
  for (sh in c(0, 1e6, 1e8)) {
    for (s in c(1, 1e3)) {
      d <- transform(mtcars, z = rev(wt) * s + sh, zv = wt * s + sh)
      full <- glm(am ~ zv + hp, d, family = binomial)

      # a different predictor is never nested
      expect_error(blr_test_lr(full, glm(am ~ z, d, family = binomial)),
                   "nested")

      # a subset of the full model's own columns is nested
      red <- glm(am ~ zv, d, family = binomial)
      k   <- blr_test_lr(full, red)
      a   <- anova(red, full, test = "Chisq")
      expect_equal(k$test_result[["lr_ratio"]], a[["Deviance"]][2],
                   tolerance = 1e-6)
      expect_equal(k$test_result[["d_f"]], 1)
    }

    # without an intercept the null comparison is unavailable
    d  <- transform(mtcars, zv = wt + sh)
    m0 <- glm(am ~ 0 + zv + hp, d, family = binomial)
    if (sh > 0) {
      expect_warning(fs0 <- blr_model_fit_stats(m0), "not nested")
      expect_true(is.na(fs0$lr_ratio))
    }
  }
})

test_that("BFXX8-1 reproductions stop; nested controls are accepted (bfx9)", {
  set.seed(1219)
  n <- 256
  d <- data.frame(a = seq_len(n)/8, v = sample(-128:127)/8,
                  u = sample(-128:127)/8, h = rnorm(n))
  d$y <- rbinom(n, 1, plogis(-.5 + .02*d$a + .1*d$v + .3*d$h))

  for (pow in c(27, 33)) {
    delta <- if (pow == 27) 1e-5 else .001
    d$b <- d$a + 2^-pow*d$v
    d$z <- d$v + delta*d$u
    full    <- glm(y ~ a + b + h, d, family = binomial)
    reduced <- glm(y ~ z, d, family = binomial)
    stable  <- glm(y ~ a + v + h, d, family = binomial)

    expect_error(blr_test_lr(full, reduced), "nested")
    expect_error(blr_test_lr(stable, reduced), "nested")

    # delta = 0 control: v itself is not a column of the full model either,
    # so it is declined; a subset of its columns is accepted and exact
    expect_error(blr_test_lr(full, glm(y ~ v, d, family = binomial)),
                 "nested")
    red <- glm(y ~ a + b, d, family = binomial)
    k   <- blr_test_lr(full, red)
    an  <- anova(red, full, test = "Chisq")
    expect_equal(k$test_result[["lr_ratio"]], an[["Deviance"]][2])
    expect_equal(k$test_result[["d_f"]], 1)
  }

  # scale controls on an orthogonal-style design
  set.seed(888)
  e <- data.frame(a = rnorm(n), v = rnorm(n), u = rnorm(n))
  e$y <- rbinom(n, 1, plogis(.3*e$a + .3*e$v))
  for (s in c(1, 1e155, 1e-200)) {
    e$b <- (e$a + 1e-9*e$v) * s
    e$z <- (e$v + 1e-5*e$u) * s
    e$a2 <- e$a * s
    full <- glm(y ~ a2 + b, e, family = binomial)
    expect_error(blr_test_lr(full, glm(y ~ z, e, family = binomial)),
                 "nested")
  }
})

test_that("same-named but unequal columns are rejected, with no tolerance (bfx11)", {
  # small units: wt * 1e-200 reversed in the reduced model
  d <- transform(mtcars, x = wt * 1e-200)
  full <- glm(am ~ x + hp, d, family = binomial)
  red  <- glm(am ~ x, transform(d, x = rev(x)), family = binomial)
  expect_error(blr_test_lr(full, red), "nested")

  # at other scales too
  for (s in c(1, 1e-10, 1e-100, 1e150)) {
    d <- transform(mtcars, x = wt * s)
    expect_error(
      blr_test_lr(glm(am ~ x + hp, d, family = binomial),
                  glm(am ~ x, transform(d, x = rev(x)), family = binomial)),
      "nested")
  }

  # exact dyadic case: same names, columns differ by a tiny relative amount
  set.seed(1219)
  n <- 256
  d <- data.frame(a = seq_len(n)/8, v = sample(-128:127)/8,
                  u = sample(-128:127)/8, h = rnorm(n))
  d$y <- rbinom(n, 1, plogis(-.5 + .02*d$a + .3*d$u))
  d$b <- d$a + 2^-27*d$v
  rd  <- d
  rd$b <- rd$a + 2^-27*rd$u
  expect_error(blr_test_lr(glm(y ~ a + b + h, d, family = binomial),
                           glm(y ~ a + b, rd, family = binomial)), "nested")

  # a one-ulp change in one value is not nested either
  m <- mtcars
  m2 <- m
  m2$wt[3] <- m2$wt[3] * (1 + 1e-15)
  expect_error(blr_test_lr(glm(am ~ wt + hp, m, family = binomial),
                           glm(am ~ wt, m2, family = binomial)), "nested")
})

test_that("ordinary nested comparisons still run and match anova (bfx11)", {
  # non-separating simulated data, so no glm warnings
  set.seed(11)
  n <- 300
  d <- data.frame(x = rnorm(n), z = rnorm(n),
                  g = factor(sample(c("a", "b", "c"), n, TRUE)),
                  off = rnorm(n, 0, .3))
  d$y <- rbinom(n, 1, plogis(.2 + .5 * d$x - .4 * d$z + .3 * (d$g == "b") +
                               d$off))
  cases <- list(
    list(y ~ x + z, y ~ x),
    list(y ~ g + x, y ~ g),
    list(y ~ x * z, y ~ x + z),
    list(y ~ g * x, y ~ g + x),
    list(y ~ x + z + offset(off), y ~ x + offset(off))
  )
  for (cs in cases) {
    full <- glm(cs[[1]], d, family = binomial)
    red  <- glm(cs[[2]], d, family = binomial)
    k <- blr_test_lr(full, red)
    a <- anova(red, full, test = "Chisq")
    expect_equal(k$test_result[["lr_ratio"]], a[["Deviance"]][2])
    expect_equal(k$test_result[["d_f"]], a[["Df"]][2])
    expect_equal(k$test_result[["p_value"]], a[["Pr(>Chi)"]][2])
  }

  # argument offset, absent in the reduced model: not comparable
  fo <- glm(y ~ x + z, d, family = binomial, offset = off)
  expect_error(blr_test_lr(fo, glm(y ~ x, d, family = binomial)), "offset")
})
