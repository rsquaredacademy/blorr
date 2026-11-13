# regression
model <- glm(
  honcomp ~ female + read + science, data = hsb2,
  family = binomial(link = "logit")
)

test_that("model_deviance returns deviance", {
  actual   <- round(model_deviance(model), 4)
  expected <- 160.2364
  expect_equal(actual, expected)
})

test_that("null_ll returns log likelihood of intercept only model", {
  model <- glm(
    vs ~ cyl + disp + hp, data = mtcars,
    family = binomial(link = "logit")
  )
  actual   <- round(null_ll(model), 3)
  expected <- -21.93
  expect_equal(actual, expected)
})

test_that("model_ll returns full model log likelihood", {
  actual   <- round(model_ll(model), 3)
  expected <- -80.118
  expect_equal(actual, expected)
})

test_that("output from model_aic is as expected", {
  actual   <- round(model_aic(model), 4)
  expected <- 168.2364
  expect_equal(actual, expected)
})

test_that("output from model_bic is as expected", {
  actual   <- round(model_bic(model), 4)
  expected <- 181.4296
  expect_equal(actual, expected)
})

test_that("output from blr_mcfadden_rsq is as expected", {
  actual   <- round(blr_rsq_mcfadden(model), 4)
  expected <- 0.3072
  expect_equal(actual, expected)
})

test_that("output from blr_mcfadden_adj_rsq is as expected", {
  actual   <- round(blr_rsq_mcfadden_adj(model), 4)
  expected <- 0.2726
  expect_equal(actual, expected)
})

test_that("output from blr_cox_snell_rsq is as expected", {
  actual   <- round(blr_rsq_cox_snell(model), 4)
  expected <- 0.299
  expect_equal(actual, expected)
})


test_that("output from blr_nagelkerke_rsq is as expected", {
  actual   <- round(blr_rsq_nagelkerke(model), 4)
  expected <- 0.4363
  expect_equal(actual, expected)
})

test_that("output from blr_mckelvey_zavoina_rsq is as expected", {
  actual   <- round(blr_rsq_mckelvey_zavoina(model), 4)
  expected <- 0.5178
  expect_equal(actual, expected)
})

test_that("output from blr_count_rsq is as expected", {
  actual   <- round(blr_rsq_count(model), 4)
  expected <- 0.81
  expect_equal(actual, expected)
})

test_that("output from blr_adj_count_rsq is as expected", {
  actual   <- round(blr_rsq_adj_count(model), 4)
  expected <- 0.283
  expect_equal(actual, expected)
})

test_that("output from blr_effron_rsq is as expected", {
  actual   <- round(blr_rsq_effron(model), 4)
  expected <- 0.3305
  expect_equal(actual, expected)
})

test_that("blr_model_fit_stats prints the correct output", {
  k <- blr_model_fit_stats(model)
  expect_snapshot(k)
})

test_that("blr_multi_model_fit_stats prints the correct output", {
  model1 <- glm(
    honcomp ~ prog + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  k <- blr_multi_model_fit_stats(model, model1)
  expect_snapshot(k)
})


test_that("blr_lr_test prints the correct output", {
  k <- blr_test_lr(model)
  expect_snapshot(k)
})

test_that("pseudo R2 uses the number of observations used to fit the model", {
  d <- hsb2
  d$read[which(d$honcomp == "0")[1:10]] <- NA

  m   <- glm(honcomp ~ female + read + science, data = d,
             family = binomial(link = "logit"))
  n   <- nobs(m)
  ll0 <- as.numeric(logLik(glm(honcomp ~ 1, data = model.frame(m),
                               family = binomial(link = "logit"))))
  ll1 <- as.numeric(logLik(m))

  cox_snell  <- 1 - exp((2 / n) * (ll0 - ll1))
  nagelkerke <- cox_snell / (1 - exp((2 / n) * ll0))

  expect_equal(blr_rsq_cox_snell(m), cox_snell)
  expect_equal(blr_rsq_nagelkerke(m), nagelkerke)
})

test_that("pseudo R2 and printed fit stats work without a data argument", {
  honcomp <- hsb2$honcomp
  female  <- hsb2$female
  read    <- hsb2$read
  science <- hsb2$science

  m   <- glm(honcomp ~ female + read + science,
             family = binomial(link = "logit"))
  n   <- nobs(m)
  ll0 <- as.numeric(logLik(glm(honcomp ~ 1, family = binomial(link = "logit"))))
  ll1 <- as.numeric(logLik(m))

  expect_equal(blr_rsq_cox_snell(m), 1 - exp((2 / n) * (ll0 - ll1)))
  expect_length(blr_rsq_nagelkerke(m), 1)

  out      <- capture.output(print(blr_model_fit_stats(m)))
  bic_line <- out[grepl("BIC:", out, fixed = TRUE)]

  expect_equal(length(bic_line), 1)
  expect_match(bic_line, format(round(BIC(m), 3), nsmall = 3), fixed = TRUE)
})

test_that("multi model fit stats label each measure by name", {
  model1 <- glm(honcomp ~ prog + read + science, data = hsb2,
                family = binomial(link = "logit"))
  model2 <- glm(honcomp ~ female + read + math, data = hsb2,
                family = binomial(link = "logit"))

  out <- capture.output(print(blr_multi_model_fit_stats(model1, model2)))

  value_of <- function(label) {
    line <- out[grepl(label, out, fixed = TRUE)]
    expect_length(line, 1)
    as.numeric(tail(strsplit(trimws(line), "[[:space:]]+")[[1]], 1))
  }

  expect_equal(value_of("ML (Cox-Snell) R2"),
               round(blr_rsq_cox_snell(model2), 3))
  expect_equal(value_of("Cragg-Uhler(Nagelkerke) R2"),
               round(blr_rsq_nagelkerke(model2), 3))
  expect_equal(value_of("McKelvey & Zavoina's R2"),
               round(blr_rsq_mckelvey_zavoina(model2), 3))
  expect_equal(value_of("Efron's R2"), round(blr_rsq_effron(model2), 3))
  expect_equal(value_of("AIC"), round(AIC(model2), 3))
  expect_equal(value_of("BIC"), round(BIC(model2), 3))
})

test_that("weighted fits keep their weights in LR tests and pseudo R2", {
  d <- mtcars
  d$w <- rep(c(1, 2), 16)
  model <- glm(am ~ wt + hp, data = d, family = binomial, weights = w)
  null  <- update(model, . ~ 1)

  lr <- blr_test_lr(model)$test_result
  a  <- anova(null, model, test = "Chisq")

  expect_equal(lr$lr_ratio, a[["Deviance"]][2])
  expect_equal(lr$d_f, a[["Df"]][2])
  expect_equal(lr$p_value, a[["Pr(>Chi)"]][2])

  cox_snell <- 1 - exp(2 * (as.numeric(logLik(null)) -
                              as.numeric(logLik(model))) / nobs(model))
  nagelkerke <- cox_snell / (1 - exp(2 * as.numeric(logLik(null)) / nobs(model)))

  expect_equal(blr_rsq_cox_snell(model), cox_snell)
  expect_equal(blr_rsq_nagelkerke(model), nagelkerke)
})

test_that("blr_regress summarises the supplied weighted fit", {
  d <- mtcars
  d$w <- rep(c(1, 2), 16)
  model <- glm(am ~ wt + hp, data = d, family = binomial, weights = w)

  k <- blr_regress(model)

  expect_equal(k$est, unname(coef(model)))
  expect_equal(k$se, unname(summary(model)$coefficients[, "Std. Error"]))
  expect_equal(k$n_obs, unname(nobs(model)))
})

test_that("pseudo R2 is identical for na.omit and na.exclude fits", {
  d <- mtcars
  d$hp[1] <- NA
  me <- glm(am ~ wt + hp, data = d, family = binomial,
            na.action = na.exclude)
  mo <- glm(am ~ wt + hp, data = d, family = binomial,
            na.action = na.omit)

  r2 <- function(m) {
    c(blr_rsq_effron(m), blr_rsq_count(m), blr_rsq_adj_count(m),
      blr_rsq_mckelvey_zavoina(m))
  }

  expect_equal(r2(me), r2(mo))
  expect_false(anyNA(r2(me)))

  k <- blr_model_fit_stats(me)
  expect_false(anyNA(c(k$effron, k$count_r2, k$count_adj, k$mckelvey)))
})

test_that("fit statistics are available for an intercept-only model", {
  model <- glm(am ~ 1, data = mtcars, family = binomial)

  k <- blr_model_fit_stats(model)

  expect_equal(k$lr_ratio, 0)
  expect_equal(k$lr_df, 0)
  expect_equal(k$dev_df, df.residual(model))
})

test_that("a rank-one model without an intercept has no zero LR test", {
  model <- glm(am ~ 0 + wt, data = mtcars, family = binomial)

  expect_warning(
    k <- blr_model_fit_stats(model),
    "likelihood ratio test is not available"
  )

  # the fitted and intercept-only likelihoods differ, so reporting a zero
  # statistic would be wrong
  expect_false(isTRUE(all.equal(k$loglik_null, k$loglik_model)))
  expect_false(isTRUE(all.equal(
    -2 * (k$loglik_null - k$loglik_model), 0
  )))
  expect_true(is.na(k$lr_ratio))
  expect_true(is.na(k$lr_df))
  expect_true(is.na(k$lr_pval))
  expect_equal(k$dev_df, df.residual(model))

  # the same applies when more than one predictor is fitted without an
  # intercept: the intercept-only model is not nested in the fitted design
  model2 <- glm(am ~ 0 + wt + hp, data = mtcars, family = binomial)

  expect_warning(
    k2 <- blr_model_fit_stats(model2),
    "likelihood ratio test is not available"
  )
  expect_true(is.na(k2$lr_ratio))
  expect_equal(k2$dev_df, df.residual(model2))
})

test_that("residual df uses the fitted rank when a coefficient is aliased", {
  model <- glm(am ~ wt + I(2 * wt), data = mtcars, family = binomial)

  k <- blr_model_fit_stats(model)

  expect_equal(k$dev_df, df.residual(model))
  expect_equal(model_d_f(model), model$rank)
})

test_that("no-intercept fit statistics do not depend on predictor scale", {
  ref_ll <- NULL

  for (s in c(1, 1e6, 1e8)) {
    d     <- transform(mtcars, x = wt * s)
    model <- glm(am ~ 0 + x, data = d, family = binomial)

    expect_warning(
      k <- blr_model_fit_stats(model),
      "likelihood ratio test is not available"
    )

    # the fitted and intercept-only likelihoods match base R, differ from each
    # other, and are unchanged by rescaling the predictor
    expect_equal(k$loglik_null, as.numeric(logLik(update(model, . ~ 1))))
    expect_equal(k$loglik_model, as.numeric(logLik(model)))
    expect_true(is.na(k$lr_ratio))
    expect_true(is.na(k$lr_df))
    expect_true(is.na(k$lr_pval))
    expect_equal(k$dev_df, df.residual(model))
    expect_false(isTRUE(all.equal(k$loglik_null, k$loglik_model)))

    if (is.null(ref_ll)) {
      ref_ll <- c(k$loglik_null, k$loglik_model)
    } else {
      expect_equal(c(k$loglik_null, k$loglik_model), ref_ll)
    }
  }
})

