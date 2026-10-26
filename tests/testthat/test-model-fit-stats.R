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


test_that("null model retains weights, offset, and family in blr_model_fit_stats", {
  # weighted model
  set.seed(3)
  w <- sample(1:4, nrow(hsb2), replace = TRUE)
  m_w <- glm(honcomp ~ female + read + science, data = hsb2, family = binomial, weights = w)
  m0_w <- glm(honcomp ~ 1, data = hsb2, family = binomial, weights = w)
  bfs_w <- blr_model_fit_stats(m_w)
  expect_equal(bfs_w$loglik_null, as.numeric(logLik(m0_w)))
  expect_equal(bfs_w$lr_ratio, m0_w$deviance - m_w$deviance)

  # offset model
  set.seed(4)
  off <- rnorm(nrow(hsb2))
  m_off <- glm(honcomp ~ female + read + science, data = hsb2, family = binomial, offset = off)
  m0_off <- glm(honcomp ~ 1, data = hsb2, family = binomial, offset = off)
  bfs_off <- blr_model_fit_stats(m_off)
  expect_equal(bfs_off$loglik_null, as.numeric(logLik(m0_off)))
  expect_equal(bfs_off$lr_ratio, m0_off$deviance - m_off$deviance)

  # probit model
  m_prob <- glm(honcomp ~ female + read + science, data = hsb2, family = binomial(link = "probit"))
  expect_error(blr_model_fit_stats(m_prob), "logit")
  expect_equal(i_model(m_prob)$family$link, "probit")
})

test_that("weighted fit statistics use prior weights and report NA measures", {
  set.seed(3)
  w <- sample(1:4, nrow(hsb2), replace = TRUE)
  m  <- glm(honcomp ~ female + read + science, data = hsb2,
            family = binomial(link = "logit"), weights = w)
  m0 <- glm(honcomp ~ 1, data = hsb2,
            family = binomial(link = "logit"), weights = w)

  n_w <- sum(w)
  ll0 <- as.numeric(logLik(m0))
  ll1 <- as.numeric(logLik(m))

  cox_snell  <- 1 - exp((2 / n_w) * (ll0 - ll1))
  nagelkerke <- cox_snell / (1 - exp((2 / n_w) * ll0))

  res <- blr_model_fit_stats(m)

  expect_equal(res$cox_snell, cox_snell)
  expect_equal(res$nagelkerke, nagelkerke)
  expect_true(is.na(res$count_r2))
  expect_true(is.na(res$count_adj))
  expect_true(is.na(res$effron))
  expect_true(is.na(res$mckelvey))

  out <- capture.output(print(res))
  expect_true(any(grepl(
    "not available for models with prior weights", out, fixed = TRUE
  )))
})

test_that("weighted R2 functions error for models with prior weights", {
  set.seed(3)
  w <- sample(1:4, nrow(hsb2), replace = TRUE)
  m <- glm(honcomp ~ female + read + science, data = hsb2,
           family = binomial(link = "logit"), weights = w)

  expect_error(
    blr_rsq_count(m),
    "Count R2 is not available for models with prior weights", fixed = TRUE
  )
  expect_error(
    blr_rsq_adj_count(m),
    "Adj Count R2 is not available for models with prior weights", fixed = TRUE
  )
  expect_error(
    blr_rsq_effron(m),
    "Efron's R2 is not available for models with prior weights", fixed = TRUE
  )
  expect_error(
    blr_rsq_mckelvey_zavoina(m),
    "McKelvey & Zavoina's R2 is not available for models with prior weights",
    fixed = TRUE
  )
})

test_that("null model uses the model weights when the data has a weights column", {
  set.seed(2)
  dd <- hsb2
  dd$weights <- runif(nrow(dd), 1, 3)
  m  <- glm(honcomp ~ female + read + weights, data = dd, family = binomial)
  m0 <- glm(honcomp ~ 1, data = dd, family = binomial)

  ll1 <- as.numeric(logLik(m))
  ll0 <- as.numeric(logLik(m0))

  res <- blr_model_fit_stats(m)

  expect_equal(res$loglik_null, ll0)
  expect_equal(res$lr_ratio, m0$deviance - m$deviance)
  expect_equal(blr_rsq_mcfadden(m), 1 - (ll1 / ll0))
  expect_equal(blr_rsq_cox_snell(m), 1 - exp((2 / nobs(m)) * (ll0 - ll1)))
  expect_equal(blr_test_lr(m)$test_result$lr_ratio, m0$deviance - m$deviance)
})

test_that("null model uses the model weights and offset when the data has both columns", {
  set.seed(3)
  w   <- sample(1:4, nrow(hsb2), replace = TRUE)
  off <- rnorm(nrow(hsb2))

  dd <- hsb2
  dd$weights <- runif(nrow(dd), 0.5, 2)
  dd$offset  <- rnorm(nrow(dd))

  m  <- glm(honcomp ~ female + read + weights + offset, data = dd,
            family = binomial, weights = w, offset = off)
  m0 <- glm(honcomp ~ 1, data = dd,
            family = binomial, weights = w, offset = off)

  ll1 <- as.numeric(logLik(m))
  ll0 <- as.numeric(logLik(m0))

  res <- blr_model_fit_stats(m)

  expect_equal(res$loglik_null, ll0)
  expect_equal(res$lr_ratio, m0$deviance - m$deviance)
  expect_equal(blr_rsq_mcfadden(m), 1 - (ll1 / ll0))
  expect_equal(
    blr_rsq_cox_snell(m),
    1 - exp((2 / sum(w)) * (ll0 - ll1))
  )
  expect_equal(blr_test_lr(m)$test_result$lr_ratio, m0$deviance - m$deviance)
})
