test_that("output from blr_gini_index is 2 * AUC - 1", {
  model <- glm(
    honcomp ~ female + read + science, data = blorr::hsb2,
    family = binomial(link = "logit")
  )

  prob  <- fitted(model)
  event <- model$y == 1
  auc   <- mean(outer(prob[event], prob[!event], ">") +
    0.5 * outer(prob[event], prob[!event], "=="))

  expect_equal(blr_gini_index(model), 2 * auc - 1)
})

test_that("gini index and lorenz curve use the rows the model was fitted on", {
  d <- hsb2
  d$read[which(d$honcomp == "0")[1:10]] <- NA

  model <- glm(
    honcomp ~ female + read, data = d,
    family = binomial(link = "logit")
  )

  prob  <- fitted(model)
  event <- model$y == 1
  auc   <- mean(outer(prob[event], prob[!event], ">") +
    0.5 * outer(prob[event], prob[!event], "=="))

  expect_equal(nobs(model), 190)
  expect_equal(blr_gini_index(model), 2 * auc - 1)

  lorenz <- blr_lorenz_curve(model, print_plot = FALSE)
  expect_equal(nrow(lorenz$data), nrow(blr_gains_table(model)$gains_table) + 2L)
  expect_equal(
    lorenz$labels$subtitle,
    paste0("Gini Index = ", round(2 * auc - 1, 2))
  )
})

test_that("gini index on a test set uses the response from the test data", {
  model <- glm(
    honcomp ~ female + read, data = hsb2,
    family = binomial(link = "logit")
  )

  test_data <- hsb2[1:50, ]
  prob      <- predict(model, newdata = test_data, type = "response")
  event     <- test_data$honcomp == 1
  auc       <- mean(outer(prob[event], prob[!event], ">") +
    0.5 * outer(prob[event], prob[!event], "=="))

  expect_equal(blr_gini_index(model, test_data), 2 * auc - 1)
})

test_that("gini index errors when data lacks the response column", {
  model <- glm(
    honcomp ~ female + read, data = hsb2,
    family = binomial(link = "logit")
  )

  expect_error(
    blr_gini_index(model, hsb2[1:50, c("female", "read")]),
    "must contain the response variable"
  )
})

test_that("gini index is fast on a large model", {
  skip_on_cran()

  set.seed(1)
  n <- 100000
  d <- data.frame(
    y  = rbinom(n, 1, 0.1),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )

  model <- glm(y ~ x1 + x2, data = d, family = binomial(link = "logit"))

  elapsed <- system.time(blr_gini_index(model))[["elapsed"]]
  expect_lt(elapsed, 5)
})

test_that("AUC on complete rows matches the concordance statistic", {
  d <- hsb2
  d$read[which(d$honcomp == "0")[1:10]] <- NA

  model <- glm(
    honcomp ~ female + read, data = d,
    family = binomial(link = "logit")
  )

  prob <- fitted(model)
  y    <- model$y
  auc  <- mean(outer(prob[y == 1], prob[y == 0], ">") +
    0.5 * outer(prob[y == 1], prob[y == 0], "=="))

  pairs <- blr_pairs(model)

  expect_equal(pairs$c, auc)
  expect_equal(pairs$somers_d, 2 * auc - 1)
})

test_that("gini index is finite on a large balanced model", {
  skip_on_cran()
  set.seed(2)
  n <- 100000
  d <- data.frame(y = rbinom(n, 1, 0.5), x1 = rnorm(n), x2 = rnorm(n))
  model <- glm(y ~ x1 + x2, data = d, family = binomial)
  y  <- model$y
  p  <- predict(model, newdata = model$model, type = "response")
  n1 <- as.numeric(sum(y == 1))
  n0 <- as.numeric(sum(y == 0))
  w  <- suppressWarnings(wilcox.test(p[y == 1], p[y == 0], exact = FALSE))$statistic
  expect_equal(blr_gini_index(model), 2 * as.numeric(w) / (n1 * n0) - 1)
})
