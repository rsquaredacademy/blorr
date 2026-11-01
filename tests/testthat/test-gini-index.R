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

test_that("default gini uses the fitted response labels", {
  d  <- transform(mtcars, am = factor(am, labels = c("no", "yes")))
  mf <- glm(am ~ wt + hp, d, family = binomial)

  expect_equal(blr_gini_index(mf), 0.967611336032)
  expect_equal(blr_gini_index(mf), blr_gini_index(mf, d))

  ml <- glm(I(am == 1) ~ wt + hp, mtcars, family = binomial)
  mn <- glm(am ~ wt + hp, mtcars, family = binomial)

  expect_equal(blr_gini_index(ml), 0.967611336032)
  expect_equal(blr_gini_index(ml), blr_gini_index(mn))
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
  # more than 46,340 events: integer products would overflow
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  expect_gt(n1, 46340)
  r   <- rank(p)
  auc <- (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (as.numeric(n1) * n0)
  gini <- blr_gini_index(model)
  expect_true(is.finite(gini))
  expect_equal(gini, 2 * auc - 1)
})

test_that("validation uses the fitted event class when test levels differ", {
  model <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  td    <- transform(mtcars, am = factor(am, levels = c(9, 0, 1)))

  expect_equal(blr_gini_index(model, td), blr_gini_index(model, mtcars))
  expect_equal(sum(blr_gains_table(model, td)$gains_table[["1"]]),
               sum(blr_gains_table(model, mtcars)$gains_table[["1"]]))

  cm <- blr_confusion_matrix(model, data = td)
  expect_equal(dim(cm$conf_matrix), c(2L, 2L))
  expect_equal(sum(cm$conf_matrix), nrow(mtcars))
})

test_that("held-out response values outside the fitted classes are rejected", {
  m  <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  td <- mtcars
  td$am[1] <- 9

  expect_error(blr_gini_index(m, td), "not present in the fitted model")
  expect_error(blr_confusion_matrix(m, data = td),
               "not present in the fitted model")
  expect_error(blr_gains_table(m, td), "not present in the fitted model")
  expect_error(blr_test_hosmer_lemeshow(m, data = td),
               "not present in the fitted model")

  d  <- transform(mtcars, am = factor(am, labels = c("no", "yes")))
  mf <- glm(am ~ wt + hp, data = d, family = binomial)
  tf <- d
  tf$am <- factor(as.character(tf$am), levels = c("no", "yes", "unknown"))
  tf$am[1] <- "unknown"

  expect_error(blr_gini_index(mf, tf), "not present in the fitted model")

  ml <- glm(I(am == 1) ~ wt + hp, data = mtcars, family = binomial)
  tl <- mtcars
  tl[["I(am == 1)"]] <- c(2, mtcars$am[-1] == 1)

  expect_error(blr_gini_index(ml, tl), "not present in the fitted model")
})

test_that("lorenz data uses model defaults and fitted rows", {
  model <- glm(am ~ wt + hp, data = mtcars, family = binomial)

  k <- blr_prep_lorenz_data(model)
  expect_equal(nrow(k), nrow(blr_gains_table(model)$gains_table) + 2L)
  expect_equal(k$cum_1s_per[c(1, nrow(k))], c(0, 1))

  d <- mtcars
  d$hp[1] <- NA
  m <- glm(am ~ wt + hp, data = d, family = binomial)

  k2 <- blr_prep_lorenz_data(m, d, FALSE)
  expect_equal(nrow(k2), nrow(blr_gains_table(m)$gains_table) + 2L)
  expect_equal(k2$cum_1s_per[nrow(k2)], 1)
})


