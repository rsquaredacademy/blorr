test_that("output from blr_gini_index is as expected", {
  model <- glm(
    honcomp ~ female + read + science, data = blorr::hsb2,
    family = binomial(link = "logit")
  )

  actual <- blr_gini_index(model)
  expected <- ineq::Gini(model$fitted.values)
  expect_equal(actual, expected)
})

test_that("gini index and lorenz curve use the rows the model was fitted on", {
  d <- hsb2
  d$read[which(d$honcomp == "0")[1:10]] <- NA

  model <- glm(
    honcomp ~ female + read, data = d,
    family = binomial(link = "logit")
  )

  prob <- fitted(model)
  n    <- length(prob)

  expected <- (((sum(sort(prob) * seq_len(n)) / sum(prob)) * 2) -
    (n + 1)) / n

  expect_equal(nobs(model), 190)
  expect_equal(blr_gini_index(model), expected)

  lorenz <- blr_lorenz_curve(model, print_plot = FALSE)
  expect_equal(nrow(lorenz$data), nrow(blr_gains_table(model)$gains_table) + 2L)
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
