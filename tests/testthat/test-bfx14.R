# EX1: a p-value tie used to add or remove several terms in one step while the
# history recorded a single step. Every step must move exactly one term and the
# AIC history must describe the model after that step.

selection_aic_for <- function(response, terms, data, fit) {
  form <- if (length(terms) == 0) {
    stats::as.formula(paste(response, "~ 1"))
  } else {
    stats::as.formula(paste(response, "~", paste(terms, collapse = " + ")))
  }
  stats::AIC(stats::glm(form, data = data, family = fit$family))
}

test_that("p-both history records one term and one AIC per step (EX1)", {
  m <- glm(y ~ ., data = stepwise, family = binomial(link = "logit"))
  invisible(capture.output(k <- blr_step_p_both(m)))

  expect_equal(k$steps, length(k$orders))
  expect_equal(k$steps, length(k$method))
  expect_equal(k$steps, length(k$aic))
  expect_equal(k$steps, length(k$bic))
  expect_equal(k$steps, length(k$dev))

  terms <- character(0)
  for (i in seq_len(k$steps)) {
    if (k$method[i] == "addition") {
      terms <- c(terms, k$orders[i])
    } else {
      terms <- setdiff(terms, k$orders[i])
    }
    expect_equal(k$aic[i], selection_aic_for("y", terms, stepwise, m),
                 tolerance = 1e-8)
  }

  ref <- glm(y ~ ., data = stepwise[, c("y", k$predictors)],
             family = binomial(link = "logit"))
  expect_equal(coef(k$model), coef(ref))
})

test_that("p-forward history records one term and one AIC per step (EX1)", {
  m <- glm(y ~ ., data = stepwise, family = binomial(link = "logit"))
  invisible(capture.output(k <- blr_step_p_forward(m)))

  expect_equal(k$steps, length(k$predictors))
  expect_equal(k$steps, length(k$aic))
  expect_equal(k$steps, length(k$bic))
  expect_equal(k$steps, length(k$dev))

  for (i in seq_len(k$steps)) {
    expect_equal(k$aic[i],
                 selection_aic_for("y", k$predictors[seq_len(i)], stepwise, m),
                 tolerance = 1e-8)
  }

  ref <- glm(y ~ ., data = stepwise[, c("y", k$predictors)],
             family = binomial(link = "logit"))
  expect_equal(coef(k$model), coef(ref))
})

test_that("AIC selectors break ties by candidate order and still move one term", {
  set.seed(14)
  d <- data.frame(x_a = rnorm(200), noise = rnorm(200))
  d$x_b <- d$x_a
  d$y <- rbinom(200, 1, plogis(2 * d$x_a))

  fwd_model <- glm(y ~ x_a + x_b + noise, data = d, family = binomial)
  invisible(capture.output(fwd <- blr_step_aic_forward(fwd_model)))
  expect_equal(fwd$predictors[1], "x_a")
  expect_equal(fwd$steps, length(fwd$predictors))
  expect_equal(fwd$steps, length(fwd$aics))

  # backward: each x_a/x_b pair is repeated with the two columns swapped and
  # the same response, so dropping either one gives exactly the same AIC while
  # dropping signal carries noise worsens it. Candidate order must pick x_a.
  set.seed(15)
  n <- 100
  db <- data.frame(x_a = rnorm(n), x_b = rnorm(n), noise = rnorm(n))
  db$y <- rbinom(n, 1, plogis(2 * db$noise))
  swapped <- db
  swapped$x_a <- db$x_b
  swapped$x_b <- db$x_a
  db <- rbind(db, swapped)

  bwd_model <- glm(y ~ x_a + x_b + noise, data = db, family = binomial)
  invisible(capture.output(bwd <- blr_step_aic_backward(bwd_model)))
  expect_equal(bwd$predictors[1], "x_a")
  expect_equal(bwd$steps, length(bwd$predictors))
  expect_equal(bwd$steps, nrow(bwd$result) - 1L)
})
