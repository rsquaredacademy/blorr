test_that("output from forward variable selection is as expected", {
  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  actual   <- blr_step_aic_forward(model)$predictors 
  expected <- c("read", "female", "science")

  expect_equal(actual, expected)
})

test_that("output from forward variable p selection is as expected", {
  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  actual   <- blr_step_p_forward(model)$predictors
  expected <- c("read", "female", "science")

  expect_equal(actual, expected)
})

test_that("forward p selection keeps weights and prints the returned model", {
  set.seed(1)
  d <- mtcars
  d$w <- rep(c(1, 2), 16)
  d$noise <- rnorm(32)
  m <- glm(am ~ wt + hp + noise, d, family = binomial, weights = w)

  out <- capture.output(k <- blr_step_p_forward(m, penter = 0.03))

  expected <- glm(am ~ wt + hp, data = d, family = binomial, weights = w)

  expect_equal(k$predictors, c("wt", "hp"))
  expect_equal(unname(coef(k$model)), unname(coef(expected)))
  for (b in coef(k$model)) {
    expect_true(any(grepl(sprintf("%.4f", b), out, fixed = TRUE)))
  }
})

test_that("backward p elimination prints the weighted model it returns", {
  set.seed(1)
  d <- mtcars
  d$w <- rep(c(1, 2), 16)
  d$noise <- rnorm(32)
  m <- glm(am ~ wt + hp + noise, d, family = binomial, weights = w)

  out <- capture.output(k <- blr_step_p_backward(m))

  expected <- glm(reformulate(setdiff(k$indvar, k$removed), response = "am"),
                  data = d, family = binomial, weights = w)
  expect_equal(unname(coef(k$model)), unname(coef(expected)))
  for (b in coef(k$model)) {
    expect_true(any(grepl(sprintf("%.4f", b), out, fixed = TRUE)))
  }
})

test_that("stepwise p selection prints the weighted model it returns", {
  set.seed(1)
  d <- mtcars
  d$w <- rep(c(1, 2), 16)
  d$noise <- rnorm(32)
  m <- glm(am ~ wt + hp + noise, d, family = binomial, weights = w)

  out <- capture.output(k <- blr_step_p_both(m))

  expected <- glm(reformulate(k$predictors, response = "am"), data = d,
                  family = binomial, weights = w)
  expect_equal(unname(coef(k$model)), unname(coef(expected)))
  for (b in coef(k$model)) {
    expect_true(any(grepl(sprintf("%.4f", b), out, fixed = TRUE)))
  }
})

test_that("AIC selection procedures print the weighted model they return", {
  set.seed(1)
  d <- mtcars
  d$w <- rep(c(1, 2), 16)
  d$noise <- rnorm(32)
  m <- glm(am ~ wt + hp + noise, d, family = binomial, weights = w)

  runs <- list(
    function() blr_step_aic_forward(m, progress = TRUE),
    function() blr_step_aic_backward(m, progress = TRUE),
    function() blr_step_aic_both(m, details = TRUE)
  )

  for (run in runs) {
    out <- capture.output(k <- run())
    expected <- glm(formula(k$model), data = d, family = binomial,
                    weights = w)
    expect_equal(unname(coef(k$model)), unname(coef(expected)))
    for (b in coef(k$model)) {
      expect_true(any(grepl(sprintf("%.4f", b), out, fixed = TRUE)))
    }
  }
})

