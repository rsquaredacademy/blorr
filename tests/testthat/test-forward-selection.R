# term labels of a candidate model are taken from the stored model frame, so a
# transformed term such as log(wt) is stored as a quoted column name and the
# label gains backticks; strip them before comparing with the source formula
sel_terms <- function(model) {
  gsub("`", "", attr(stats::terms(model), "term.labels"), fixed = TRUE)
}

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

test_that("p-value selection reevaluates transformed predictors on fitted rows", {
  set.seed(1)
  d <- mtcars
  d$noise <- rnorm(32)
  m <- glm(am ~ log(wt) + hp + noise, data = d, family = binomial)

  expected <- glm(am ~ log(wt) + hp, data = d, family = binomial)

  out <- capture.output(fwd <- blr_step_p_forward(m))
  expect_equal(sel_terms(fwd$model), c("log(wt)", "hp"))
  expect_equal(unname(coef(fwd$model)), unname(coef(expected)))
  expect_equal(fwd$predictors, c("log(wt)", "hp"))

  out <- capture.output(bwd <- blr_step_p_backward(m))
  expect_equal(sel_terms(bwd$model), c("log(wt)", "hp"))
  expect_equal(unname(coef(bwd$model)), unname(coef(expected)))

  out <- capture.output(both <- blr_step_p_both(m))
  expect_equal(sel_terms(both$model), c("log(wt)", "hp"))
  expect_equal(unname(coef(both$model)), unname(coef(expected)))
  expect_equal(both$predictors, c("log(wt)", "hp"))
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

test_that("all six procedures use the stored fit after source-data mutation", {
  set.seed(1)
  d <- mtcars
  d$noise <- rnorm(32)
  m <- glm(am ~ wt + hp + noise, d, family = binomial)
  original  <- d
  reference <- glm(am ~ wt + hp, original, family = binomial)

  procedures <- list(
    p_forward    = function(x) { capture.output(k <- blr_step_p_forward(x)); k },
    p_backward   = function(x) { capture.output(k <- blr_step_p_backward(x)); k },
    p_both       = function(x) { capture.output(k <- blr_step_p_both(x)); k },
    aic_forward  = function(x) { capture.output(k <- blr_step_aic_forward(x)); k },
    aic_backward = function(x) { capture.output(k <- blr_step_aic_backward(x)); k },
    aic_both     = function(x) { capture.output(k <- blr_step_aic_both(x)); k }
  )

  mutations <- list(
    doubled  = function(d) { d$wt <- 2 * d$wt; d },
    reversed = function(d) { d$wt <- rev(d$wt); d }
  )

  for (mutate in mutations) {
    d <- mutate(original)

    for (name in names(procedures)) {
      k <- procedures[[name]](m)

      expect_equal(unname(coef(k$model)), unname(coef(reference)))
      expect_equal(sort(sel_terms(k$model)), c("hp", "wt"))
      expect_equal(predict(k$model, original, type = "response"),
                   reference$fitted.values)
    }
  }
})

test_that("selection evaluates locally defined transformations from the model frame", {
  makefit <- function() {
    log2wt <- function(x) log(x)
    d <- mtcars
    set.seed(1)
    d$noise <- rnorm(32)
    list(
      model     = glm(am ~ log2wt(wt) + hp + noise, d, family = binomial),
      reference = glm(am ~ log2wt(wt) + hp, d, family = binomial)
    )
  }
  fit <- makefit()

  out <- capture.output(k <- blr_step_p_forward(fit$model))

  expect_equal(sel_terms(k$model), c("log2wt(wt)", "hp"))
  expect_equal(unname(coef(k$model)), unname(coef(fit$reference)))
})

test_that("selection works for a no-data fit with locally scoped variables", {
  makefit <- function() {
    set.seed(1)
    y_local     <- mtcars$am
    x_local     <- mtcars$wt
    hp_local    <- mtcars$hp
    noise_local <- rnorm(32)
    list(
      model = glm(y_local ~ log(x_local) + hp_local + noise_local,
                  family = binomial),
      reference = glm(y_local ~ log(x_local) + hp_local, family = binomial)
    )
  }
  fit <- makefit()

  out <- capture.output(k <- blr_step_p_forward(fit$model))

  expect_equal(sel_terms(k$model), c("log(x_local)", "hp_local"))
  expect_equal(unname(coef(k$model)), unname(coef(fit$reference)))
})


selection_procedures <- list(
  p_forward    = function(x) { capture.output(k <- blr_step_p_forward(x)); k },
  p_backward   = function(x) { capture.output(k <- blr_step_p_backward(x)); k },
  p_both       = function(x) { capture.output(k <- blr_step_p_both(x)); k },
  aic_forward  = function(x) { capture.output(k <- blr_step_aic_forward(x)); k },
  aic_backward = function(x) { capture.output(k <- blr_step_aic_backward(x)); k },
  aic_both     = function(x) { capture.output(k <- blr_step_aic_both(x)); k }
)

# the returned model must equal an independent glm on its own formula: same
# coefficients and names, likelihood, and predictions on raw new data
expect_matches_reference <- function(model, data, newdata) {
  ref <- glm(formula(model), data = data, family = family(model))
  expect_equal(coef(model), coef(ref))
  expect_equal(as.numeric(logLik(model)), as.numeric(logLik(ref)))
  expect_equal(
    predict(model, newdata, type = "response"),
    predict(ref, newdata, type = "response")
  )
}

test_that("selected transformed models predict from raw new data (bfx5)", {
  set.seed(1)
  d <- transform(mtcars, noise = rnorm(32))
  m <- glm(am ~ log(wt) + hp + noise, d, family = binomial)
  expected <- glm(am ~ log(wt) + hp, d, family = binomial)

  for (run in selection_procedures) {
    k <- run(m)
    expect_equal(attr(terms(k$model), "term.labels"), c("log(wt)", "hp"))
    expect_equal(unname(coef(k$model)),
                 c(19.76650039746535, -22.66901763616302, 0.0331005632650925),
                 tolerance = 1e-8)
    expect_equal(coef(k$model), coef(expected))
    expect_equal(predict(k$model, mtcars, type = "response"),
                 predict(expected, mtcars, type = "response"))
  }

  m2 <- glm(am ~ I(wt^2) + hp, d, family = binomial)
  capture.output(k2 <- blr_step_aic_backward(m2))
  expect_matches_reference(k2$model, d, mtcars)
})

test_that("selection keeps interaction terms grouped (bfx5)", {
  set.seed(103)
  d <- data.frame(x = rnorm(300), z = rnorm(300), noise = rnorm(300))
  d$y <- rbinom(300, 1, plogis(.2 + 1.2 * d$x + .8 * d$z + .6 * d$x * d$z))
  m <- glm(y ~ x * z + noise, d, family = binomial)

  for (nm in names(selection_procedures)) {
    k <- selection_procedures[[nm]](m)
    expect_matches_reference(k$model, d, d[1:25, ])
  }

  capture.output(k <- blr_step_p_forward(m))
  expect_equal(attr(terms(k$model), "term.labels"), c("x", "z", "noise", "x:z"))
  expect_equal(as.numeric(logLik(k$model)), -155.495848540222, tolerance = 1e-8)

  set.seed(5)
  d5 <- data.frame(x = rnorm(200), g = factor(sample(letters[1:3], 200, TRUE)),
                   w = rnorm(200), noise = rnorm(200))
  d5$y <- rbinom(200, 1, plogis(d5$x + (d5$g == "b") - d5$w^2 / 2))
  m5 <- glm(y ~ g * x + poly(w, 2) + noise, d5, family = binomial)
  for (run in selection_procedures) {
    expect_matches_reference(run(m5)$model, d5, d5[1:15, ])
  }
})

test_that("selection works with non-syntactic predictor names (bfx5)", {
  set.seed(1)
  d <- transform(mtcars, noise = rnorm(32))
  names(d)[names(d) == "wt"] <- "body weight"
  m <- glm(am ~ `body weight` + hp + noise, d, family = binomial)
  expected <- glm(am ~ `body weight` + hp, d, family = binomial)

  for (run in selection_procedures) {
    k <- run(m)
    expect_equal(coef(k$model), coef(expected))
    expect_equal(predict(k$model, d, type = "response"),
                 predict(expected, d, type = "response"))
  }
})

test_that("selected locally transformed models predict from raw data (bfx5)", {
  set.seed(1)
  d  <- transform(mtcars, noise = rnorm(32))
  lt <- function(z) 2 * log(z)
  m  <- glm(am ~ lt(wt) + hp + noise, d, family = binomial)
  expected <- glm(am ~ lt(wt) + hp, d, family = binomial)

  for (run in selection_procedures) {
    k <- run(m)
    expect_equal(coef(k$model), coef(expected))
    expect_equal(predict(k$model, mtcars, type = "response"),
                 predict(expected, mtcars, type = "response"))
  }
})
