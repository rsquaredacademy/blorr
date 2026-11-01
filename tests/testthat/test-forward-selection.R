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

