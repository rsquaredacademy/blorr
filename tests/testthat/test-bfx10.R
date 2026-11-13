# grouped binomial data (bfx10): compare with the equivalent Bernoulli fit on
# the expanded data

grouped_fixture <- function(unequal = FALSE) {
  d <- mtcars
  d$s <- rep(1:9, length.out = 32)
  n   <- if (unequal) rep(c(10, 12, 15, 11), length.out = 32) else rep(10, 32)
  d$n <- n
  d$s <- pmin(d$s, d$n)
  d$f <- d$n - d$s
  e   <- d[rep(seq_len(32), times = d$n), ]
  e$y <- unlist(lapply(seq_len(32), function(i) c(rep(1, d$s[i]), rep(0, d$f[i]))))
  list(d = d, e = e,
       g  = glm(cbind(s, f) ~ wt + hp, d, family = binomial),
       p  = glm(I(s / n) ~ wt + hp, d, weights = n, family = binomial),
       be = glm(y ~ wt + hp, e, family = binomial))
}

# every exported function that takes a fitted glm as its first argument
entry_points <- function() {
  ns  <- getNamespaceExports("blorr")
  fns <- vapply(ns, function(n) {
    f <- get(n, envir = asNamespace("blorr"))
    is.function(f) && length(formals(f)) > 0 &&
      names(formals(f))[1] %in% c("model", "full_model", "object") &&
      !(n %in% c("blr_launch_app"))
  }, TRUE)
  sort(ns[fns])
}

unsupported_msg <- "blorr supports unweighted 0/1 [(]Bernoulli[)] models; weighted and grouped binomial models are not supported[.]"
refit_msg <- "refit with glm[(][.][.][.], model = TRUE, y = TRUE[)]"

test_that("every entry point rejects weighted and grouped models (bfx12)", {
  eps <- entry_points()
  expect_gte(length(eps), 50)

  x <- grouped_fixture(TRUE)
  d1 <- transform(mtcars, w = rep(c(1, 9), 16))
  d2 <- transform(mtcars, w = rep(c(1, 9), 16), prop = am)
  d2$w[1] <- 0
  d2$prop[1] <- .5
  bad <- list(
    interior_cbind   = x$g,
    interior_prop    = x$p,
    boundary_cbind   = glm(cbind(am * w, (1 - am) * w) ~ wt + hp, d1,
                           family = binomial),
    case_weights     = glm(am ~ wt + hp, d1, weights = w, family = binomial),
    zero_weight      = glm(am ~ wt + hp, transform(mtcars, w = c(0, rep(1, 31))),
                           weights = w, family = binomial),
    zero_weight_prop = glm(prop ~ wt + hp, d2, weights = w, family = binomial)
  )

  for (nm in names(bad)) {
    for (ep in eps) {
      f <- get(ep, envir = asNamespace("blorr"))
      expect_error(suppressWarnings(f(bad[[nm]])), unsupported_msg,
                   label = paste(ep, nm))
    }
  }

  # a weighted or grouped model in either position of the LR test
  ok <- glm(am ~ wt + hp, mtcars, family = binomial)
  expect_error(blr_test_lr(ok, bad$case_weights), unsupported_msg)
  expect_error(blr_test_lr(bad$case_weights, ok), unsupported_msg)
  expect_error(blr_multi_model_fit_stats(ok, bad$interior_cbind), unsupported_msg)
})

test_that("a fit without its model frame or response is declined (bfx12)", {
  eps <- entry_points()
  d1 <- transform(mtcars, w = rep(c(1, 9), 16))
  no_model <- glm(cbind(am * w, (1 - am) * w) ~ wt + hp, d1, family = binomial,
                  model = FALSE)
  x <- grouped_fixture(TRUE)
  no_y <- glm(I(s / n) ~ wt + hp, x$d, weights = n, family = binomial,
              y = FALSE)
  plain_no_model <- glm(am ~ wt + hp, mtcars, family = binomial, model = FALSE)
  plain_no_y     <- glm(am ~ wt + hp, mtcars, family = binomial, y = FALSE)

  for (m in list(no_model, no_y, plain_no_model, plain_no_y)) {
    for (ep in eps) {
      f <- get(ep, envir = asNamespace("blorr"))
      expect_error(suppressWarnings(f(m)), refit_msg, label = ep)
    }
  }
})

test_that("unweighted Bernoulli models are accepted (bfx12)", {
  m <- glm(honcomp ~ female + read + science, data = hsb2,
           family = binomial(link = "logit"))
  expect_s3_class(blr_model_fit_stats(m), "blr_model_fit_stats")
  auc <- unname((sum(rank(fitted(m))[m$y == 1]) - sum(m$y) * (sum(m$y) + 1) / 2) /
                  (sum(m$y) * sum(m$y == 0)))
  expect_equal(blr_pairs(m)$c, auc, tolerance = 1e-9)
  # explicit unit weights, factor and logical responses, na.exclude
  expect_s3_class(blr_regress(glm(am ~ wt, mtcars, weights = rep(1, 32),
                                  family = binomial)), "blr_regress")
  expect_s3_class(blr_model_fit_stats(glm(factor(am) ~ wt, mtcars,
                                          family = binomial)),
                  "blr_model_fit_stats")
  expect_s3_class(blr_model_fit_stats(glm(am == 1 ~ wt, mtcars,
                                          family = binomial)),
                  "blr_model_fit_stats")
  d <- mtcars
  d$hp[2] <- NA
  expect_s3_class(blr_regress(glm(am ~ wt + hp, d, family = binomial,
                                  na.action = na.exclude)), "blr_regress")
})

test_that("AIC detail tables print the real df of each term (bfx10)", {
  set.seed(1)
  d <- data.frame(y = rbinom(120, 1, .4), x = rnorm(120),
                  g = factor(sample(letters[1:4], 120, TRUE)), z = rnorm(120))
  m <- glm(y ~ x + g + z, d, family = binomial)

  dfs <- function(out) {
    ln <- trimws(out)
    ln <- ln[grepl("^(x|g|z)[[:space:]]+[0-9]+[[:space:]]+[0-9.]+", ln)]
    v  <- vapply(strsplit(ln, "[[:space:]]+"), function(p) as.numeric(p[2]), 0)
    names(v) <- vapply(strsplit(ln, "[[:space:]]+"), `[`, "", 1)
    v
  }

  d1 <- drop1(m)
  out <- capture.output(blr_step_aic_backward(m, details = TRUE))
  first <- dfs(out)[1:3]
  expect_equal(first[["g"]], d1["g", "Df"])
  expect_equal(first[["x"]], d1["x", "Df"])
  expect_equal(first[["z"]], d1["z", "Df"])

  a1 <- add1(glm(y ~ 1, d, family = binomial), ~ x + g + z)
  out <- capture.output(blr_step_aic_forward(m, details = TRUE))
  first <- dfs(out)[1:3]
  expect_equal(first[["g"]], a1["g", "Df"])
  expect_equal(first[["x"]], a1["x", "Df"])

  out <- capture.output(blr_step_aic_both(m, details = TRUE))
  first <- dfs(out)[1:3]
  expect_equal(first[["g"]], a1["g", "Df"])
  expect_equal(first[["z"]], a1["z", "Df"])
})

test_that("zero-parameter model summarizes; aliased p-selection is rejected (bfx10)", {
  m <- glm(am ~ 0, mtcars, family = binomial)
  r <- blr_regress(m)
  expect_equal(length(r$est), 0)
  expect_equal(r$resid_df, 32)
  expect_equal(r$resid_df, df.residual(m))
  r2 <- blr_regress(m, odd_conf_limit = TRUE)
  expect_equal(length(r2$est), 0)
  expect_output(print(r2), "Model Overview")

  set.seed(1)
  d <- transform(mtcars, noise = rnorm(32))
  for (f in c("am ~ I(0*wt) + wt + hp + noise", "am ~ 0 + I(0*wt) + wt + hp + noise")) {
    m <- glm(as.formula(f), d, family = binomial)
    expect_error(blr_step_p_forward(m), "aliased")
    expect_error(blr_step_p_both(m), "aliased")
    expect_error(blr_step_p_backward(m), "aliased")
    # AIC procedures keep working on the same fit
    expect_no_error(capture.output(blr_step_aic_forward(m)))
  }
})
