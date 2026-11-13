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

test_that("grouped binomial association, count and Efron/McKelvey measures match the expanded fit (bfx10)", {
  for (un in c(FALSE, TRUE)) {
    x <- grouped_fixture(un)
    expect_equal(unname(coef(x$g)), unname(coef(x$be)), tolerance = 1e-6)

    for (m in list(x$g, x$p)) {
      expect_equal(blr_pairs(m), blr_pairs(x$be), tolerance = 1e-6)
      expect_equal(blr_rsq_count(m), blr_rsq_count(x$be), tolerance = 1e-6)
      expect_equal(blr_rsq_adj_count(m), blr_rsq_adj_count(x$be),
                   tolerance = 1e-6)
      expect_equal(blr_rsq_effron(m), blr_rsq_effron(x$be), tolerance = 1e-6)
      expect_equal(blr_rsq_mcfadden(m), blr_rsq_mcfadden(x$be), tolerance = 1e-6)
      expect_equal(blr_rsq_mcfadden_adj(m), blr_rsq_mcfadden_adj(x$be),
                   tolerance = 1e-6)
      expect_equal(blr_rsq_cox_snell(m), blr_rsq_cox_snell(x$be),
                   tolerance = 1e-6)
      expect_equal(blr_rsq_nagelkerke(m), blr_rsq_nagelkerke(x$be),
                   tolerance = 1e-6)
      expect_equal(blr_rsq_mckelvey_zavoina(m),
                   blr_rsq_mckelvey_zavoina(x$be), tolerance = 1e-6)
    }
  }

  # the full fit-statistics table equals the expanded fit, field by field
  x <- grouped_fixture(TRUE)
  for (m in list(x$g, x$p)) {
    a <- blr_model_fit_stats(m)
    b <- blr_model_fit_stats(x$be)
    for (nm in names(b)) {
      expect_equal(a[[nm]], b[[nm]], tolerance = 1e-6, label = nm)
    }
  }
  expect_s3_class(blr_multi_model_fit_stats(x$g, x$p), "blr_multi_model_fit_stats")

  x <- grouped_fixture()
  expect_equal(blr_pairs(x$g)$pairs, sum(x$d$s) * sum(x$d$f) - 0 -
                 sum(0 * x$d$s))
  expect_equal(blr_pairs(x$g)$c, 0.595098039216, tolerance = 1e-9)
  expect_equal(blr_rsq_count(x$g), 0.5625)
  expect_equal(blr_rsq_adj_count(x$g), 0.0666666666667, tolerance = 1e-9)
})

test_that("blr_regress summaries use trials for grouped data (bfx10)", {
  x <- grouped_fixture()
  r <- blr_regress(x$g)
  expect_equal(unname(unlist(r$resp_prof)), c(sum(x$d$f), sum(x$d$s)))
  expect_equal(r$resp_prof[["1"]], 150)
  expect_equal(r$resp_prof[["0"]], 170)
  expect_equal(r$resp_name, "cbind(s, f)")
  expect_equal(length(r$resp_name), 1)
  expect_equal(r$blr_cord, blr_pairs(x$be), tolerance = 1e-6, ignore_attr = TRUE)
  out <- capture.output(print(r))
  expect_true(any(grepl("cbind(s, f)", out, fixed = TRUE)))
})

test_that("functions that assume 0/1 rows stop on grouped binomial data (bfx10)", {
  x <- grouped_fixture()
  for (m in list(x$g, x$p)) {
    expect_error(blr_gains_table(m), "grouped binomial")
    expect_error(blr_gini_index(m), "grouped binomial")
    expect_error(blr_prep_lorenz_data(m), "grouped binomial")
    expect_error(blr_confusion_matrix(m), "grouped binomial")
    expect_error(blr_test_hosmer_lemeshow(m), "grouped binomial")
    expect_error(blr_linktest(m), "grouped binomial")
  }
  # ordinary Bernoulli models are unaffected
  expect_s3_class(blr_model_fit_stats(x$be), "blr_model_fit_stats")
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
