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

test_that("grouped binomial models are rejected everywhere (bfx11)", {
  x <- grouped_fixture(TRUE)
  msg <- "blorr supports 0/1 [(]Bernoulli[)] responses; grouped binomial models"
  d0 <- x$d
  d0$prop <- d0$s / d0$n
  prop <- glm(prop ~ wt + hp, d0, weights = n, family = binomial)
  bnd <- transform(d0, bp = ifelse(prop > .5, 1, 0))
  mats <- list(x$g, x$p, prop)
  sel <- list(blr_step_aic_forward, blr_step_aic_backward, blr_step_aic_both,
              blr_step_p_forward, blr_step_p_backward, blr_step_p_both)
  for (m in mats) {
    for (f in list(blr_model_fit_stats, blr_multi_model_fit_stats, blr_pairs,
                   blr_regress, blr_rsq_mcfadden, blr_rsq_mcfadden_adj,
                   blr_rsq_cox_snell, blr_rsq_nagelkerke, blr_rsq_effron,
                   blr_rsq_mckelvey_zavoina, blr_rsq_count,
                   blr_rsq_adj_count, blr_gains_table, blr_gini_index,
                   blr_lorenz_curve, blr_prep_lorenz_data,
                   blr_confusion_matrix, blr_test_hosmer_lemeshow,
                   blr_linktest, blr_test_lr, blr_coll_diag, blr_vif_tol,
                   blr_eigen_cindex, blr_residual_diagnostics,
                   blr_plot_diag_fit, blr_plot_diag_leverage,
                   blr_plot_diag_influence, sel[[1]], sel[[2]], sel[[3]],
                   sel[[4]], sel[[5]], sel[[6]])) {
      expect_error(suppressWarnings(f(m)), msg)
    }
  }
  # a cbind response whose proportions are all 0 or 1 is grouped too
  d1 <- mtcars
  d1$w <- rep(c(1, 9), 16)
  d1$s <- d1$am * d1$w
  d1$f <- (1 - d1$am) * d1$w
  g1 <- glm(cbind(s, f) ~ wt + hp, d1, family = binomial)
  expect_error(blr_pairs(g1), msg)
  expect_error(blr_model_fit_stats(g1), msg)
})

test_that("Bernoulli models, with and without case weights, are unaffected (bfx11)", {
  m <- glm(honcomp ~ female + read + science, data = hsb2,
           family = binomial(link = "logit"))
  expect_s3_class(blr_model_fit_stats(m), "blr_model_fit_stats")
  auc <- unname((sum(rank(fitted(m))[m$y == 1]) - sum(m$y) * (sum(m$y) + 1) / 2) /
                  (sum(m$y) * sum(m$y == 0)))
  expect_equal(blr_pairs(m)$c, auc, tolerance = 1e-9)
  expect_equal(blr_rsq_count(m), mean((fitted(m) >= .5) == m$y))
  mw <- glm(am ~ wt + hp, mtcars, weights = rep(c(1, 2), 16),
            family = binomial)
  expect_s3_class(blr_regress(mw), "blr_regress")
  # factor and logical responses are Bernoulli too
  mf <- glm(factor(am) ~ wt, mtcars, family = binomial)
  expect_s3_class(blr_model_fit_stats(mf), "blr_model_fit_stats")
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
