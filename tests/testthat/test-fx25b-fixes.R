# pkg-blorr-fx25b: regression restores and quick in-domain fixes from the
# rx25b re-audit. Every assertion is a value comparison, not a snapshot.

# M2: the one-class model guard used to reject a degenerate but meaningful
# model. The expected values below are the c848118 output (see the report).
test_that("M2: a one-class model keeps its c848118 confusion matrix", {
  m <- glm(rep(0, 32) ~ wt, mtcars, family = binomial())
  cm <- blr_confusion_matrix(m)

  expected <- matrix(c(32, 0, 0, 0), nrow = 2,
                     dimnames = list(Prediction = c("0", "1"),
                                     Reference = c("0", "1")))
  expect_equal(unclass(cm$conf_matrix), unclass(as.table(expected)))

  expect_equal(cm$accuracy, 1)
  expect_equal(cm$specificity, 1)
  expect_equal(cm$no_information_rate, 1)
  expect_equal(cm$prevalence, 0)
  expect_equal(cm$detection_rate, 0)
  expect_equal(cm$detection_prevalence, 0)
  expect_true(is.na(cm$sensitivity))
  expect_true(is.na(cm$negative_predicted_value))
  expect_true(is.na(cm$positive_predicted_value))
  expect_true(is.na(cm$balanced_accuracy))
  expect_true(is.nan(cm$mcnemar_kappa))
  expect_true(is.na(cm$mcnemar_test_p_val))
  expect_match(cm$mcnemar_note, "not defined")

  out <- capture.output(print(cm))
  expect_true(any(grepl("Accuracy : 1.0000", out, fixed = TRUE)))
  expect_true(any(grepl("Sensitivity : NA", out, fixed = TRUE)))
  expect_true(any(grepl("Specificity : 1.0000", out, fixed = TRUE)))
})

test_that("M2: the other model APIs that accepted a one-class fit still do", {
  m <- glm(rep(0, 32) ~ wt, mtcars, family = binomial())

  expect_true(is.finite(blr_model_fit_stats(m)$mcfadden))
  expect_true(is.nan(blr_rsq_adj_count(m)))

  gains <- blr_gains_table(m)$gains_table
  expect_equal(nrow(gains), 10L)
  expect_equal(sum(gains$total), 32)

  # Degenerate one-class fit: the statistic is 0/0 territory and differs by
  # platform (0 on Windows, NA on Linux/macOS). The regression being guarded
  # is that the call works at all, as it did on c848118.
  hl <- expect_no_error(blr_test_hosmer_lemeshow(m))
  expect_true(is.na(hl$chisq_stat) || hl$chisq_stat == 0)
})

# M4: a predictor value is only required on rows retained after a missing
# response is removed. This worked in c848118 and is the pre-guard behaviour.
test_that("M4: missing responses are removed before the predictor guard", {
  d <- data.frame(y = c(0, 1, 0, 1, NA),
                  g = c("a", "a", "b", "b", NA),
                  h = c("x", "y", "x", "y", NA))

  seg <- blr_segment(d, y, g)$segment_data
  expect_equal(as.character(seg$pred), c("a", "b"))
  expect_equal(seg[["1s%"]], c(0.5, 0.5))

  dist <- blr_segment_dist(d, y, g)$dist_table
  expect_equal(dist$n, c(2, 2))
  expect_equal(dist[["1s"]], c(1, 1))
  expect_equal(dist[["n%"]], c(0.5, 0.5))
  expect_equal(dist[["1s%"]], c(0.5, 0.5))

  two <- blr_segment_twoway(d, y, g, h)$twoway_segment
  expect_equal(unname(two["a", "x"]), 0)
  expect_equal(unname(two["a", "y"]), 1)
  expect_equal(unname(two["b", "x"]), 0)
  expect_equal(unname(two["b", "y"]), 1)

  # a missing predictor on a retained row is its own NA level (fx25c)
  d$g[2] <- NA
  seg_na <- blr_segment(d, y, g)$segment_data
  expect_equal(as.character(seg_na$pred), c("a", "b", NA))
  expect_equal(seg_na[["1s%"]], c(0, 0.5, 1))
})

# H1: an explicit missing factor level is a missing response, not a non-event.
explicit_na_data <- function() {
  data.frame(
    y = factor(c("0", "1", NA, "0", "1"), exclude = NULL),
    g = c("a", "a", "a", "b", "b"),
    h = c("x", "x", "y", "y", "y")
  )
}

test_that("H1: explicit NA factor responses are excluded from segment tables", {
  d <- explicit_na_data()

  seg <- blr_segment(d, y, g)$segment_data
  expect_equal(as.character(seg$pred), c("a", "b"))
  expect_equal(seg[["1s%"]], c(0.5, 0.5))

  dist <- blr_segment_dist(d, y, g)$dist_table
  expect_equal(dist$n, c(2, 2))
  expect_equal(dist[["1s"]], c(1, 1))
  expect_equal(dist[["n%"]], c(0.5, 0.5))
  expect_equal(dist[["1s%"]], c(0.5, 0.5))

  two <- blr_segment_twoway(d, y, g, h)$twoway_segment
  expect_equal(unname(two["a", "x"]), 0.5)
  expect_true(is.na(two["a", "y"]))
  expect_equal(unname(two["b", "y"]), 0.5)
})

test_that("H1: explicit NA factor responses are excluded from WoE/IV", {
  d <- explicit_na_data()

  tab <- blr_woe_iv(d, g, y)$woe_iv_table
  expect_equal(as.character(tab$levels), c("a", "b"))
  expect_equal(tab$count_0s, c(1, 1))
  expect_equal(tab$count_1s, c(1, 1))
  expect_equal(tab$dist_0s, c(0.5, 0.5))
  expect_equal(tab$dist_1s, c(0.5, 0.5))
  expect_equal(tab$woe, c(0, 0))
  expect_equal(tab$iv, c(0, 0))

  out <- capture.output(blr_woe_iv_stats(d, y, g))
  expect_true(any(grepl("Variable: g", out, fixed = TRUE)))
  expect_true(any(grepl("0.50", out, fixed = TRUE)))
})

test_that("H1: explicit NA factor responses are excluded from bivariate analysis", {
  d <- explicit_na_data()

  complete <- d[!is.na(as.character(d$y)), , drop = FALSE]
  complete$y <- droplevels(complete$y)

  expect_equal(
    blr_bivariate_analysis(d, y, g),
    blr_bivariate_analysis(complete, y, g)
  )
})

# H2: the event class is fixed on the full response before missing predictors
# are removed. Factor and numeric 0/1 responses must now agree.
test_that("H2: WoE keeps the factor event class after complete-case filtering", {
  d_factor <- data.frame(y = factor(c(0, 1, 1, 1)), g = c(NA, "a", "a", "b"))
  d_numeric <- data.frame(y = c(0, 1, 1, 1), g = c(NA, "a", "a", "b"))

  kf <- blr_woe_iv(d_factor, g, y)
  kn <- blr_woe_iv(d_numeric, g, y)

  expect_equal(kf$woe_iv_table$count_0s, c(0, 0))
  expect_equal(kf$woe_iv_table$count_1s, c(2, 1))
  expect_equal(kf$woe_iv_table$count_0s, kn$woe_iv_table$count_0s)
  expect_equal(kf$woe_iv_table$count_1s, kn$woe_iv_table$count_1s)
  expect_equal(kf$woe_iv_table$woe, kn$woe_iv_table$woe, tolerance = 1e-12)
  expect_equal(kf$woe_iv_table$iv, kn$woe_iv_table$iv, tolerance = 1e-12)
  expect_equal(kf$iv_total, kn$iv_total, tolerance = 1e-12)

  kf_print <- capture.output(blr_woe_iv_stats(d_factor, y, g))
  kn_print <- capture.output(blr_woe_iv_stats(d_numeric, y, g))
  expect_equal(kf_print, kn_print)
})

test_that("H2: the event mapping survives either missing response class", {
  # only events remain after the missing predictor row is removed
  d_event <- data.frame(y = factor(c(0, 1, 1, 1)), g = c(NA, "a", "a", "b"))
  k_event <- blr_woe_iv(d_event, g, y)$woe_iv_table
  expect_equal(k_event$count_0s, c(0, 0))
  expect_equal(k_event$count_1s, c(2, 1))

  # only non-events remain after the missing predictor row is removed
  d_nonevent <- data.frame(y = factor(c(1, 0, 0, 0)),
                           g = c(NA, "a", "a", "b"))
  k_nonevent <- blr_woe_iv(d_nonevent, g, y)$woe_iv_table
  expect_equal(k_nonevent$count_0s, c(2, 1))
  expect_equal(k_nonevent$count_1s, c(0, 0))
})

# H3: no complete rows is an error, not an empty table with total IV 0.
test_that("H3: no complete WoE rows is a named complete-case error", {
  d <- data.frame(y = c(0, 1), g = c(NA, NA))

  expect_error(
    blr_woe_iv(d, g, y),
    "No complete observations remain",
    fixed = TRUE
  )
  expect_error(
    blr_woe_iv_stats(d, y, g),
    "No complete observations remain",
    fixed = TRUE
  )

  d_factor <- data.frame(y = factor(c("no", "yes")), g = c(NA, NA))
  expect_error(
    blr_woe_iv(d_factor, g, y),
    "No complete observations remain",
    fixed = TRUE
  )
})
