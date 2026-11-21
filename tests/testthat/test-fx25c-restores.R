# pkg-blorr-fx25c: regression restores for one-class responses and missing
# predictors. Every assertion is a value comparison against base R or against
# the continuity-corrected c848118 WoE/IV result, not a snapshot.

group_factor <- function(x) {
  if (anyNA(x)) addNA(factor(x)) else factor(x)
}

segment_expected <- function(d) {
  g <- group_factor(d$g)
  n <- as.numeric(table(g))
  data.frame(
    level = levels(g),
    n     = n,
    ones  = as.numeric(tapply(d$y, g, sum)),
    share = n / sum(n),
    rate  = as.numeric(tapply(d$y, g, mean)),
    stringsAsFactors = FALSE
  )
}

twoway_expected <- function(d, v1, v2) {
  event <- d$y == 1
  a     <- group_factor(d[[v1]])
  b     <- group_factor(d[[v2]])
  denom <- table(a, b)
  counts <- table(a[event], b[event])
  out    <- counts / denom
  out[denom == 0] <- NA
  out
}

d1 <- data.frame(y = c(0, 1, 0, 1), g = c("a", "a", NA, "b"))
d0 <- data.frame(y = rep(0, 4), g = c("a", "a", "b", "b"))
d2 <- transform(d0, y = 1)

test_that("fx25c: d1 missing predictor matches base-R tapply/table values", {
  d1$h <- c("x", "y", "x", "y")
  expected <- segment_expected(d1)

  seg <- blr_segment(d1, y, g)$segment_data
  expect_equal(as.character(seg$pred), expected$level)
  expect_equal(seg[["1s%"]], expected$rate, tolerance = 1e-12)

  dist <- blr_segment_dist(d1, y, g)$dist_table
  expect_equal(as.character(dist$variable), expected$level)
  expect_equal(dist$n, expected$n)
  expect_equal(dist[["1s"]], expected$ones)
  expect_equal(dist[["n%"]], expected$share, tolerance = 1e-12)
  expect_equal(dist[["1s%"]], expected$rate, tolerance = 1e-12)

  two <- blr_segment_twoway(d1, y, g, h)$twoway_segment
  expect_equal(unname(two), unname(twoway_expected(d1, "g", "h")),
               tolerance = 1e-12)

  # numeric, character and factor predictor encodings agree
  d1_num <- data.frame(y = d1$y, g = c(1, 1, NA, 2))
  d1_fac <- data.frame(y = d1$y, g = factor(d1$g))
  expect_equal(blr_segment(d1_num, y, g)$segment_data[["1s%"]],
               expected$rate, tolerance = 1e-12)
  expect_equal(blr_segment(d1_fac, y, g)$segment_data[["1s%"]],
               expected$rate, tolerance = 1e-12)
})

test_that("fx25c: d1 printers show the NA level without crashing", {
  d1$h <- c("x", "y", "x", "y")

  seg_out <- capture.output(print(blr_segment(d1, y, g)))
  expect_true(any(grepl("\\bNA\\b", seg_out)))

  dist_out <- capture.output(print(blr_segment_dist(d1, y, g)))
  expect_true(any(grepl("\\bNA\\b", dist_out)))

  two_out <- capture.output(print(blr_segment_twoway(d1, y, g, h)))
  expect_true(any(grepl("\\bNA\\b", two_out)))
})

test_that("fx25c: d0 and d2 segment/dist values match base R", {
  for (value in c(0, 1)) {
    d <- if (value == 0) d0 else d2
    expected <- segment_expected(d)

    seg <- blr_segment(d, y, g)$segment_data
    expect_equal(seg[["1s%"]], expected$rate, tolerance = 1e-12)

    dist <- blr_segment_dist(d, y, g)$dist_table
    expect_equal(dist$n, expected$n)
    expect_equal(dist[["1s"]], expected$ones)
    expect_equal(dist[["n%"]], expected$share, tolerance = 1e-12)
    expect_equal(dist[["1s%"]], expected$rate, tolerance = 1e-12)

    two <- blr_segment_twoway(transform(d, h = c("x", "y", "x", "y")),
                              y, g, h)$twoway_segment
    expect_equal(unname(two), unname(twoway_expected(
      transform(d, h = c("x", "y", "x", "y")), "g", "h"
    )), tolerance = 1e-12)
  }
})

test_that("fx25c: numeric, factor and logical one-class encodings agree", {
  g <- c("a", "a", "b", "b")

  encodings <- function(value) {
    list(
      numeric         = rep(value, 4),
      factor          = factor(rep(as.character(value), 4)),
      factor_declared = factor(rep(as.character(value), 4),
                               levels = c("0", "1")),
      logical         = rep(as.logical(value), 4)
    )
  }

  for (value in c(0, 1)) {
    base <- blr_segment(data.frame(y = rep(value, 4), g = g),
                        y, g)$segment_data
    base_woe <- blr_woe_iv(data.frame(y = rep(value, 4), g = g), g, y)

    for (encoding in names(encodings(value))) {
      d <- data.frame(y = encodings(value)[[encoding]], g = g)

      seg <- blr_segment(d, y, g)$segment_data
      expect_equal(seg[["1s%"]], base[["1s%"]])
      expect_equal(seg[["1s%"]], rep(value, 2))

      woe <- blr_woe_iv(d, g, y)
      expect_equal(woe$woe_iv_table, base_woe$woe_iv_table)
      expect_equal(woe$iv_total, 0)
    }
  }
})

test_that("fx25c: d0 and d2 WoE/IV match the c848118 corrected values", {
  for (value in c(0, 1)) {
    d <- if (value == 0) d0 else d2
    k <- blr_woe_iv(d, g, y)
    tab <- k$woe_iv_table

    if (value == 0) {
      expect_equal(tab$count_0s, c(2, 2))
      expect_equal(tab$count_1s, c(0, 0))
    } else {
      expect_equal(tab$count_0s, c(0, 0))
      expect_equal(tab$count_1s, c(2, 2))
    }

    expect_equal(tab$dist_0s, c(0.5, 0.5))
    expect_equal(tab$dist_1s, c(0.5, 0.5))
    expect_equal(tab$woe, c(0, 0))
    expect_equal(tab$iv, c(0, 0))
    expect_equal(k$iv_total, 0)

    out <- capture.output(blr_woe_iv_stats(d, y, g))
    expect_true(any(grepl("Variable: g", out, fixed = TRUE)))
    expect_true(any(grepl("0.50", out, fixed = TRUE)))
  }
})

test_that("fx25c: a one-level factor labelled 1 is all events", {
  g <- c("a", "a", "b", "b")
  d <- data.frame(y = factor(rep("1", 4)), g = g)

  expect_equal(blr_segment(d, y, g)$segment_data[["1s%"]], c(1, 1))
  expect_equal(blr_segment_dist(d, y, g)$dist_table[["1s"]], c(2L, 2L))
  expect_equal(blr_woe_iv(d, g, y)$woe_iv_table$count_1s, c(2, 2))
  expect_equal(blr_woe_iv(d, g, y)$woe_iv_table$count_0s, c(0, 0))

  d$y <- factor(rep("0", 4))
  expect_equal(blr_segment(d, y, g)$segment_data[["1s%"]], c(0, 0))
  expect_equal(blr_woe_iv(d, g, y)$woe_iv_table$count_1s, c(0, 0))
  expect_equal(blr_woe_iv(d, g, y)$woe_iv_table$count_0s, c(2, 2))
})

test_that("fx25c: no observed response level is still an error", {
  empty <- data.frame(y = numeric(0), g = character(0))

  expect_error(
    blr_segment(empty, y, g),
    "`y` has 0 levels; blorr needs both events and non-events.",
    fixed = TRUE
  )
  expect_error(
    blr_woe_iv(empty, g, y),
    "`y` has 0 levels; blorr needs both events and non-events.",
    fixed = TRUE
  )
})
