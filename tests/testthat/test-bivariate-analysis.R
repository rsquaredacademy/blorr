test_that("output from blr_bivariate_analysis is as expected", {
  k <- blr_bivariate_analysis(hsb2, honcomp, female, prog)
  expect_equal(k$iv, c(0.1023, 0.4329))
  expect_equal(round(k$likelihood_ratio, 2), c(3.94, 16.15))
})


test_that("blr_bivariate_analysis prints the correct output", {
  k <- blr_bivariate_analysis(hsb2, honcomp, female, prog)
  expect_snapshot(k)
})

test_that("output from blr_segment is as expected", {
  
  k        <- blr_segment(hsb2, honcomp, race)
  actual   <- rev(k$segment_data[['1s%']])[1]
  expected <- c(0.30)

  expect_equal(actual, expected)
})

test_that("event rate by level is events in level divided by n in level", {
  expected <- round(as.numeric(tapply(hsb2$honcomp == 1, hsb2$race, mean)), 2)

  k <- blr_segment(hsb2, honcomp, race)
  expect_equal(k$segment_data[['1s%']], expected)

  k2 <- blr_segment_dist(hsb2, honcomp, race)
  expect_equal(k2$dist_table[['1s%']], expected)
})

test_that("event rate by level works when a level has no events", {
  d <- hsb2
  d$event <- as.integer(d$honcomp == 1)
  d$event[d$race == "1"] <- 0

  k <- blr_segment(d, event, race)
  expect_equal(k$segment_data[['1s%']][1], 0)

  k2 <- blr_segment_dist(d, event, race)
  expect_equal(k2$dist_table[['1s%']][1], 0)
})

test_that("event rate uses the second level of a factor response with non-0/1 labels", {
  d <- hsb2
  d$hc_lab <- factor(ifelse(d$honcomp == 1, "yes", "no"), levels = c("no", "yes"))
  expected <- round(as.numeric(tapply(d$hc_lab == "yes", d$race, mean)), 2)

  k <- blr_segment(d, hc_lab, race)
  expect_equal(k$segment_data[['1s%']], expected)

  k2 <- blr_segment_dist(d, hc_lab, race)
  expect_equal(k2$dist_table[['1s%']], expected)
})

test_that("event rate is correct when a level has only events (logical response)", {
  d <- data.frame(
    g = c("a", "a", "b", "b", "c", "c"),
    y = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  expected <- c(1, 0.5, 0)

  k <- blr_segment(d, y, g)
  expect_equal(k$segment_data[["1s%"]], expected)

  k2 <- blr_segment_dist(d, y, g)
  expect_equal(k2$dist_table[["1s%"]], expected)
})

test_that("event rate is correct when a level has only events (factor response)", {
  d <- data.frame(
    g = c("a", "a", "b", "b", "c", "c"),
    y = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  d$y_lab <- factor(ifelse(d$y, "yes", "no"), levels = c("no", "yes"))
  expected <- c(1, 0.5, 0)

  k <- blr_segment(d, y_lab, g)
  expect_equal(k$segment_data[["1s%"]], expected)

  k2 <- blr_segment_dist(d, y_lab, g)
  expect_equal(k2$dist_table[["1s%"]], expected)
})

test_that("blr_segment prints the correct output", {
  k <- blr_segment(hsb2, honcomp, race)
  expect_snapshot(k)
})

test_that("output from blr_twoway_segment is as expected", {

  k        <- blr_segment_twoway(hsb2, honcomp, prog, race)
  actual   <- unname(round(k$twoway_segment[, 4], 3))
  expected <- c(0.035, 0.165, 0.020)
  expect_equal(actual, expected)
})

test_that("blr_twoway_segment prints the correct output", {
  k <- blr_segment_twoway(hsb2, honcomp, prog, race)
  expect_snapshot(k)
})

test_that("output from blr_segment_dist is as expected", {
  
  k        <- blr_segment_dist(hsb2, honcomp, race)
  actual   <- rev(k$dist_table[['1s%']])[1]
  expected <- c(0.30)
  expect_equal(actual, expected)
})

test_that("blr_twoway_segment prints the correct output", {
  k <- blr_segment_dist(hsb2, honcomp, race)
  expect_snapshot(k)
})


test_that("output from blr_woe_iv is as expected", {
  k        <- blr_woe_iv(hsb2, prog, honcomp)
  actual   <- sum(k$woe_iv_table[['iv']])
  expected <- 0.4329
  expect_equal(actual, expected)
})

test_that("blr_woe_iv accepts character and numeric predictors", {
  d <- hsb2
  d$race_ch <- as.character(d$race)

  k <- blr_woe_iv(d, race_ch, honcomp)
  expect_equal(nrow(k$woe_iv_table), nlevels(hsb2$race))

  k2 <- blr_woe_iv(hsb2, read, honcomp)
  expect_equal(nrow(k2$woe_iv_table), length(unique(hsb2$read)))
})

test_that("blr_woe_iv drops unused factor levels", {
  d <- subset(hsb2, race != "1")

  k <- blr_woe_iv(d, race, honcomp)

  expect_equal(nrow(k$woe_iv_table), 3L)
  expect_equal(as.character(k$woe_iv_table$levels), c("2", "3", "4"))
  expect_false(any(is.nan(k$woe_iv_table$iv)))
})

test_that("blr_woe_iv applies a continuity correction to zero cells", {
  d <- hsb2
  d$hc2 <- d$honcomp
  d$hc2[d$race == "1"] <- "0"

  k <- blr_woe_iv(d, race, hc2)$woe_iv_table

  expect_equal(k$count_1s[1], 0)
  expect_true(all(is.finite(k$woe)))
  expect_true(all(is.finite(k$iv)))

  event <- d$hc2 == "1"
  no    <- as.numeric(tapply(!event, d$race, sum))
  yes   <- as.numeric(tapply(event, d$race, sum))
  no[1]  <- no[1] + 0.5
  yes[1] <- yes[1] + 0.5

  dist_no  <- no / sum(no)
  dist_yes <- yes / sum(yes)

  expect_equal(k$woe[1], log(dist_no[1] / dist_yes[1]), tolerance = 1e-3)
  expect_equal(k$iv[1], (dist_no[1] - dist_yes[1]) *
    log(dist_no[1] / dist_yes[1]), tolerance = 1e-3)
})

test_that("blr_woe_iv matches hand computation", {
  d <- data.frame(
    g = factor(c("a", "a", "a", "b", "b", "b")),
    y = c(1, 1, 0, 0, 0, 1)
  )

  k <- blr_woe_iv(d, g, y)$woe_iv_table

  no       <- c(1, 2)
  yes      <- c(2, 1)
  dist_no  <- no / sum(no)
  dist_yes <- yes / sum(yes)
  woe      <- log(dist_no / dist_yes)
  iv       <- (dist_no - dist_yes) * woe

  expect_equal(k$count_0s, no)
  expect_equal(k$count_1s, yes)
  expect_equal(k$woe, woe, tolerance = 1e-3)
  expect_equal(k$iv, iv, tolerance = 1e-3)
})

test_that("blr_woe_iv prints the correct output", {
  k <- blr_woe_iv(hsb2, prog, honcomp)
  expect_snapshot(k)
})


test_that("output from blr_woe_iv_stats is as expected", {
  k <- blr_woe_iv_stats(hsb2, honcomp, prog, race, female, schtyp)
  expect_snapshot(k)
})
