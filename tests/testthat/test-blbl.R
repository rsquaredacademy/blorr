# M5: association cells are proportions but were labelled as percentages.
# M7: segment printers showed factor codes and the wrong headers.

assoc_line <- function(out, label) {
  grep(label, out, value = TRUE)[1]
}

test_that("association percentages are printed on the percent scale (M5)", {
  m <- glm(am ~ wt + hp, data = mtcars, family = binomial())
  k <- blr_regress(am ~ wt + hp, data = mtcars)
  out <- capture.output(print(k))

  # independent pair counts from the fitted probabilities
  p <- fitted(m)
  y <- mtcars$am
  pos <- p[y == 1]
  neg <- p[y == 0]
  cmp <- outer(pos, neg, "-")

  expect_equal(k$blr_cord[["pairs"]], length(pos) * length(neg))
  expect_equal(k$blr_cord[["concordance"]],
               sum(cmp > 0) / (length(pos) * length(neg)))
  expect_equal(k$blr_cord[["discordance"]],
               sum(cmp < 0) / (length(pos) * length(neg)))
  expect_equal(k$blr_cord[["tied"]],
               sum(cmp == 0) / (length(pos) * length(neg)))

  # the returned proportions are unchanged
  expect_equal(k$blr_cord[["concordance"]], 243 / 247)
  expect_equal(k$blr_cord[["discordance"]], 4 / 247)

  # printed cells keep the % labels but show 0-100 values
  expect_match(assoc_line(out, "% Concordant"), "98\\.3806")
  expect_match(assoc_line(out, "% Discordant"), "1\\.6194")
  expect_match(assoc_line(out, "% Tied"), "0\\.0000")
  expect_false(grepl("0\\.9838", assoc_line(out, "% Concordant")))
  expect_false(grepl("0\\.0162", assoc_line(out, "% Discordant")))
})

test_that("segment distribution prints levels, predictor name and percent cells (M5, M7)", {
  d <- as.data.frame(descriptr::mtcarz)
  k <- blr_segment_dist(d, am, cyl)

  # factor levels, not their integer codes
  expect_equal(k$dist_table$variable, factor(c("4", "6", "8")))
  expect_equal(as.character(k$dist_table$variable), c("4", "6", "8"))
  expect_equal(k$dist_table$n, c(11L, 7L, 14L))
  expect_equal(k$dist_table$`1s`, c(8L, 3L, 2L))
  expect_equal(k$dist_table$`1s%`, c(0.73, 0.43, 0.14))

  out <- capture.output(print(k))

  # header is the selected predictor's name, not the internal `pred`
  expect_true(any(grepl("cyl", out)))
  expect_false(any(grepl("pred", out)))
  expect_false(any(grepl("^\\s+[123]\\s", out)))

  # rows are labelled with the levels they describe
  row4 <- assoc_line(out, "73\\.00")
  expect_match(row4, "^\\s*4\\s+11\\s+8\\s+34\\.00\\s+73\\.00")
  row6 <- assoc_line(out, "43\\.00")
  expect_match(row6, "^\\s*6\\s+7\\s+3\\s+22\\.00\\s+43\\.00")
  row8 <- assoc_line(out, "14\\.00")
  expect_match(row8, "^\\s*8\\s+14\\s+2\\s+44\\.00\\s+14\\.00")
})

test_that("two-way segmentation uses the two grouping variable names as headers (M7)", {
  d <- as.data.frame(descriptr::mtcarz)
  k <- blr_segment_twoway(d, am, cyl, gear)

  event <- d$am == "1"
  tab <- table(d$cyl[event], d$gear[event]) / sum(!is.na(d$am))
  expect_equal(unname(unclass(k$twoway_segment)), unname(unclass(tab)))
  expect_equal(unname(dimnames(k$twoway_segment)), list(c("4", "6", "8"), c("3", "4", "5")))

  out <- capture.output(print(k))

  expect_match(out[1], "gear")
  expect_match(out[3], "^\\s*cyl\\s+3\\s+4\\s+5")
  expect_false(any(grepl("am", out)))
})

test_that("segment distribution and two-way tables print expected layout", {
  d <- as.data.frame(descriptr::mtcarz)
  expect_snapshot(print(blr_segment_dist(d, am, cyl)))
  expect_snapshot(print(blr_segment_twoway(d, am, cyl, gear)))
})
