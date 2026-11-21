# Ordinary-input regression grid for the rx25 fix round.
#
# Every touched data-column function is compared with a base-R computation on
# ordinary mtcars, hsb2 and bank_marketing inputs, followed by one value test
# per audit finding. These tests are value assertions; snapshots are not used
# here.

event_vec <- function(x) {
  if (is.factor(x)) {
    as.integer(droplevels(x)) == 2L
  } else if (is.logical(x)) {
    x
  } else {
    x == 1
  }
}

call_segment <- function(d, response, predictor) {
  eval(bquote(blr_segment(d, .(as.name(response)), .(as.name(predictor)))))
}

call_segment_dist <- function(d, response, predictor) {
  eval(bquote(blr_segment_dist(d, .(as.name(response)), .(as.name(predictor)))))
}

call_segment_twoway <- function(d, response, v1, v2) {
  eval(bquote(
    blr_segment_twoway(d, .(as.name(response)), .(as.name(v1)),
                       .(as.name(v2)))
  ))
}

call_woe_iv <- function(d, predictor, response) {
  eval(bquote(blr_woe_iv(d, .(as.name(predictor)), .(as.name(response)))))
}

call_woe_iv_stats <- function(d, response, predictors) {
  args <- c(list(d, as.name(response)), lapply(predictors, as.name))
  do.call(blr_woe_iv_stats, args)
}

call_bivariate <- function(d, response, predictors) {
  args <- c(list(d, as.name(response)), lapply(predictors, as.name))
  do.call(blr_bivariate_analysis, args)
}

manual_segment <- function(d, response, predictor) {
  event <- event_vec(d[[response]])
  keep  <- !is.na(event)
  p     <- d[[predictor]][keep]
  event <- event[keep]
  n     <- as.numeric(table(p))
  ones  <- as.numeric(tapply(event, p, sum))

  data.frame(
    pred = names(table(p)),
    n    = n,
    ones = ones,
    rate = ones / n,
    stringsAsFactors = FALSE
  )
}

manual_twoway <- function(d, response, v1, v2) {
  event <- event_vec(d[[response]])
  keep  <- !is.na(event)
  a     <- d[[v1]][keep]
  b     <- d[[v2]][keep]
  event <- event[keep]

  denom  <- table(a, b)
  counts <- table(a[event], b[event])
  out    <- counts / denom
  out[denom == 0] <- NA
  out
}

manual_woe_iv <- function(d, predictor, response) {
  event <- event_vec(d[[response]])
  keep  <- !is.na(event) & !is.na(d[[predictor]])
  p     <- droplevels(factor(d[[predictor]][keep]))
  event <- event[keep]

  no_raw  <- as.numeric(tapply(!event, p, sum))
  yes_raw <- as.numeric(tapply(event, p, sum))

  no  <- no_raw
  yes <- yes_raw
  zero <- no == 0 | yes == 0
  no[zero]  <- no[zero] + 0.5
  yes[zero] <- yes[zero] + 0.5

  dist_no  <- no / sum(no)
  dist_yes <- yes / sum(yes)
  woe      <- log(dist_no / dist_yes)
  iv       <- (dist_no - dist_yes) * woe

  list(
    levels   = levels(p),
    no       = no_raw,
    yes      = yes_raw,
    dist_no  = dist_no,
    dist_yes = dist_yes,
    woe      = woe,
    iv       = iv,
    iv_total = sum(iv)
  )
}

mtcars_grid <- transform(mtcars, cyl = factor(cyl), gear = factor(gear))

grids <- list(
  mtcars = list(
    data       = mtcars_grid,
    response   = "am",
    predictors = c("cyl", "gear"),
    twoway     = c("cyl", "gear")
  ),
  hsb2 = list(
    data       = hsb2,
    response   = "honcomp",
    predictors = c("prog", "race", "female", "schtyp"),
    twoway     = c("prog", "race")
  ),
  bank_marketing = list(
    data       = bank_marketing,
    response   = "y",
    predictors = c("job", "marital", "education", "contact", "housing",
                   "loan", "poutcome"),
    twoway     = c("job", "marital")
  )
)

for (name in names(grids)) {
  grid <- grids[[name]]

  test_that(paste(name, "segment tables match base-R counts and rates"), {
    for (pred in grid$predictors) {
      k <- call_segment(grid$data, grid$response, pred)$segment_data
      expected <- manual_segment(grid$data, grid$response, pred)

      expect_equal(as.character(k$pred), expected$pred)
      expect_equal(k[["1s%"]], expected$rate, tolerance = 1e-12)
    }
  })

  test_that(paste(name, "segment distribution matches base-R counts and shares"), {
    for (pred in grid$predictors) {
      k <- call_segment_dist(grid$data, grid$response, pred)$dist_table
      expected <- manual_segment(grid$data, grid$response, pred)

      expect_equal(as.character(k$variable), expected$pred)
      expect_equal(k$n, expected$n)
      expect_equal(k[["1s"]], expected$ones)
      expect_equal(k[["n%"]], expected$n / sum(expected$n), tolerance = 1e-12)
      expect_equal(k[["1s%"]], expected$rate, tolerance = 1e-12)
    }
  })

  test_that(paste(name, "two-way rates are within-cell event rates"), {
    v1 <- grid$twoway[1]
    v2 <- grid$twoway[2]

    k <- call_segment_twoway(grid$data, grid$response, v1, v2)
    expected <- manual_twoway(grid$data, grid$response, v1, v2)

    expect_equal(as.matrix(k$twoway_segment), as.matrix(expected),
                 tolerance = 1e-12)
  })

  test_that(paste(name, "WoE and IV match base-R computation"), {
    for (pred in grid$predictors) {
      k <- call_woe_iv(grid$data, pred, grid$response)
      expected <- manual_woe_iv(grid$data, pred, grid$response)
      tab <- k$woe_iv_table

      expect_equal(as.character(tab$levels), expected$levels)
      expect_equal(tab$count_0s, expected$no)
      expect_equal(tab$count_1s, expected$yes)
      expect_equal(tab$dist_0s, round(expected$dist_no, 4), tolerance = 1e-12)
      expect_equal(tab$dist_1s, round(expected$dist_yes, 4), tolerance = 1e-12)
      expect_equal(tab$woe, round(expected$woe, 4), tolerance = 1e-12)
      expect_equal(tab$iv, round(expected$iv, 4), tolerance = 1e-12)
      expect_equal(k$iv_total, expected$iv_total, tolerance = 1e-12)
    }
  })

  test_that(paste(name, "variable WoE/IV output uses the unrounded totals"), {
    preds <- grid$predictors[1:2]
    out <- capture.output(call_woe_iv_stats(grid$data, grid$response, preds))

    for (pred in preds) {
      expected <- manual_woe_iv(grid$data, pred, grid$response)
      expect_true(any(grepl(paste0("Variable: ", pred), out, fixed = TRUE)))
      expect_true(any(grepl(as.character(round(expected$iv_total, 4)),
                            out, fixed = TRUE)))
    }
  })

  test_that(paste(name, "bivariate IV and LR match base R"), {
    preds <- grid$predictors[1:2]
    k <- call_bivariate(grid$data, grid$response, preds)
    m0 <- glm(reformulate("1", grid$response), data = grid$data,
              family = binomial)

    for (i in seq_along(preds)) {
      pred <- preds[i]
      expected <- manual_woe_iv(grid$data, pred, grid$response)
      m <- glm(reformulate(pred, grid$response), data = grid$data,
               family = binomial)
      a <- anova(m0, m, test = "Chisq")

      expect_equal(k$iv[i], expected$iv_total, tolerance = 1e-10)
      expect_equal(k$likelihood_ratio[i], a[["Deviance"]][2],
                   tolerance = 1e-8)
      expect_equal(k$df[i], a[["Df"]][2])
      expect_equal(k$pval[i], a[["Pr(>Chi)"]][2], tolerance = 1e-10)
    }
  })
}

test_that("H1: two-way cells are events divided by responses in the cell", {
  d <- transform(mtcars, cyl = factor(cyl), gear = factor(gear))
  k <- blr_segment_twoway(d, am, cyl, gear)

  expect_equal(unname(k$twoway_segment["4", "4"]), 6 / 8)
  expect_equal(unname(k$twoway_segment["6", "4"]), 2 / 4)
  expect_true(is.na(k$twoway_segment["8", "4"]))

  out <- capture.output(print(k))
  expect_true(any(grepl("^\\s*4\\s+0\\.000\\s+0\\.750\\s+1\\.000", out)))
  expect_true(any(grepl("^\\s*8\\s+0\\.000\\s+-\\s+1\\.000", out)))
})

test_that("H2: proportions stay unrounded and only printing rounds (M5 tests)", {
  d <- transform(mtcars, cyl = factor(cyl))

  k <- blr_segment(d, am, cyl)$segment_data
  expect_equal(k[["1s%"]], c(8 / 11, 3 / 7, 2 / 14), tolerance = 1e-12)

  k2 <- blr_segment_dist(d, am, cyl)$dist_table
  expect_equal(k2[["n%"]], c(11 / 32, 7 / 32, 14 / 32), tolerance = 1e-12)
  expect_equal(k2[["1s%"]], c(8 / 11, 3 / 7, 2 / 14), tolerance = 1e-12)

  out <- capture.output(print(blr_segment_dist(d, am, cyl)))
  expect_true(any(grepl("34\\.38\\s+72\\.73", out)))
  expect_false(any(grepl("34\\.00\\s+73\\.00", out)))
})

test_that("H2: the segment distribution plot uses the unrounded values", {
  d <- transform(mtcars, cyl = factor(cyl))
  k <- blr_segment_dist(d, am, cyl)
  p <- plot(k, print_plot = FALSE)
  built <- ggplot2::ggplot_build(p)$data

  expect_equal(sort(built[[1]]$y), sort(k$dist_table[["n%"]]),
               tolerance = 1e-12)
  expect_equal(sort(built[[2]]$y), sort(k$dist_table[["1s%"]]),
               tolerance = 1e-12)
})

test_that("H3: one-level numeric and factor responses are both rejected", {
  apis <- list(
    blr_segment = function(d) blr_segment(d, y, g),
    blr_segment_dist = function(d) blr_segment_dist(d, y, g),
    blr_segment_twoway = function(d) blr_segment_twoway(d, y, g, h),
    blr_woe_iv = function(d) blr_woe_iv(d, g, y),
    blr_woe_iv_stats = function(d) blr_woe_iv_stats(d, y, g)
  )
  base <- data.frame(g = c("a", "a", "b", "b"), h = c("x", "y", "x", "y"))

  for (api in names(apis)) {
    for (y in list(c(1, 1, 1, 1), factor(c(1, 1, 1, 1)))) {
      d <- base
      d$y <- y
      expect_error(
        apis[[api]](d),
        "`y` has 1 level; blorr needs both events and non-events.",
        fixed = TRUE
      )
    }
  }

  expect_error(
    blr_check_binary_response(c(1, 1, 1), "y"),
    "`y` has 1 level; blorr needs both events and non-events.",
    fixed = TRUE
  )
  expect_error(
    blr_check_binary_response(factor(c(1, 1, 1)), "y"),
    "`y` has 1 level; blorr needs both events and non-events.",
    fixed = TRUE
  )
})

test_that("H4: total IV is rounded once from full-precision components", {
  d <- data.frame(y = c(0, 1, 0, 1, 0, 1),
                  g = c("a", "a", "b", "b", "a", "b"))

  k <- blr_woe_iv(d, g, y, digits = 1)
  expected <- 2 * (2 / 3 - 1 / 3) * log(2)

  expect_equal(k$iv_total, expected, tolerance = 1e-12)
  expect_equal(sum(k$woe_iv_table$iv), 0.4, tolerance = 1e-12)

  out <- capture.output(print(k))
  expect_true(any(grepl("0\\.5", out)))
  expect_false(any(grepl("0\\.4\\s*$", out)))
})

test_that("M1: a missing predictor is a named error, not a printer crash", {
  d <- data.frame(y = c(0, 1, 0, 1),
                  g = c("a", "a", NA, "b"),
                  h = c("x", "y", "x", "y"))

  expect_error(
    blr_segment(d, y, g),
    "The predictor `g` has 1 missing value",
    fixed = TRUE
  )
  expect_error(
    blr_segment_dist(d, y, g),
    "The predictor `g` has 1 missing value",
    fixed = TRUE
  )
  expect_error(
    blr_segment_twoway(d, y, h, g),
    "The predictor `g` has 1 missing value",
    fixed = TRUE
  )
})

test_that("M2: no-event cells are zero and empty tables print without a crash", {
  k <- blr_segment_twoway(hsb2, honcomp, prog, race)
  expect_equal(unname(k$twoway_segment["1", "1"]), 0)

  out <- capture.output(print(k))
  expect_true(any(grepl("^\\s*1\\s+0\\.000", out)))

  zero <- data.frame(y = c(0, 0, 0, 0), g = c("a", "a", "b", "b"),
                     h = c("x", "y", "x", "y"))
  expect_error(
    blr_segment_twoway(zero, y, g, h),
    "`y` has 1 level; blorr needs both events and non-events.",
    fixed = TRUE
  )

  empty <- data.frame(y = numeric(0), g = character(0), h = character(0))
  expect_error(
    blr_segment_twoway(empty, y, g, h),
    "`y` has 0 levels; blorr needs both events and non-events.",
    fixed = TRUE
  )

  x <- structure(
    list(twoway_segment = matrix(numeric(0), nrow = 0, ncol = 0),
         varnames = c("y", "g", "h")),
    class = "blr_segment_twoway"
  )
  expect_output(print(x), "No non-missing observations to summarise.")
})
