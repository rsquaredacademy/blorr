# #130: the response profile, the printed regression summary and the object
# they are built from assumed exactly two response levels. glm() silently
# recodes a factor response with three or more levels as "first level" versus
# "everything else", so the extra levels were dropped from the printed output.

three_level_data <- function() {
  d <- hsb2
  d$ses3 <- d$ses # factor with three levels: 1, 2, 3
  d
}

test_that("resp_profile errors for a three-level factor response (#130)", {
  d <- three_level_data()
  m <- glm(ses3 ~ female + read, data = d, family = binomial)

  expect_error(
    resp_profile(m),
    "The response `ses3` has 3 levels"
  )
})

test_that("blr_regress errors for a three-level factor response (#130)", {
  d <- three_level_data()

  expect_error(
    blr_regress(ses3 ~ female + read, data = d),
    "The response `ses3` has 3 levels"
  )
})

test_that("print_blr_reg errors when the stored profile is not binary (#130)", {
  d <- three_level_data()
  bad <- list(resp_name = "ses3", resp_prof = table(d$ses3))

  expect_error(
    print_blr_reg(bad),
    "The response `ses3` has 3 levels"
  )
})

test_that("model functions reject a three-level response through the shared gate (#130)", {
  d <- three_level_data()
  m <- glm(ses3 ~ female + read, data = d, family = binomial)

  # both fit statistics and R2 helpers reach blr_check_supported() directly
  expect_error(
    blr_model_fit_stats(m),
    "The response `ses3` has 3 levels"
  )
  expect_error(
    blr_rsq_adj_count(m),
    "The response `ses3` has 3 levels"
  )

  # a stepwise entry point, with a full candidate set
  ms <- glm(ses3 ~ female + read + math, data = d, family = binomial)
  expect_error(
    blr_step_aic_forward(ms),
    "The response `ses3` has 3 levels"
  )
})

test_that("binary responses are unchanged (#130)", {
  # values captured from the package before the check was added
  m <- glm(honcomp ~ female + read + science, data = hsb2,
           family = binomial)
  expect_equal(names(resp_profile(m)), c("0", "1"))
  expect_equal(as.integer(resp_profile(m)), c(147L, 53L))

  d <- hsb2
  d$hc_lab <- factor(ifelse(d$honcomp == 1, "yes", "no"),
                     levels = c("no", "yes"))
  mf <- glm(hc_lab ~ female + read, data = d, family = binomial)
  expect_equal(names(resp_profile(mf)), c("no", "yes"))
  expect_equal(as.integer(resp_profile(mf)), c(147L, 53L))

  ml <- glm(am == 1 ~ wt, data = mtcars, family = binomial)
  expect_equal(names(resp_profile(ml)), c("FALSE", "TRUE"))
  expect_equal(as.integer(resp_profile(ml)), c(19L, 13L))

  # the printed response summary is byte for byte what it was before the check
  k <- blr_regress(am ~ wt + hp, data = mtcars)
  expect_equal(as.integer(k$resp_prof), c(19L, 13L))

  out <- capture.output(print(k))
  summary_lines <- trimws(
    out[grep("Response Summary", out):(grep("Maximum Likelihood Estimates", out) - 1L)]
  )
  expect_equal(
    summary_lines,
    c(
      "Response Summary",
      "--------------------------------------------------------",
      "Outcome        Frequency        Outcome        Frequency",
      "--------------------------------------------------------",
      "0              19               1              13",
      "--------------------------------------------------------",
      ""
    )
  )
})
