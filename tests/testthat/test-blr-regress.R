test_that("output from blr_regress is as expected", {
  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  k <- blr_regress(model, odd_conf_limit = TRUE)

  expect_snapshot(k)
})

test_that("odds ratio confidence limits are labelled as profile-likelihood intervals", {
  model <- glm(
    honcomp ~ female + read, data = hsb2,
    family = binomial(link = "logit")
  )

  k   <- blr_regress(model, odd_conf_limit = TRUE)
  out <- capture.output(print(k))

  expect_true(any(grepl("95% Profile", out)))
  expect_false(any(grepl("95% Wald", out)))
  expect_equal(
    unname(as.matrix(k$blr_conf)),
    unname(suppressMessages(exp(confint(model)))[-1, ])
  )
})

test_that("blr_regress response profile keeps factor response labels", {
  d <- hsb2
  d$hc_lab <- factor(ifelse(d$honcomp == 1, "yes", "no"), levels = c("no", "yes"))

  k <- blr_regress(hc_lab ~ female + read, data = d)

  expect_equal(names(k$resp_prof), c("no", "yes"))
  expect_equal(as.integer(k$resp_prof), c(147L, 53L))
})

test_that("blr_regress prints factor response labels in the response summary", {
  d <- hsb2
  d$hc_lab <- factor(ifelse(d$honcomp == 1, "yes", "no"), levels = c("no", "yes"))

  out <- capture.output(blr_regress(hc_lab ~ female + read, data = d))
  start <- grep("Response Summary", out)
  end <- grep("Maximum Likelihood Estimates", out)
  summary_lines <- out[start:end]

  expect_true(any(grepl("no", summary_lines)))
  expect_true(any(grepl("yes", summary_lines)))
})

test_that("blr_regress reports nobs and response profile on data with missing values", {
  d <- hsb2
  d$read[which(d$honcomp == "0")[1:10]] <- NA

  model <- glm(
    honcomp ~ female + read, data = d,
    family = binomial(link = "logit")
  )

  k <- blr_regress(honcomp ~ female + read, data = d)

  expect_equal(k$n_obs, unname(nobs(model)))
  expect_equal(as.integer(k$resp_prof[[1]]), sum(model$y == 0))
  expect_equal(as.integer(k$resp_prof[[2]]), sum(model$y == 1))
})
