test_that("output from blr_regress is as expected", {
  model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  k <- blr_regress(model, odd_conf_limit = TRUE)

  expect_snapshot(k)
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
