test_that("blr_test_lr throws an error when the model is not an object of class glm", {

  model1 <- glm(
    honcomp ~ 1, data = hsb2,
    family = binomial(link = "logit")
  )

  model2 <- lm(write ~ 1, data = hsb2)
  expect_error(blr_test_lr(model1, model2))
  expect_error(blr_test_lr(model2, model1))

})

test_that("output is as expected when reduced model is not specified", {

   model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  actual   <- round(blr_test_lr(model)$test_result[['lr_ratio']], 4)
  expected <- 71.0525

  expect_equal(actual, expected)

})

test_that("LR test df for nested models matches anova", {

  full_model <- glm(
    honcomp ~ female + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  reduced_model <- glm(
    honcomp ~ female + read, data = hsb2,
    family = binomial(link = "logit")
  )

  k <- blr_test_lr(full_model, reduced_model)
  a <- anova(reduced_model, full_model, test = "LRT")

  expect_equal(k$test_result[['lr_ratio']], a[['Deviance']][2])
  expect_equal(k$test_result[['d_f']], a[['Df']][2])
  expect_equal(k$test_result[['p_value']], a[['Pr(>Chi)']][2])

})

test_that("LR test uses the model frame when the data has missing values", {

  d <- hsb2
  d$read[which(d$honcomp == "0")[1:10]] <- NA

  model <- glm(
    honcomp ~ female + read + science, data = d,
    family = binomial(link = "logit")
  )

  k   <- blr_test_lr(model)
  ll0 <- as.numeric(logLik(glm(honcomp ~ 1, data = model.frame(model),
                               family = binomial(link = "logit"))))

  expect_equal(k$test_result[['d_f']], 3)
  expect_equal(k$test_result[['lr_ratio']], -2 * (ll0 - as.numeric(logLik(model))))

})


test_that("null model retains weights, offset, and family in blr_test_lr", {
  # weighted model
  set.seed(3)
  w <- sample(1:4, nrow(hsb2), replace = TRUE)
  m_w <- glm(honcomp ~ female + read + science, data = hsb2, family = binomial, weights = w)
  m0_w <- glm(honcomp ~ 1, data = hsb2, family = binomial, weights = w)
  res_w <- blr_test_lr(m_w)
  expect_equal(res_w$test_result[['lr_ratio']], m0_w$deviance - m_w$deviance)

  # offset model
  set.seed(4)
  off <- rnorm(nrow(hsb2))
  m_off <- glm(honcomp ~ female + read + science, data = hsb2, family = binomial, offset = off)
  m0_off <- glm(honcomp ~ 1, data = hsb2, family = binomial, offset = off)
  res_off <- blr_test_lr(m_off)
  expect_equal(res_off$test_result[['lr_ratio']], m0_off$deviance - m_off$deviance)

  # probit model
  m_prob <- glm(honcomp ~ female + read + science, data = hsb2, family = binomial(link = "probit"))
  expect_error(blr_test_lr(m_prob), "logit")
  expect_equal(i_model(m_prob)$family$link, "probit")
})
