context("test-model-fit-stats.R")

# regression
model <- glm(honcomp ~ female + read + science, data = hsb2,
             family = binomial(link = 'logit'))

test_that('model_deviance returns deviance', {

  actual <- model_deviance(model) %>%
    round(4)
  expected <- 160.2364
  expect_equal(actual, expected)

})

test_that('null_ll returns log likelihood of intercept only model', {

  model <- glm(vs ~ cyl + disp + hp, data = mtcars,
               family = binomial(link = 'logit'))
  actual <- null_ll(model) %>%
    round(3)
  expected <- -21.93
  expect_equal(actual, expected)

})

test_that('model_ll returns full model log likelihood', {

  actual <- model_ll(model) %>%
    round(3)
  expected <- -80.118
  expect_equal(actual, expected)

})

test_that('output from model_aic is as expected', {

  actual <- model_aic(model) %>%
    round(4)
  expected <- 168.2364
  expect_equal(actual, expected)

})

test_that('output from model_bic is as expected', {

  actual <- model_bic(model) %>%
    round(4)
  expected <- 181.4296
  expect_equal(actual, expected)

})

test_that('output from blr_mcfadden_rsq is as expected', {

  actual <- blr_mcfadden_rsq(model) %>%
    round(4)
  expected <- 0.3072
  expect_equal(actual, expected)

})

test_that('output from blr_mcfadden_adj_rsq is as expected', {

  actual <- blr_mcfadden_adj_rsq(model) %>%
    round(4)
  expected <- 0.2726
  expect_equal(actual, expected)

})

test_that('output from blr_cox_snell_rsq is as expected', {

  actual <- blr_cox_snell_rsq(model) %>%
    round(4)
  expected <- 0.299
  expect_equal(actual, expected)

})


test_that('output from blr_nagelkerke_rsq is as expected', {

  actual <- blr_nagelkerke_rsq(model) %>%
    round(4)
  expected <- 0.4363
  expect_equal(actual, expected)

})

test_that('output from blr_mckelvey_zavoina_rsq is as expected', {

  actual <- blr_mckelvey_zavoina_rsq(model) %>%
    round(4)
  expected <- 0.5178
  expect_equal(actual, expected)

})

test_that('output from blr_count_rsq is as expected', {

  actual <- blr_count_rsq(model) %>%
    round(4)
  expected <- 0.81
  expect_equal(actual, expected)

})

test_that('output from blr_adj_count_rsq is as expected', {

  actual <- blr_adj_count_rsq(model) %>%
    round(4)
  expected <- 0.283
  expect_equal(actual, expected)

})

test_that('output from blr_effron_rsq is as expected', {

  actual <- blr_effron_rsq(model) %>%
    round(4)
  expected <- 0.3305
  expect_equal(actual, expected)

})


