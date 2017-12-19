context("test-utils.R")

hsb <- descriptr::hsb

# create honcomp variable
hsb %<>%
  mutate(honcomp = if_else(write >= 60, 1, 0))

# regression
model <- glm(honcomp ~ female + read + science, data = hsb,
             family = binomial(link = 'logit'))

test_that("response_var extract response variable name from the model", {

  actual <- response_var(model)
  expected <- sym('honcomp')
  expect_equal(actual, expected)

})

test_that('data_name extracts name of the data set from the model', {

  actual <- data_name(model)
  expected <- sym('hsb')
  expect_equal(actual, expected)

})


test_that('data_nrows returns the number of observations in the data set', {

  actual <- data_nrows(model)
  expected <- 200
  expect_equal(actual, expected)

})

test_that('converge_status returns the model convergence status', {

  actual <- converge_status(model)
  expect_true(actual)

})

test_that('residual_df returns the residual degrees of freedom', {

  actual <- residual_df(model)
  expected <- 196
  expect_equal(actual, expected)

})

test_that('model_df returns the model degrees of freedom', {

  actual <- model_df(model)
  expected <- 199
  expect_equal(actual, expected)

})

test_that('model_loglik returns log likelihood', {

  actual <- model_loglik(model) %>%
    round(4)
  expected <- 160.2364
  expect_equal(actual, expected)

})

test_that('null returns null log likelihood', {

  model <- glm(vs ~ cyl + disp + hp, data = mtcars,
               family = binomial(link = 'logit'))
  actual <- null_ll(model) %>%
    round(3)
  expected <- -21.93
  expect_equal(actual, expected)

})

test_that('output from resp_profile is as expected', {

  actual <- resp_profile(model) %>%
    extract2(1)
  expected <- 147
  expect_equal(actual, expected)

})
