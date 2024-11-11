# regression
model <- glm(
  honcomp ~ female + read + science, data = hsb2,
  family = binomial(link = "logit")
)

test_that("model_deviance returns deviance", {
  actual   <- round(model_deviance(model), 4)
  expected <- 160.2364
  expect_equal(actual, expected)
})

test_that("null_ll returns log likelihood of intercept only model", {
  model <- glm(
    vs ~ cyl + disp + hp, data = mtcars,
    family = binomial(link = "logit")
  )
  actual   <- round(null_ll(model), 3)
  expected <- -21.93
  expect_equal(actual, expected)
})

test_that("model_ll returns full model log likelihood", {
  actual   <- round(model_ll(model), 3)
  expected <- -80.118
  expect_equal(actual, expected)
})

test_that("output from model_aic is as expected", {
  actual   <- round(model_aic(model), 4)
  expected <- 168.2364
  expect_equal(actual, expected)
})

test_that("output from model_bic is as expected", {
  actual   <- round(model_bic(model), 4)
  expected <- 181.4296
  expect_equal(actual, expected)
})

test_that("output from blr_mcfadden_rsq is as expected", {
  actual   <- round(blr_rsq_mcfadden(model), 4)
  expected <- 0.3072
  expect_equal(actual, expected)
})

test_that("output from blr_mcfadden_adj_rsq is as expected", {
  actual   <- round(blr_rsq_mcfadden_adj(model), 4)
  expected <- 0.2726
  expect_equal(actual, expected)
})

test_that("output from blr_cox_snell_rsq is as expected", {
  actual   <- round(blr_rsq_cox_snell(model), 4)
  expected <- 0.299
  expect_equal(actual, expected)
})


test_that("output from blr_nagelkerke_rsq is as expected", {
  actual   <- round(blr_rsq_nagelkerke(model), 4)
  expected <- 0.4363
  expect_equal(actual, expected)
})

test_that("output from blr_mckelvey_zavoina_rsq is as expected", {
  actual   <- round(blr_rsq_mckelvey_zavoina(model), 4)
  expected <- 0.5178
  expect_equal(actual, expected)
})

test_that("output from blr_count_rsq is as expected", {
  actual   <- round(blr_rsq_count(model), 4)
  expected <- 0.81
  expect_equal(actual, expected)
})

test_that("output from blr_adj_count_rsq is as expected", {
  actual   <- round(blr_rsq_adj_count(model), 4)
  expected <- 0.283
  expect_equal(actual, expected)
})

test_that("output from blr_effron_rsq is as expected", {
  actual   <- round(blr_rsq_effron(model), 4)
  expected <- 0.3305
  expect_equal(actual, expected)
})

test_that("blr_model_fit_stats prints the correct output", {
  k <- blr_model_fit_stats(model)
  expect_snapshot(k)
})

test_that("blr_multi_model_fit_stats prints the correct output", {
  model1 <- glm(
    honcomp ~ prog + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  k <- blr_multi_model_fit_stats(model, model1)
  expect_snapshot(k)
})


test_that("blr_lr_test prints the correct output", {
  k <- blr_test_lr(model)
  expect_snapshot(k)
})
