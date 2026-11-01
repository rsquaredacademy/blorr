test_that("output from blr_hosmer_lemeshow_test is as expected", {
  model <- glm(
    honcomp ~ race + read + science, data = hsb2,
    family = binomial(link = "logit")
  )
  actual   <-  round(blr_test_hosmer_lemeshow(model)$pvalue, 2)
  expected <- 0.66
  expect_equal(actual, expected)
})


test_that("hosmer lemeshow test prints the correct output", {
  model <- glm(
    honcomp ~ race + read + science, data = hsb2,
    family = binomial(link = "logit")
  )

  k <- blr_test_hosmer_lemeshow(model)

  expect_snapshot(k)
})

test_that("Hosmer-Lemeshow accepts explicit training data and response labels", {
  model <- glm(am ~ wt + hp, data = mtcars, family = binomial)

  expect_equal(
    blr_test_hosmer_lemeshow(model)[c("chisq_stat", "pvalue")],
    blr_test_hosmer_lemeshow(model, mtcars)[c("chisq_stat", "pvalue")]
  )

  d  <- mtcars
  d$am <- factor(d$am, labels = c("no", "yes"))
  mf <- glm(am ~ wt + hp, data = d, family = binomial)

  k <- blr_test_hosmer_lemeshow(mf, d)[c("chisq_stat", "pvalue")]
  expect_false(anyNA(k))
})

test_that("Hosmer-Lemeshow df follows the number of observed groups", {
  d <- data.frame(
    y = c(0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0),
    x = rep(0:2, each = 4)
  )
  h <- blr_test_hosmer_lemeshow(glm(y ~ x, data = d, family = binomial))

  expect_equal(nrow(h$partition_table), 3L)
  expect_equal(h$df, 1L)
  expect_equal(h$pvalue, pchisq(h$chisq_stat, 1, lower.tail = FALSE))
})

test_that("Hosmer-Lemeshow is unavailable when there are too few groups", {
  d <- data.frame(y = c(0, 1, 0, 1), x = 1:4)
  h <- blr_test_hosmer_lemeshow(glm(y ~ 1, data = d, family = binomial))

  expect_equal(nrow(h$partition_table), 1L)
  expect_true(is.na(h$df))
  expect_true(is.na(h$pvalue))
})

