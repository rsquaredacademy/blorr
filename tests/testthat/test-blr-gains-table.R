model <- glm(
  honcomp ~ female + read + science, data = hsb2,
  family = binomial(link = "logit")
)

test_that("output from blr_gains_table is as expected", {
  gtable   <- blr_gains_table(model, hsb2)
  sens     <- round(mean(gtable$gains_table$sensitivity), 2)
  spec     <- round(mean(gtable$gains_table$specificity), 2)
  accu     <- round(mean(gtable$gains_table$accuracy), 2)
  actual   <- c(sens, spec, accu)
  expected <- c(80.94, 54.35, 61.40)
  expect_equal(actual, expected)
})

test_that("gains table assigns every observation to exactly one decile", {
  for (n in c(8, 15, 35)) {
    set.seed(n)
    d <- data.frame(y = rbinom(n, 1, 0.5), x = rnorm(n))
    m <- glm(y ~ x, data = d, family = binomial)
    gt <- blr_gains_table(m)$gains_table

    expect_equal(sum(gt$total), nrow(d))
    expect_equal(sum(gt$`1`), sum(d$y))
    expect_true(all(gt$decile >= 1 & gt$decile <= 10))
    expect_equal(anyDuplicated(gt$decile), 0L)
  }
})

test_that("gains table handles a decile with no events", {
  p <- predict(model, newdata = hsb2, type = "response")

  test_data <- hsb2
  test_data$honcomp <- as.integer(
    rank(-p) > 20 & hsb2$honcomp == "1"
  )

  gt <- blr_gains_table(model, test_data)$gains_table

  expect_equal(gt$`1`[1], 0)
  expect_equal(sum(gt$`1`), sum(test_data$honcomp))
  expect_equal(sum(gt$total), nrow(test_data))
})


