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


test_that("Hosmer-Lemeshow degrees of freedom adapts to partition table rows", {
  set.seed(2); n <- 600
  d <- data.frame(a = factor(sample(1:3, n, TRUE)), b = factor(sample(1:2, n, TRUE)))
  eta <- -1 + 0.8 * (d$a == "2") + 1.2 * (d$a == "3") + 0.5 * (d$b == "2") + 1.5 * (d$a == "3" & d$b == "2")
  d$y <- rbinom(n, 1, plogis(eta))
  m <- glm(y ~ a + b, data = d, family = binomial)

  res <- blr_test_hosmer_lemeshow(m)
  expect_equal(res$df, nrow(res$partition_table) - 2)
  expect_equal(res$df, 4)
  expect_equal(res$pvalue, pchisq(res$chisq_stat, df = 4, lower.tail = FALSE))
  expect_equal(round(res$pvalue, 3), 0.389)

  # 10-group case (hsb2) keeps df 8
  m10 <- glm(honcomp ~ female + read + science, data = hsb2, family = binomial(link = "logit"))
  res10 <- blr_test_hosmer_lemeshow(m10)
  expect_equal(nrow(res10$partition_table), 10)
  expect_equal(res10$df, 8)
})

test_that("Hosmer-Lemeshow print for fewer than 10 groups has no NA rows", {
  set.seed(2); n <- 600
  d <- data.frame(a = factor(sample(1:3, n, TRUE)), b = factor(sample(1:2, n, TRUE)))
  eta <- -1 + 0.8 * (d$a == "2") + 1.2 * (d$a == "3") + 0.5 * (d$b == "2") + 1.5 * (d$a == "3" & d$b == "2")
  d$y <- rbinom(n, 1, plogis(eta))
  m <- glm(y ~ a + b, data = d, family = binomial)

  res <- blr_test_hosmer_lemeshow(m)
  out <- capture.output(print(res))
  expect_false(any(grepl("NA", out)))
  expect_output(print(res), "Partition for the Hosmer & Lemeshow Test")
})

