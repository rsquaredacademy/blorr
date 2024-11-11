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
