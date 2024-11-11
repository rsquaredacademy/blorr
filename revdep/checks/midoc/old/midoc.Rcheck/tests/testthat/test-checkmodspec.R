# Check output when gaussian model is mis-specified
res1<-evaluate_promise(checkModSpec(formula="bmi7~matage+mated+pregsize",
        family="gaussian(identity)", data=bmi))
## Plot is not tested
#There's a trailing blank, but only visible in testing, so just trim for test purposes
test_that("checkModSpec correctly identifies that the proposed gaussian model is
          mis-specified",
  {
    expect_equal(trimws(paste0(gsub("\n"," ",res1$messages), collapse=" "),"right"),
"Model mis-specification method: regression of model residuals on a fractional polynomial of the fitted values  P-value: 0  A small p-value means the model may be mis-specified. Check the specification of each relationship in your model.")
    expect_equal(res1$result$formula, "bmi7~matage+mated+pregsize")
    expect_equal(res1$result$family, "gaussian(identity)")
    expect_equal(res1$result$datalab, "bmi")
  }
)

# Check output when gaussian model is correctly specified
res2<-evaluate_promise(checkModSpec(
  formula="bmi7~matage+I(matage^2)+mated+pregsize",
  family="gaussian(identity)", data=bmi))
## Plot is not tested
#There's a trailing blank, but only visible in testing, so just trim for test purposes
test_that("checkModSpec correctly identifies that the proposed gaussian model is
          correctly specified",
  {
    expect_equal(trimws(paste0(gsub("\n"," ",res2$messages), collapse=" "),"right"),
"Model mis-specification method: regression of model residuals on a fractional polynomial of the fitted values  P-value: 1  A large p-value means there is little evidence of model mis-specification.")
  }
)

# Check output when logistic model is mis-specified
res3<-evaluate_promise(checkModSpec(formula="mated~matage+bmi7+pregsize",
                                    family="binomial(logit)", data=bmi))
## Plot is not tested
#There's a trailing blank, but only visible in testing, so just trim for test purposes
test_that("checkModSpec correctly identifies that the proposed logistic model
          is mis-specified",
  {
    expect_equal(trimws(paste0(gsub("\n"," ",res3$messages), collapse=" "),"right"),
                 "Model mis-specification method: Pregibon's link test  P-value: 0.012756  A small p-value means the model may be mis-specified. Check the specification of each relationship in your model.")
    expect_equal(res3$result$formula, "mated~matage+bmi7+pregsize")
    expect_equal(res3$result$family, "binomial(logit)")
    expect_equal(res3$result$datalab, "bmi")
  }
)

# Check output when logistic model is correctly specified
res4<-evaluate_promise(checkModSpec(
  formula="mated~matage+I(matage^2)+bmi7+pregsize",
  family="binomial(logit)", data=bmi))
## Plot is not tested
#There's a trailing blank, but only visible in testing, so just trim for test purposes
test_that("checkModSpec correctly identifies that the proposed model is corretly specified",
  {
    expect_equal(trimws(paste0(gsub("\n"," ",res4$messages), collapse=" "),"right"),
"Model mis-specification method: Pregibon's link test  P-value: 0.381826  A large p-value means there is little evidence of model mis-specification.")
  }
)
