# First specify the imputation model as a 'mimod' object, suppressing the
## message
mimod_bmi7 <- checkModSpec(
  formula="bmi7~matage+I(matage^2)+mated+pregsize",
  family="gaussian(identity)", data=bmi, message=FALSE)
# Save the proposed 'mice' options as a 'miprop' object, suppressing the
## message and plots
miprop <- proposeMI(mimodobj=mimod_bmi7, data=bmi, plot=FALSE, message=FALSE)
# Check both the output when a substantive model is specified and that a
## mice object is created
res1<-evaluate_promise(doMImice(miprop, 123,
        substmod="lm(bmi7 ~ matage + I(matage^2) + mated)"))
#Trim output for test purposes
test_that("doMImice creates both the correct output when a substantive model is
          specified and a mice 'mids' object",
  {
    expect_equal(substr(trimws(paste0(gsub("\n"," ",res1$message), collapse=" "),
                               "right"),1,100),
"Given the substantive model: lm(bmi7 ~ matage + I(matage^2) + mated) , multiple imputation estimates")
    expect_equal(mice::is.mids(res1$result),TRUE)
  }
)
