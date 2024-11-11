# First specify each imputation model as a 'mimod' object, suppressing the
## message
mimod_bmi7 <- checkModSpec(
  formula="bmi7~matage+I(matage^2)+mated+pregsize",
  family="gaussian(identity)", data=bmi, message=FALSE)
mimod_pregsize <- checkModSpec(
  formula="pregsize~bmi7+matage+I(matage^2)+mated",
  family="binomial(logit)", data=bmi, message=FALSE)
# Check the proposed 'mice' options when specifying more than one imputation
## model (suppressing the plot)
res1<-evaluate_promise(proposeMI(mimodobj=list(mimod_bmi7,mimod_pregsize),
                                 data=bmi, plot = FALSE))
#Trim output for test purposes
test_that("proposeMI suggests correct mice options and creates expected object",
  {
    expect_equal(substr(trimws(paste0(gsub("\n"," ",res1$messages), collapse=" "),
                               "right"),1,110),
"Based on your proposed imputation model and dataset, your mice() call should be as follows:  mice(data = bmi ,")
    expect_equal(res1$result$m,41)
    expect_equal(res1$result$method,list("norm", "logreg"))
    expect_equal(paste0(res1$result$formulas),c("bmi7 ~ matage + I(matage^2) + mated + pregsize", "pregsize ~ bmi7 + matage + I(matage^2) + mated"))
  }
)
