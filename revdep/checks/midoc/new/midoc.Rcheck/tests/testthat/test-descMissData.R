# Check output when data are missing
res1<-evaluate_promise(descMissData(y="bmi7", covs="matage mated", data=bmi))
## Plot is not tested
#There's a trailing blank, but only visible in testing, so just trim for test purposes
test_that("descMissData output is as expected when data are missing",
  {
    expect_equal(trimws(paste0(gsub("\n","",res1$result), collapse=" "),"right"),
"1:2 c(1, 0) c(1, 1) c(1, 1) c(592, 408) c(59, 41)")
  }
)

