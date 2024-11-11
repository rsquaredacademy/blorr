# Check output when imputation model is valid
res1<-evaluate_promise(checkMI(dep="bmi7", preds="matage mated pregsize", r_dep="r",
        mdag="matage -> bmi7 mated -> matage mated -> bmi7 sep_unmeas -> mated
        sep_unmeas -> r pregsize -> bmi7 pregsize -> bwt sep_unmeas -> bwt"))
#There's a trailing blank, but only visible in testing, so just trim for test purposes
test_that("checkMI correctly identifies when MI is valid given the mDAG and imputation model",
  {
    expect_equal(trimws(paste0(gsub("\n"," ",res1$messages), collapse=" "),"right"),
"Based on the proposed directed acyclic graph (DAG), the incomplete variable and its missingness indicator are independent given imputation model predictors. Hence, multiple imputation methods which assume data are missing at random are valid in principle.")
  }
)

# Check output when imputation model is not valid, but could be valid for a
## different set of predictors
res2<-evaluate_promise(checkMI(dep="bmi7", preds="matage mated bwt", r_dep="r",
        mdag="matage -> bmi7 mated -> matage mated -> bmi7 sep_unmeas -> mated
        sep_unmeas -> r pregsize -> bmi7 pregsize -> bwt sep_unmeas -> bwt"))
#There's a trailing blank, but only visible in testing, so just trim for test purposes
test_that("checkMI correctly identifies imputation model is not valid, but could
  be valid for a different set of predictors",
  {
    expect_equal(trimws(paste0(gsub("\n"," ",res2$messages), collapse=" "),"right"),
"Based on the proposed directed acyclic graph (DAG), the incomplete variable and its missingness indicator are not independent given imputation model predictors. Hence, multiple imputation methods which assume data are missing at random are not valid.  Consider using a different imputation model and/or strategy (e.g. not-at-random fully conditional specification).  For example, the incomplete variable and its missingness indicator are independent if, in addition to the specified predictors, the following sets of variables are included as predictors in the imputation model (note that this list is not necessarily exhaustive, particularly if your DAG is complex):  pregsize  c(\"pregsize\", \"sep_unmeas\")")
  }
)


