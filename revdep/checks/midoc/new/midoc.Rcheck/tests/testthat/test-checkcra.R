# Check output when complete records analysis is not valid, but could be
## valid for a different set of covariates
res1<-evaluate_promise(checkCRA(y="bmi7", covs="matage", r_cra="r",
        mdag="matage -> bmi7 mated -> matage mated -> bmi7 sep_unmeas -> mated
        sep_unmeas -> r"))
#There's a trailing blank, but only visible in testing, so just trim for test purposes
test_that("checkCRA correctly identifies that CRA is not valid given the mDAG
          and analysis model but could be valid for a different set of covariates",
  {
    expect_equal(trimws(paste0(gsub("\n"," ",res1$messages), collapse=" "),"right"),
"Based on the proposed directed acyclic graph (DAG), the analysis model outcome and complete record indicator are not independent given analysis model covariates. Hence, in general, complete records analysis is not valid.  In special cases, depending on the type of analysis model and estimand of interest, complete records analysis may still be valid. See, for example, Bartlett et al. (2015) (https://doi.org/10.1093/aje/kwv114) for further details.  Consider using a different analysis model and/or strategy, e.g. multiple imputation.  For example, the analysis model outcome and complete record indicator are independent if, in addition to the specified covariates, the following sets of variables are included as covariates in the analysis model (note that this list is not necessarily exhaustive, particularly if your DAG is complex):  mated  c(\"mated\", \"sep_unmeas\")")
    }
)

# Check output when complete records analysis is valid
res2<-evaluate_promise(checkCRA(y="bmi7", covs="matage mated", r_cra="r",
         mdag="matage -> bmi7 mated -> matage mated -> bmi7 sep_unmeas -> mated
         sep_unmeas -> r"))
test_that("checkCRA correctly identifies that CRA is valid",
  {
    expect_equal(trimws(paste0(gsub("\n"," ",res2$messages), collapse=" "),"right"),
"Based on the proposed directed acyclic graph (DAG), the analysis model outcome and complete record indicator are independent given analysis model covariates. Hence, complete records analysis is valid.")
  }
)

# Check output when complete records analysis is not valid, but could be valid
## for a different estimand
res3<-evaluate_promise(checkCRA(y="bmi7", covs="matage mated", r_cra="r",
        mdag="matage -> bmi7 mated -> matage mated -> bmi7 sep_unmeas -> mated
        sep_unmeas -> r matage -> bmi3 mated -> bmi3 bmi3 -> bmi7 bmi3 -> r"))
test_that("checkCRA correctly identifies that CRA is not valid, but could be
          valid for a different estimand",
  {
    expect_equal(trimws(paste0(gsub("\n"," ",res3$messages), collapse=" "),"right"),
"Based on the proposed directed acyclic graph (DAG), the analysis model outcome and complete record indicator are not independent given analysis model covariates. Hence, in general, complete records analysis is not valid.  In special cases, depending on the type of analysis model and estimand of interest, complete records analysis may still be valid. See, for example, Bartlett et al. (2015) (https://doi.org/10.1093/aje/kwv114) for further details.  There are no other variables which could be added to the model to make the analysis model outcome and complete record indicator conditionally independent, without changing the estimand of interest. Consider using a different strategy e.g. multiple imputation.  Alternatively, consider whether a different estimand could be of interest. For example, the analysis model outcome and complete record indicator are independent given each of the following sets of variables:  c(\"bmi3\", \"mated\")  c(\"bmi3\", \"matage\", \"mated\")  c(\"bmi3\", \"sep_unmeas\")  c(\"bmi3\", \"matage\", \"sep_unmeas\")  c(\"bmi3\", \"mated\", \"sep_unmeas\")  c(\"bmi3\", \"matage\", \"mated\", \"sep_unmeas\")")
  }
)

# Check output when complete records analysis is never valid
res4 <- evaluate_promise(checkCRA(y="bmi7", covs="matage mated", r_cra="r",
          mdag="matage -> bmi7 mated -> matage mated -> bmi7 sep_unmeas -> mated
          sep_unmeas -> r bmi7 -> r"))
test_that("checkCRA correctly identifies that CRA is never valid",
  {
    expect_equal(trimws(paste0(gsub("\n"," ",res4$messages), collapse=" "),"right"),
"Based on the proposed directed acyclic graph (DAG), the analysis model outcome and complete record indicator are not independent given analysis model covariates. Hence, in general, complete records analysis is not valid.  In special cases, depending on the type of analysis model and estimand of interest, complete records analysis may still be valid. See, for example, Bartlett et al. (2015) (https://doi.org/10.1093/aje/kwv114) for further details.  There are no other variables which could be added to the model to make the analysis model outcome and complete record indicator conditionally independent. Consider using a different strategy e.g. multiple imputation.")
  }
)
