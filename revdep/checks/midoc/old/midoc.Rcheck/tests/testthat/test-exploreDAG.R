# Check output when there are no testable paths
res1<-evaluate_promise(exploreDAG(mdag="matage -> bmi7 mated -> matage
        mated -> bmi7 sep_unmeas -> mated bmi7 -> r", data=bmi))
#Trim output for test purposes
test_that("exploreDAG correctly identifies both the implied independencies and that
          none of them are testable",
  {
    expect_equal(substr(trimws(paste0(gsub("\n"," ",res1$messages), collapse=" "),
                               "right"),358,606),
"None of the fully observed variables are conditionally independent. Hence, no consistency checks will be performed.  Consider whether it is valid and possible to explore relationships between partially observed variables using the observed data, e.g")
  }
)

# Check output when there are testable paths
res2<-evaluate_promise(exploreDAG(mdag="matage -> bmi7 mated -> matage
        mated -> bmi7 sep_unmeas -> mated sep_unmeas -> r", data=bmi))
#Trim output for test purposes
test_that("exploreDAG correctly identifies both the implied independencies and
          the testable subset",
  {
    expect_equal(substr(trimws(paste0(gsub("\n"," ",res2$messages), collapse=" "),
                               "right"),378,543),
"These (conditional) independence statements are explored below using the canonical correlations approach for mixed data. See ??dagitty::localTests for further details")
  }
)


