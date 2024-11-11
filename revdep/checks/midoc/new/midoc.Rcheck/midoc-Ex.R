pkgname <- "midoc"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('midoc')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("checkCRA")
### * checkCRA

flush(stderr()); flush(stdout())

### Name: checkCRA
### Title: Inspect complete records analysis model
### Aliases: checkCRA

### ** Examples

# Example DAG for which complete records analysis is not valid, but could be
## valid for a different set of covariates
checkCRA(y="bmi7", covs="matage", r_cra="r",
         mdag="matage -> bmi7 mated -> matage mated -> bmi7
               sep_unmeas -> mated sep_unmeas -> r")
# For the DAG in the example above, complete records analysis is valid
## if a different set of covariates is used
checkCRA(y="bmi7", covs="matage mated", r_cra="r",
         mdag="matage -> bmi7 mated -> matage mated -> bmi7
               sep_unmeas -> mated sep_unmeas -> r")

# Example DAG for which complete records is not valid, but could be valid
## for a different estimand
checkCRA(y="bmi7", covs="matage mated", r_cra="r",
         mdag="matage -> bmi7 mated -> matage mated -> bmi7
               sep_unmeas -> mated sep_unmeas -> r matage -> bmi3
               mated -> bmi3 bmi3 -> bmi7 bmi3 -> r")

# Example DAG for which complete records analysis is never valid
checkCRA(y="bmi7", covs="matage mated", r_cra="r",
         mdag="matage -> bmi7 mated -> matage mated -> bmi7
               sep_unmeas -> mated sep_unmeas -> r bmi7 -> r")



cleanEx()
nameEx("checkMI")
### * checkMI

flush(stderr()); flush(stdout())

### Name: checkMI
### Title: Inspect multiple imputation model
### Aliases: checkMI

### ** Examples

# Example DAG for which multiple imputation is valid
checkMI(dep="bmi7", preds="matage mated pregsize", r_dep="r",
        mdag="matage -> bmi7 mated -> matage mated -> bmi7
              sep_unmeas -> mated sep_unmeas -> r pregsize -> bmi7
              pregsize -> bwt sep_unmeas -> bwt")

# Example DAG for which multiple imputation is not valid, due to a collider
checkMI(dep="bmi7", preds="matage mated bwt", r_dep="r",
        mdag="matage -> bmi7 mated -> matage mated -> bmi7
              sep_unmeas -> mated sep_unmeas -> r pregsize -> bmi7
              pregsize -> bwt sep_unmeas -> bwt")



cleanEx()
nameEx("checkModSpec")
### * checkModSpec

flush(stderr()); flush(stdout())

### Name: checkModSpec
### Title: Inspect parametric model specification
### Aliases: checkModSpec

### ** Examples

# Example (incorrectly) assuming a linear relationship
checkModSpec(formula="bmi7~matage+mated+pregsize",
             family="gaussian(identity)", data=bmi)
  ## For the example above, (correctly) assuming a quadratic relationship
checkModSpec(formula="bmi7~matage+I(matage^2)+mated+pregsize",
             family="gaussian(identity)", data=bmi)



cleanEx()
nameEx("descMissData")
### * descMissData

flush(stderr()); flush(stdout())

### Name: descMissData
### Title: Lists missing data patterns in the specified dataset
### Aliases: descMissData

### ** Examples

descMissData(y="bmi7", covs="matage mated", data=bmi)
descMissData(y="bmi7", covs="matage mated pregsize bwt", data=bmi, plot=TRUE)



cleanEx()
nameEx("doMImice")
### * doMImice

flush(stderr()); flush(stdout())

### Name: doMImice
### Title: Performs multiple imputation
### Aliases: doMImice

### ** Examples

# First specify the imputation model as a 'mimod' object
## (suppressing the message)
mimod_bmi7 <- checkModSpec(formula="bmi7~matage+I(matage^2)+mated+pregsize",
                           family="gaussian(identity)",
                           data=bmi,
                           message=FALSE)
# Save the proposed 'mice' options as a 'miprop' object
## (suppressing the message)
miprop <- proposeMI(mimodobj=mimod_bmi7,
                    data=bmi,
                    message=FALSE,
                    plot = FALSE)
# Create the set of imputed datasets using the proposed 'mice' options
imp <- doMImice(miprop,123)

# Additionally, fit the substantive model to each imputed dataset and display
## the pooled results
doMImice(miprop, 123, substmod="lm(bmi7 ~ matage + I(matage^2) + mated)")



cleanEx()
nameEx("exploreDAG")
### * exploreDAG

flush(stderr()); flush(stdout())

### Name: exploreDAG
### Title: Compares data with proposed DAG
### Aliases: exploreDAG

### ** Examples

exploreDAG(mdag="matage -> bmi7 mated -> matage mated -> bmi7
                 sep_unmeas -> mated sep_unmeas -> r",
           data=bmi)



cleanEx()
nameEx("midocVignette")
### * midocVignette

flush(stderr()); flush(stdout())

### Name: midocVignette
### Title: Run an interactive vignette for the midoc package
### Aliases: midocVignette

### ** Examples

## Don't show: 
if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
# Run the interactive vignette
midocVignette()
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("proposeMI")
### * proposeMI

flush(stderr()); flush(stdout())

### Name: proposeMI
### Title: Suggests multiple imputation options
### Aliases: proposeMI

### ** Examples

# First specify each imputation model as a 'mimod' object
## (suppressing the message)
mimod_bmi7 <- checkModSpec(formula="bmi7~matage+I(matage^2)+mated+pregsize",
                           family="gaussian(identity)",
                           data=bmi,
                           message=FALSE)
mimod_pregsize <- checkModSpec(
                           formula="pregsize~bmi7+matage+I(matage^2)+mated",
                           family="binomial(logit)",
                           data=bmi,
                           message=FALSE)

# Display the proposed 'mice' options (suppressing the plot prompt)
## When specifying a single imputation model
proposeMI(mimodobj=mimod_bmi7,
          data=bmi,
          plotprompt = FALSE)
## When specifying more than one imputation model (suppressing the plots)
proposeMI(mimodobj=list(mimod_bmi7,mimod_pregsize),
          data=bmi,
          plot = FALSE)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
