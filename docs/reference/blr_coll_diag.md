# Collinearity diagnostics

Variance inflation factor, tolerance, eigenvalues and condition indices.

## Usage

``` r
blr_coll_diag(model)

blr_vif_tol(model)

blr_eigen_cindex(model)
```

## Arguments

- model:

  An object of class `glm`.

## Value

`blr_coll_diag` returns an object of class `"blr_coll_diag"`. An object
of class `"blr_coll_diag"` is a list containing the following
components:

- vif_t:

  tolerance and variance inflation factors

- eig_cindex:

  eigen values and condition index

## Details

Collinearity implies two variables are near perfect linear combinations
of one another. Multicollinearity involves more than two variables. In
the presence of multicollinearity, regression estimates are unstable and
have high standard errors.

*Tolerance*

Percent of variance in the predictor that cannot be accounted for by
other predictors.

*Variance Inflation Factor*

Variance inflation factors measure the inflation in the variances of the
parameter estimates due to collinearities that exist among the
predictors. It is a measure of how much the variance of the estimated
regression coefficient \\\beta_k\\ is inflated by the existence of
correlation among the predictor variables in the model. A VIF of 1 means
that there is no correlation among the kth predictor and the remaining
predictor variables, and hence the variance of \\\beta_k\\ is not
inflated at all. The general rule of thumb is that VIFs exceeding 4
warrant further investigation, while VIFs exceeding 10 are signs of
serious multicollinearity requiring correction.

*Condition Index*

Most multivariate statistical approaches involve decomposing a
correlation matrix into linear combinations of variables. The linear
combinations are chosen so that the first combination has the largest
possible variance (subject to some restrictions), the second combination
has the next largest variance, subject to being uncorrelated with the
first, the third has the largest possible variance, subject to being
uncorrelated with the first and second, and so forth. The variance of
each of these linear combinations is called an eigenvalue. Collinearity
is spotted by finding 2 or more variables that have large proportions of
variance (.50 or more) that correspond to large condition indices. A
rule of thumb is to label as large those condition indices in the range
of 30 or larger.

## References

Belsley, D. A., Kuh, E., and Welsch, R. E. (1980). Regression
Diagnostics: Identifying Influential Data and Sources of Collinearity.
New York: John Wiley & Sons.

## Examples

``` r
# model
model <- glm(honcomp ~ female + read + science, data = hsb2,
family = binomial(link = 'logit'))

# vif and tolerance
blr_vif_tol(model)
#>   Variable Tolerance      VIF
#> 1  female1 0.9824347 1.017879
#> 2     read 0.6021370 1.660752
#> 3  science 0.5939847 1.683545

# eigenvalues and condition indices
blr_eigen_cindex(model)
#>   Eigenvalue Condition Index    intercept      female1         read
#> 1 3.57391760        1.000000 3.500782e-06 2.232611e-05 0.0006577134
#> 2 0.39409893        3.011408 5.155657e-06 8.559696e-04 0.0015266309
#> 3 0.01888407       13.757025 1.632498e-03 4.397949e-05 0.1143698337
#> 4 0.01309940       16.517583 5.619292e-05 1.312420e-05 0.2758987745
#>        science
#> 1 0.0009448623
#> 2 0.0025603607
#> 3 0.0545537252
#> 4 0.5468553525

# collinearity diagnostics
blr_coll_diag(model)
#> Tolerance and Variance Inflation Factor
#> ---------------------------------------
#>   Variable Tolerance      VIF
#> 1  female1 0.9824347 1.017879
#> 2     read 0.6021370 1.660752
#> 3  science 0.5939847 1.683545
#> 
#> 
#> Eigenvalue and Condition Index
#> ------------------------------
#>   Eigenvalue Condition Index    intercept      female1         read
#> 1 3.57391760        1.000000 3.500782e-06 2.232611e-05 0.0006577134
#> 2 0.39409893        3.011408 5.155657e-06 8.559696e-04 0.0015266309
#> 3 0.01888407       13.757025 1.632498e-03 4.397949e-05 0.1143698337
#> 4 0.01309940       16.517583 5.619292e-05 1.312420e-05 0.2758987745
#>        science
#> 1 0.0009448623
#> 2 0.0025603607
#> 3 0.0545537252
#> 4 0.5468553525
```
