#' Collinearity diagnostics
#'
#' Variance inflation factor, tolerance, eigenvalues and condition indices.
#'
#' @param model An object of class \code{glm}.
#'
#' @details
#' Collinearity implies two variables are near perfect linear combinations of
#' one another. Multicollinearity involves more than two variables. In the
#' presence of multicollinearity, regression estimates are unstable and have
#' high standard errors.
#'
#' \emph{Tolerance}
#'
#' Percent of variance in the predictor that cannot be accounted for by other predictors.
#'
#' \emph{Variance Inflation Factor}
#'
#' Variance inflation factors measure the inflation in the variances of the parameter estimates due to
#' collinearities that exist among the predictors. It is a measure of how much the variance of the estimated
#' regression coefficient \eqn{\beta_k}  is inflated by the existence of correlation among the predictor variables
#' in the model. A VIF of 1 means that there is no correlation among the kth predictor and the remaining predictor
#' variables, and hence the variance of \eqn{\beta_k} is not inflated at all. The general rule of thumb is that VIFs
#' exceeding 4 warrant further investigation, while VIFs exceeding 10 are signs of serious multicollinearity
#' requiring correction.
#'
#' \emph{Condition Index}
#'
#' Most multivariate statistical approaches involve decomposing a correlation matrix into linear
#' combinations of variables. The linear combinations are chosen so that the first combination has
#' the largest possible variance (subject to some restrictions), the second combination
#' has the next largest variance, subject to being uncorrelated with the first, the third has the largest
#' possible variance, subject to being uncorrelated with the first and second, and so forth. The variance
#' of each of these linear combinations is called an eigenvalue. Collinearity is spotted by finding 2 or
#' more variables that have large proportions of variance (.50 or more) that correspond to large condition
#' indices. A rule of thumb is to label as large those condition indices in the range of 30 or larger.
#'
#'
#' @return \code{blr_coll_diag} returns an object of class \code{"blr_coll_diag"}.
#' An object of class \code{"blr_coll_diag"} is a list containing the
#' following components:
#'
#' \item{vif_t}{tolerance and variance inflation factors}
#' \item{eig_cindex}{eigen values and condition index}
#'
#' @references
#' Belsley, D. A., Kuh, E., and Welsch, R. E. (1980). Regression Diagnostics: Identifying Influential Data and
#' Sources of Collinearity. New York: John Wiley & Sons.
#'
#' @examples
#' # model
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' # vif and tolerance
#' blr_vif_tol(model)
#'
#' # eigenvalues and condition indices
#' blr_eigen_cindex(model)
#'
#' # collinearity diagnostics
#' blr_coll_diag(model)
#'
#' @importFrom stats lm as.formula model.matrix
#'
#' @export
#'
blr_coll_diag <- function(model) UseMethod("blr_coll_diag")

#' @export
#'
blr_coll_diag.default <- function(model) {

  blr_check_model(model)

  vift    <- blr_vif_tol(model)
  eig_ind <- blr_eigen_cindex(model)
  result  <- list(vif_t = vift, eig_cindex = eig_ind)

  class(result) <- "blr_coll_diag"
  return(result)

}

#' @export
#'
print.blr_coll_diag <- function(x, ...) {
  cat("Tolerance and Variance Inflation Factor\n")
  cat("---------------------------------------\n")
  print(x$vif_t)
  cat("\n\n")
  cat("Eigenvalue and Condition Index\n")
  cat("------------------------------\n")
  print(x$eig_cindex)
}

#' @rdname blr_coll_diag
#' @export
#'
blr_vif_tol <- function(model) {

  blr_check_model(model)

  vt <- viftol(model)
  data.frame(Variable  = vt$nam,
             Tolerance = vt$tol,
             VIF       = vt$vifs)

}

#' @rdname blr_coll_diag
#' @export
#'
blr_eigen_cindex <- function(model) {

  blr_check_model(model)

  pvdata <- NULL

  x      <- as.data.frame(model.matrix(model))    
  e      <- evalue(x)$e
  cindex <- cindx(e)
  pv     <- pveindex(evalue(x)$pvdata)
  out    <- data.frame(Eigenvalue = cbind(e, cindex, pv))
  
  colnames(out) <- c("Eigenvalue", "Condition Index",
                     colnames(evalue(x)$pvdata))
  return(out)

}


fmrsq <- function(nam, data, i) {

  r.squared <- NULL

  fm <- lm(as.formula(paste0("`", nam[i], "` ", "~ .")), data = data)
  max(1 - summary(fm)$r.squared, 0)

}

viftol <- function(model) {

  mm  <- model.matrix(model)
  m   <- as.data.frame(mm[, -1, drop = FALSE])
  nam <- names(m)
  p   <- ncol(m)
  tol <- numeric(p)

  if (p > 0) {
    zero_var <- vapply(m, function(x) {
      length(unique(x[!is.na(x)])) < 2
    }, logical(1))

    for (i in seq_len(p)) {
      tol[i] <- if (zero_var[i]) 0 else fmrsq(nam, m, i)
    }
  }

  vifs <- 1 / tol

  list(nam = nam, tol = tol, vifs = vifs)

}


evalue <- function(x) {

  values         <- NULL
  y              <- x
  colnames(y)[1] <- "intercept"
  z              <- scale(y, scale = T, center = F)
  tu             <- t(z) %*% z
  d              <- diag(tu)
  d[d == 0]      <- 1
  e              <- eigen(tu / d)$values
    
  list(e = e, pvdata = z)

}


cindx <- function(e) {

  # eigenvalues that are zero to numerical precision must not be fed into a
  # square root as small negative numbers
  tol <- sqrt(.Machine$double.eps) * max(abs(e), 1)
  e[abs(e) < tol] <- 0
  e <- pmax(e, 0)

  sqrt(e[1] / e)

}

pveindex <- function(z) {

  svdx     <- svd(z)
  svdxd    <- svdx$d
  tol      <- max(svdxd) * max(dim(z)) * .Machine$double.eps
  positive <- svdxd > tol

  phi <- matrix(0, nrow = ncol(z), ncol = length(svdxd))

  if (any(positive)) {
    phi[, positive] <- svdx$v[, positive, drop = FALSE] %*%
      diag(1 / svdxd[positive], nrow = sum(positive))
  }

  ph       <- t(phi ^ 2)
  diag_sum <- diag(rowSums(ph, dims = 1))
  out      <- prop.table(ph %*% diag_sum, margin = 2)

  # variance proportions are undefined for zero eigenvalues and for columns
  # that are exact linear combinations of the remaining design columns
  out[!positive, ] <- NA
  out[, colSums(out, na.rm = TRUE) == 0] <- NA

  out
  
}
