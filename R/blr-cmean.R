#' @title Conditional Mean
#' @description Conditional mean of the response variable for a given predictor
#' variable
#' @data a dataframe/tibble
#' @response the response variable column
#' @predictors the predictor variable columns
#' @details The \code{blr_cmean} function returns the conditional mean of the
#' response \code{Y} variable for a single or set of predictor \code{X}
#' variable(s)
#' @return a tibble; \code{blr_cmean} returns an object of class
#' \code{"blr_cmean"}. An object of class \code{"blr_cmean"} is a tibble
#' containing the following components
#'
#' \item{variable}{name of the predictor variable(s)}
#' \item{n}{number of observations}
#' \item{present}{number of 1s}
#' \item{absent}{number of 0s}
#' \item{cmean}{conditional mean}
#'
#' @references Hosmer, D. W., Jr., and Lemeshow, S. (2000). Applied Logistic
#' Regression. 2nd ed. New York: John Wiley & Sons. (p. 4)
#'
#' @examples
