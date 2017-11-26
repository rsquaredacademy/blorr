#' @title Weight of Evidence & Information Value
#' @description Weight of evidence and information value
#' @param dframe a data.frame
#' @param var_col predictor
#' @param response_col response variable
#' @return a data.frame
#' @export
#'
crossTab <- function(dframe, var_col, response_col) {

  dat <- dframe[, c(var_col, response_col)]
  f <- table(dat)
  f1 <- table(dat)
  n <- rbind(f, f1)
  d <- unique(n)
  d <- as.data.frame(d)
  names(d) <- c("no", "yes")
  d$total <- d$no + d$yes
  d$distribution <- round((d$total / sum(d$total) * 100), digits = 2)
  d$approval <- round(((d$yes / d$total) * 100), digits = 2)
  d$dist_yes <- round(d$yes / sum(d$yes), digits = 2)
  d$dist_no <- round(d$no / sum(d$no), digits = 2)
  d$WOE <- round(log(d$dist_yes / d$dist_no), digits = 2)
  d$dist_diff <- d$dist_yes - d$dist_no
  d$IV <- round((d$dist_diff * d$WOE), digits = 2)
  d

}

# test
chd <- readr::read_csv('chd.csv')
crossTab(chd, 'age', 'chd')
