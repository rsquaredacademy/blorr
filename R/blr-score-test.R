#' @importFrom rlang as_integer
#' @title Score Test
#' @description Score Test
#' @param x numeric
#' @param y factor
#' @return list with score test and probability
#' @export
#'
blr_score_test <- function(x, y) {

  # table of factor variable
  ta <- table(y)
  y1 <- as_integer(y)

  # compute xbar and ybar
  xbar <- mean(x, na.rm = TRUE)
  ybar <- mean(y1, na.rm = TRUE)
  ybar2 <- ta[[2]] / sum(ta)

  num <- sum(x * (y1 - ybar))
  den <- (ybar2 * (1 - ybar2)) * sum((x - xbar) ^ 2)

  st <- num / sqrt(den)
  st_prob <- pnorm(st, lower.tail = F)
  result <- list(
    score_stat = st,
    score_prob = st_prob
  )
  return(result)

}

# example
# import data
library(readr)
chd <- readr::read_csv("chd.csv")

# convert chd to type factor
chd$chd <- factor(chd$chd, levels = c("0", "1"))

st <- blr_score_test(chd$age, chd$chd)

