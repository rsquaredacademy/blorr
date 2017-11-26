# import data
library(readr)
chd <- readr::read_csv("chd.csv")
head(chd)

# convert chd to type factor
chd$chd <- factor(chd$chd, levels = c("0", "1"))

# score test
# score test
scoretest <- function(x, y) {

  # table of factor variable
  ta <- table(y)
  y1 <- as.integer(y)

  # compute xbar and ybar
  xbar <- mean(x, na.rm = T)
  ybar <- mean(y1, na.rm = T)
  ybar2 <- ta[[2]] / sum(ta)

  num <- sum(x * (y1 - ybar))
  den <- (ybar2 * (1 - ybar2)) * sum((x - xbar) ^ 2)


  st <- num / sqrt(den)
  return(round(st, 2))

}

# example
st <- scoretest(chd$age, chd$chd)
pnorm(st, lower.tail = F)
