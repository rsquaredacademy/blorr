#' @title Log Frequency
#' @description Mean response for each bin
#' @param data a data.frame
#' @param rcol response variable
#' @param ndummy number of dummy variables
#' @return a data.frame
#' @export
#'
freq_log <- function(data, rcol, ndummy) {
  n <- ndummy
  hesru <- names(data)[-rcol]
  response <- data[[rcol]]
  out <- data.frame(Group = character(0), Absent = integer(0),
                    Present = integer(0), n = integer(0),
                    Mean = numeric(0), stringsAsFactors = F)
  for (i in seq_len(n)) {
    g <- paste0("chd1$age", i)
    first <- eval(parse(text = g))
    taba <- table(first, response)[2, ]
    out[i, 1] <- hesru[i]
    out[i, 2] <- as.vector(taba[1])
    out[i, 3] <- as.vector(taba[2])
    out[i, 4] <- sum(out[i, 2], out[i, 3])
    out[i, 5] <- round(out[i, 3] / out[i, 4], 2)
  }

  # return table
  return(out)

}

# example
library(readr)
library(dplyr)

# import data
chd <- readr::read_csv("chd.csv")


# convert chd to type factor
chd$chd <- factor(chd$chd, levels = c("0", "1"))

# create dummy variables
chd1 <-  chd %>%
  mutate(age1 = ifelse(age >= 20 & age <= 29, 1, 0),
         age2 = ifelse(age >= 30 & age <= 34, 1, 0),
         age3 = ifelse(age >= 35 & age <= 39, 1, 0),
         age4 = ifelse(age >= 40 & age <= 44, 1, 0),
         age5 = ifelse(age >= 45 & age <= 49, 1, 0),
         age6 = ifelse(age >= 50 & age <= 54, 1, 0),
         age7 = ifelse(age >= 55 & age <= 59, 1, 0),
         age8 = ifelse(age >= 60 & age <= 69, 1, 0)
  )
data <- chd1
rcol <- 1
ft <- freq_log(chd1[, c(3:11)], 1, 8)
plot(ft$Mean, xaxt = "n")
axis(side = 1, labels = ft$Group, at = seq_len(8))
