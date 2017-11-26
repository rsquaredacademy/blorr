#' @title Gains Table & Lift Curve
#' @description Gains table and lift curve
#' @param a data.frame with response variable and probability
#' @return a data.frame
#' @export
gainsTable <- function(dataframe) {
    # sort the table in descending order by prob column
    custom <- dataframe[order(-prob), ]
    # number of columns
    numcol <- ncol(custom)
    numc <- numcol - 1
    # length of rows
    n <- nrow(custom)
    # interval of groups
    interval <- n / 10
    # separate the purch column
    purch <- custom[, numc]
    # split the dataframe and sum the 1s
    m <- seq_along(purch)
    s <- ceiling(m / interval)
    inta <- split(purch, s)
    pur <- unlist(lapply(inta, sum))
    # create the final table
    Decile <- 1:10
    total <- rep(interval, times = 10)
    lift <- data.frame(Decile, total, pur)
    # create the lift table
    lift <- mutate(lift,
        no_pur = total - pur,
        cum_pur = cumsum(pur),
        cum_nopur = cumsum(no_pur),
        cum_total = cumsum(total),
        cum_total_p = (cum_total / sum(total)) * 100,
        cum_pur_p = (cum_pur / sum(pur)) * 100,
        cum_nopur_p = (cum_nopur / sum(no_pur)) * 100,
        KS = cum_pur_p - cum_nopur_p,
        TP = cum_pur,
        TN = cum_nopur[10] - cum_nopur,
        FP = cum_nopur,
        FN = cum_pur[10] - cum_pur,
        Sensitivity = (TP / (TP + FN)) * 100,
        Specificity = (TN / (TN + FP)) * 100,
        Accuracy = ((TP + TN) / cum_total[10]) * 100
    )

    # names of the columns
    names(lift) <- c("Decile", "Total", "Purchase", "No Purchase",
                     "Cum Pur", "Cum NoPur", "Cum Total", "Cum Total %",
                     "Cum Pur %", "Cum NoPur %", "KS", "TP",
                     "TN", "FP", "FN", "Sensitivity", "Specifitivity",
                     "Accuracy")

    # return the result
    return(lift)
}

# packages
library(dplyr)

# create test data
x <- 0:1
purchase <- sample(x, size = 2600, replace = TRUE)
y <- as.double(c(1:99))
y1 <- y / 100
prob <- sample(y1, size = 2600, replace = TRUE)
customer <- data.frame(purchase, prob)

# result
output <- gainsTable(customer)

# testing
a <- sum(customer$purchase)
b <- sum(customer$pur)
all.equal(a, b)

View(output)




















