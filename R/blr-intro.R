# import data
library(readr)
chd <- readr::read_csv("chd.csv")
head(chd)

# convert chd to type factor
chd$chd <- factor(chd$chd, levels = c("0", "1"))

# scatter plot
plot(chd$age, chd$chd)

# binning age
library(sqldf)
help(package = 'sqldf')
sqldf("select * from chd where age <= 29 and age >= 20;")
library(dplyr)
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

data <- chd1
rcol <- 1

ft <- freq_log(chd1[, c(3:11)], 1, 8)
plot(ft$Mean, xaxt = "n")
axis(side = 1, labels = ft$Group, at = seq_len(8))

# logistic regression
model <- glm(chd ~ age, data = chd, family = binomial)
m <- summary(model)

# anova
anova(model, test = "Chisq")

# fitted values
fitt <- predict.glm(model, newdata = chd, type = "response", se.fit = T)
fitt

# likelihood ratio test
library(lmtest)
model0 <- glm(chd ~ 1, data = chd, family = binomial(link = "logit"))
lrtest(model, model0)

# likelihood ration test using anova
anova(model, model0, test = "Chisq")

# wald test
install.packages("aod")
library(aod)
wald.test(Sigma = vcov(model), b = coef(model), Terms = 2)

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

st <- scoretest(chd$age, chd$chd)
pnorm(st, lower.tail = F)

# confidence interval estimation
confint(model)
predict.glm(model, newdata = chd, type = "terms", se.fit = T)
predict.glm(model, newdata = chd, type = "response", se.fit = T)

# variance covariance matrix
vcov(model)

# complete output
