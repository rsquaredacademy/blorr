# import data
library(readr)
chd <- readr::read_csv("chd.csv")
head(chd)

# convert chd to type factor
chd$chd <- factor(chd$chd, levels = c("0", "1"))

logi <- function(data, predictors, response, conf = 0.95) {

  # generate regression function
  reg_func <- as.formula(paste(response, "~", paste(predictors,
                                                    collapse= " + ")))
  predictor <- data %>%
    select_(predictors)

  # regression model
  model <- glm(formula =  reg_func, data = data, family = binomial)
  m <- summary(model)
  logit <- m$coefficients[1] + m$coefficients[2] * predictor
  se_fit <- sqrt((m$coefficients[3] ^ 2) + (predictor ^ 2) * (m$coefficients[4] ^ 2) +
                   2 * predictor * m$cov.unscaled[2])
  l_logit <- logit - (qnorm((1 - conf) / 2) * se_fit)
  u_logit <- logit + (qnorm((1 - conf) / 2) * se_fit)
  e <- 2.71828
  ep <- e ^ logit
  el <- e ^ l_logit
  eu <- e ^ u_logit
  prob <- ep / (1 + ep)
  prob_l <- el / (1 + el)
  prob_u <- eu / (1 + eu)
  out <- cbind(logit, se_fit, l_logit, u_logit, prob, prob_l, prob_u)
  names(out) <- c("Logit", "SE Logit", "Logit Lower", "Logit Upper",
                  "Probabaility", "Lower Prob", "Upper Prob")
  return(out)
}

# test
logi(chd, "age", "chd")






