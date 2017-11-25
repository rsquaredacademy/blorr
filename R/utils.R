# libraries ------------------------
library(dplyr)
library(aod)
library(car)
library(lmtest)


# load data ------------------------
hsb <- descriptr::hsb

# create honcomp variable ----------
hsb <- hsb %>%
  mutate(honcomp = if_else(write >= 60, 1, 0))

# regression -----------------------
model <- glm(honcomp ~ female + read + science, data = hsb,
             family = binomial(link = 'logit'))

# response variable levels ---------
model$xlevels

# response variable name -----------
var_name <- model %>%
  model.frame() %>%
  names() %>%
  `[`(1)

# number of observations
model %>% fitted.values() %>% length()

# response profile
model %>%
  `$`(model) %>%
  select_(., var_name) %>%
  table(.)

# model fit statistics
# AIC
model %>% AIC()
model %>% BIC()
model %>%
  logLik() %>%
  `*`(-2)

# odds ratio
model %>%
  coef() %>%
  exp()

# wald test
wald.test(b = coef(model), Sigma = vcov(model), Terms = 2:4)
Anova(model, type = 'II', test = 'Wald')

# likelihood ratio test
lrtest(model)

# score test

# confidence intervals
model %>%
  confint()

# percent concordant
# percent discordant
# percent tied
# percent pairs
# somers' D
# Gamma
# tau-a
# c
