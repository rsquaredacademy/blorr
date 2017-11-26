# libraries ------------------------
library(dplyr)
library(aod)
library(car)
library(lmtest)
library(magrittr)
library(forcats)
library(ResourceSelection)

# load data ------------------------
hsb <- descriptr::hsb

# create honcomp variable ----------
hsb %<>%
  mutate(honcomp = if_else(write >= 60, 1, 0))

# regression -----------------------
model <- glm(honcomp ~ female + read + science, data = hsb,
             family = binomial(link = 'logit'))

# number of iterations
model %>%
  use_series(iter)

# response variable levels ---------
model %>%
  model.frame() %>%
  model.response %>%
  unique()


# response variable name -----------
var_name <- model %>%
  model.frame() %>%
  names() %>%
  extract(1)

# number of observations
model %>%
  use_series(data) %>%
  nrow

# response profile
model %>%
  use_series(model) %>%
  select(var_name) %>%
  table

# model fit statistics
# AIC
model %>% AIC()
model %>% BIC()
model %>%
  logLik() %>%
  multiply_by(-2)

# odds ratio effects
model %>%
  coef %>%
  names %>%
  extract(-1)

# odds ratio point estimates
model %>%
  coef %>%
  exp %>%
  extract(-1)

# odds ratio confidence intervals
model %>%
  confint %>%
  as_tibble %>%
  slice(2:n()) %>%
  exp


# goodness of fit tests
# hosmer lemeshow test
hoslem.test(model$y, fitted(model))

# pearson chi square statistics
sum(residuals(model, type = "pearson")^2)
model %>%
  residuals(type = "pearson") %>%
  raise_to_power(2) %>%
  sum

# wald test
wald.test(b = coef(model), Sigma = vcov(model), Terms = 2:4)
Anova(model, type = 'II', test = 'Wald')

# score test

# confidence intervals
model %>%
  confint()

# anova
anova(model, test = "Chisq")

# fitted values
fitt <- predict.glm(model, newdata = hsb, type = "response", se.fit = T)
fitt

# likelihood ratio test
library(lmtest)
model0 <- glm(honcomp ~ 1, data = hsb, family = binomial(link = "logit"))
lrtest(model, model0)

# likelihood ration test using anova
anova(model, model0, test = "Chisq")

# confidence interval estimation
confint(model)
predict.glm(model, newdata = chd, type = "terms", se.fit = T)
predict.glm(model, newdata = chd, type = "response", se.fit = T)

# variance covariance matrix
vcov(model)

# percent concordant
# percent discordant
# percent tied
# percent pairs
# somers' D
# Gamma
# tau-a
# c
