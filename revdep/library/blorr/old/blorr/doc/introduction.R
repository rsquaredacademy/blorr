## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----install, eval=FALSE------------------------------------------------------
#  install.packages("blorr")

## ----libs---------------------------------------------------------------------
library(blorr)
library(magrittr)

## ----bivar1-------------------------------------------------------------------
blr_bivariate_analysis(bank_marketing, y, job, marital, education, default, 
  housing, loan, contact, poutcome)

## ----woe1---------------------------------------------------------------------
blr_woe_iv(bank_marketing, job, y)

## ----woeplot, fig.align='center', fig.width=7, fig.height=5-------------------
k <- blr_woe_iv(bank_marketing, job, y)
plot(k)

## ----woe2---------------------------------------------------------------------
blr_woe_iv_stats(bank_marketing, y, job, marital, education)

## ----mod1---------------------------------------------------------------------
model <- glm(y ~ ., data = bank_marketing, family = binomial(link = 'logit'))

## ----stepwise1----------------------------------------------------------------
blr_step_aic_both(model)

## ----stepwise3, fig.align='center', fig.width=7, fig.height=5-----------------
model %>%
  blr_step_aic_both() %>%
  plot()

## ----model--------------------------------------------------------------------
model <- glm(y ~  age + duration + previous + housing + default +
             loan + poutcome + job + marital, data = bank_marketing, 
             family = binomial(link = 'logit'))

## ----reg1---------------------------------------------------------------------
blr_regress(model)

## ----reg2---------------------------------------------------------------------
blr_regress(y ~  age + duration + previous + housing + default +
             loan + poutcome + job + marital, data = bank_marketing)

## ----mfs----------------------------------------------------------------------
blr_model_fit_stats(model)

## ----val5---------------------------------------------------------------------
blr_confusion_matrix(model, cutoff = 0.5)

## ----val6---------------------------------------------------------------------
blr_test_hosmer_lemeshow(model)

## ----val1---------------------------------------------------------------------
blr_gains_table(model)

## ----val7, fig.align='center', fig.width=7, fig.height=5----------------------
model %>%
	blr_gains_table() %>%
	plot()

## ----val2, fig.align='center', fig.width=7, fig.height=5----------------------
model %>%
	blr_gains_table() %>%
  blr_roc_curve()

## ----val3, fig.align='center', fig.width=7, fig.height=5----------------------
model %>%
	blr_gains_table() %>%
  blr_ks_chart()

## ----val9, fig.align='center', fig.width=7, fig.height=5----------------------
model %>%
  blr_gains_table() %>%
  blr_decile_lift_chart()

## ----val8, fig.align='center', fig.width=7, fig.height=5----------------------
model %>%
  blr_gains_table() %>%
  blr_decile_capture_rate()

## ----val4, fig.align='center', fig.width=7, fig.height=5----------------------
blr_lorenz_curve(model)

## ----infl, fig.align='center', fig.height=10, fig.width=8---------------------
blr_plot_diag_influence(model)

## ----lev, fig.align='center', fig.height=7, fig.width=7-----------------------
blr_plot_diag_leverage(model)

## ----fit, fig.align='center', fig.height=7, fig.width=7-----------------------
blr_plot_diag_fit(model)

