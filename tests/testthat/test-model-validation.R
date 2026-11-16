model <- glm(
  honcomp ~ female + read + science, data = hsb2,
  family = binomial(link = "logit")
)

test_that("prints the correct output", {

  k <- blr_confusion_matrix(model, cutoff = 0.4)
  expect_snapshot(k)
})

test_that("confusion matrix statistics match hand computation", {
  k   <- blr_confusion_matrix(model, cutoff = 0.4)
  tab <- k$conf_matrix

  tp <- tab[2, 2]
  fp <- tab[2, 1]
  fn <- tab[1, 2]
  tn <- tab[1, 1]

  expect_equal(k$accuracy, (tp + tn) / sum(tab))
  expect_equal(k$sensitivity, tp / (tp + fn))
  expect_equal(k$specificity, tn / (tn + fp))
  expect_equal(k$no_information_rate, max(table(hsb2$honcomp)) / nrow(hsb2))
})

test_that("confusion matrix handles a model that predicts one class", {
  set.seed(3)
  d <- data.frame(y = c(rep(0, 400), rep(1, 20)), x = rnorm(420))
  m <- glm(y ~ x, data = d, family = binomial)

  k <- blr_confusion_matrix(m)

  expect_equal(dim(k$conf_matrix), c(2L, 2L))
  expect_equal(k$sensitivity, 0)
  expect_equal(k$specificity, 1)
  expect_equal(k$accuracy, mean(d$y == 0))
  expect_equal(k$no_information_rate, mean(d$y == 0))
})

test_that("McNemar's test is missing when there are no discordant predictions", {
  d <- data.frame(y = rep(0, 10), x = seq_len(10))
  m <- glm(y ~ x, data = d, family = binomial)

  k <- blr_confusion_matrix(m)

  expect_true(is.na(k$mcnemar_test_p_val))
  expect_match(k$mcnemar_note, "not defined")
})

test_that("no information rate uses the larger class", {
  set.seed(16)
  y <- c(rep(0, 148), rep(1, 152))
  d <- data.frame(y = y, x = y + rnorm(300, sd = 2))
  m <- glm(y ~ x, data = d, family = binomial)

  k <- blr_confusion_matrix(m)

  expect_equal(k$no_information_rate, 152 / 300)
  expect_gt(k$no_information_rate, 0.5)
})


test_that('throws an error when the model is not of the class glm', {
  model <- lm(read ~ female + science, data = hsb2)
  error <- cat("Hmmm.. Looks like you have specified an incorrect model. The below steps might be helpful:\n\n* Check if you have used the glm() function to build the model.\n* If you have never used it before, you can learn more by typing ?glm or help(glm) in the Console.\n\n Please specify the model in the below format:\n\n glm( honcomp ~ female + read + science , data =  hsb2 , family = binomial(link = 'logit'))\n\n Happy modeling :)")
  expect_error(blr_confusion_matrix(model), error)
})

test_that('cutoff value is between 0 and 1', {
  error <- cat('cutoff can take on values between 0 and 1 only. You have used 1.4 , please specify a value between 0 and 1 only.')
  expect_error(blr_confusion_matrix(model, 1.4), error)
})
