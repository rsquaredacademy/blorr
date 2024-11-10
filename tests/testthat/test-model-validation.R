model <- glm(
  honcomp ~ female + read + science, data = hsb2,
  family = binomial(link = "logit")
)

test_that("prints the correct output", {

  k <- blr_confusion_matrix(model, cutoff = 0.4)
  expect_snapshot(k)
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
