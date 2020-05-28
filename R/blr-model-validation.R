#' Confusion matrix
#'
#' Confusion matrix and statistics.
#'
#' @param model An object of class \code{glm}.
#' @param data A \code{tibble} or a \code{data.frame}.
#' @param cutoff Cutoff for classification.
#' @param ... Other arguments.
#'
#' @return Confusion matix.
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_confusion_matrix(model, cutoff = 0.4)
#'
#' @family model validation techniques
#'
#' @export
#'
blr_confusion_matrix <- function(model, cutoff = 0.5, data = NULL, ...) UseMethod("blr_confusion_matrix")

#' @rdname blr_confusion_matrix
#' @export
#'
blr_confusion_matrix.default <- function(model, cutoff = 0.5, data = NULL, ...) {

  blr_check_model(model)
  blr_check_values(cutoff, 0, 1)

  namu <- formula(model)[[2]]

  if (is.null(data)) {
    data     <- model$model
  	response <- data[[1]]
  } else {
    blr_check_data(data)
		response <- data[[as.character(namu)]]
  }

  p_data <- predict(model, newdata = data, type = "response")
  c_data <- as.factor(as.numeric(p_data > cutoff))
  out    <- table(Prediction = c_data, Reference = response)

  a <- out[4]
  b <- out[2]
  c <- out[3]
  d <- out[1]

  accuracy     <- (a + d) / (a + b + c + d)
  no_inf_rate  <- table(response)[[1]] / sum(table(response))
  sensitivity  <- a / (a +  c)
  specificity  <- d / (b + d)
  prevalence   <- (a +  c) / (a + b + c + d)
  detect_rate  <- a / (a + b + c + d)
  detect_prev  <- (a +  b) / (a + b + c + d)
  bal_accuracy <- (sensitivity + specificity) / 2
  precision    <- a / (a + b)
  recall       <- a / (a + c)
  kappa        <- blr_kappa(out)
  mcnemar_p    <- stats::mcnemar.test(out)$p.value

  ppv <- (sensitivity * prevalence) / ((sensitivity * prevalence) +
                                         ((1 - specificity) * (1 - prevalence)))

  npv <- specificity * (1 - prevalence) / (((1 - sensitivity) * prevalence) +
                                             (specificity * (1 - prevalence)))

  result <- list(
    accuracy                 = accuracy,
    balanced_accuracy        = bal_accuracy,
    conf_matrix              = out,
    detection_prevalence     = detect_prev,
    detection_rate           = detect_rate,
    mcnemar_kappa            = kappa,
    mcnemar_test_p_val       = mcnemar_p,
    negative_predicted_value = npv,
    no_information_rate      = no_inf_rate,
    positive_predicted_value = ppv,
    precision                = precision,
    prevalence               = prevalence,
    recall                   = recall,
    sensitivity              = sensitivity,
    specificity              = specificity)

  class(result) <- "blr_confusion_matrix"
  return(result)

}

#' @export
#'
print.blr_confusion_matrix <- function(x, ...) {

  cat('Confusion Matrix and Statistics', '\n\n')
  print(x$conf_matrix)
  cat('\n\n')
  cat('                Accuracy :', format(round(x$accuracy, 4), nsmall = 4), '\n')
  cat('     No Information Rate :', format(round(x$no_information_rate, 4), nsmall = 4), '\n\n')
  cat('                   Kappa :', format(round(x$mcnemar_kappa, 4), nsmall = 4), '\n\n')
  cat("McNemars's Test P-Value  :", format(round(x$mcnemar_test_p_val, 4), nsmall = 4), '\n\n')
  cat('             Sensitivity :', format(round(x$sensitivity, 4), nsmall = 4), '\n')
  cat('             Specificity :', format(round(x$specificity, 4), nsmall = 4), '\n')
  cat('          Pos Pred Value :', format(round(x$positive_predicted_value, 4), nsmall = 4), '\n')
  cat('          Neg Pred Value :', format(round(x$negative_predicted_value, 4), nsmall = 4), '\n')
  cat('              Prevalence :', format(round(x$prevalence, 4), nsmall = 4), '\n')
  cat('          Detection Rate :', format(round(x$detection_rate, 4), nsmall = 4), '\n')
  cat('    Detection Prevalence :', format(round(x$detection_prevalence, 4), nsmall = 4), '\n')
  cat('       Balanced Accuracy :', format(round(x$balanced_accuracy, 4), nsmall = 4), '\n')
  cat('               Precision :', format(round(x$precision, 4), nsmall = 4), '\n')
  cat('                  Recall :', format(round(x$recall, 4), nsmall = 4), '\n\n')
  cat("        'Positive' Class : 1")

}

blr_kappa <- function(out) {
  agreement <- sum(diag(out)) / sum(out)
  expected  <- sum(rowSums(out) * colSums(out)) / (sum(out) ^ 2)
  (agreement - expected) / (1 - expected)
}
