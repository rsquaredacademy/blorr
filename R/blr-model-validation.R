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
#' @references
#' Kuhn, M. (2008). Building predictive models in R using the caret package. \emph{Journal of Statistical Software}, 28(5), 1--26.
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
  c_data <- factor(as.integer(p_data > cutoff), levels = c(0, 1))

  if (is.factor(response) && length(levels(response)) > 1) {
    ref <- factor(response, levels = levels(response))
  } else {
    ref <- factor(as.integer(event_indicator(response)), levels = c(0, 1))
  }

  out    <- table(Prediction = c_data, Reference = ref)

  a <- out[2, 2]
  b <- out[2, 1]
  c <- out[1, 2]
  d <- out[1, 1]

  safe_div     <- function(x, y) if (is.na(y) || y == 0) NA_real_ else x / y
  n            <- sum(out)
  accuracy     <- safe_div(a + d, n)
  no_inf_rate  <- max(table(ref)) / n
  sensitivity  <- safe_div(a, a + c)
  specificity  <- safe_div(d, b + d)
  prevalence   <- safe_div(a + c, n)
  detect_rate  <- safe_div(a, n)
  detect_prev  <- safe_div(a + b, n)
  bal_accuracy <- (sensitivity + specificity) / 2
  precision    <- safe_div(a, a + b)
  recall       <- safe_div(a, a + c)
  kappa        <- blr_kappa(out)

  if ((b + c) > 0) {
    mcnemar_p    <- stats::mcnemar.test(out)$p.value
    mcnemar_note <- NULL
  } else {
    mcnemar_p    <- NA_real_
    mcnemar_note <- "McNemar's test is not defined when there are no discordant predictions."
  }

  ppv <- safe_div(sensitivity * prevalence,
                  (sensitivity * prevalence) +
                    ((1 - specificity) * (1 - prevalence)))

  npv <- safe_div(specificity * (1 - prevalence),
                  ((1 - sensitivity) * prevalence) +
                    (specificity * (1 - prevalence)))

  result <- list(
    accuracy                 = accuracy,
    balanced_accuracy        = bal_accuracy,
    conf_matrix              = out,
    detection_prevalence     = detect_prev,
    detection_rate           = detect_rate,
    mcnemar_kappa            = kappa,
    mcnemar_note             = mcnemar_note,
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
  if (!is.null(x$mcnemar_note)) {
    cat("Note:", x$mcnemar_note, "\n\n")
  }
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
