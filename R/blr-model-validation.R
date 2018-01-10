#' @title Confusion Matrix
#' @description Confusion matrix
#' @param model an object of class \code{glm}
#' @param data a tibble
#' @param cutoff cutoff for classification
#' @return table
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_confusion_matrix(model, cutoff = 0.4)
#' @export
#'
blr_confusion_matrix <- function(model, cutoff = 0.5, data = NULL) UseMethod('blr_confusion_matrix')

#' @export
#'
blr_confusion_matrix.default <- function(model, cutoff = 0.5, data = NULL) {

  if (is.null(data)) {
    data <- eval(model$call$data)
  }

  resp <- model %>%
    formula %>%
    extract2(2) %>%
    as.character

  conf_matrix <- data %>%
    mutate(
      prob = predict.glm(object = model, type = 'response'),
      predicted = if_else(prob > cutoff, 1, 0)
    ) %>%
    select(resp, predicted) %>%
    table

  a <- conf_matrix[4]
  b <- conf_matrix[3]
  c <- conf_matrix[2]
  d <- conf_matrix[1]
  abcd <- sum(a, b, c ,d)

  accuracy <- (a + d) / abcd
  precision <- a / (a + b)
  recall <- a / (a + c)
  sensitivity <- a / (a + c)
  specificity <- d / (d + b)
  prevalence <- (a + c) / abcd
  detection_rate <- a / abcd
  detection_prevalence <- (a + b) / abcd
  balanced_accuracy <- (sensitivity + specificity) / 2

  ppv <- (sensitivity * prevalence) / ((sensitivity * prevalence) +
                                         ((1 - specificity) * (1 - prevalence)))
  npv <- (specificity * (1 - prevalence)) / (((1 - sensitivity) * prevalence) +
                                               (specificity * (1 - prevalence)))


  result <- list(
    confusion_matrix = conf_matrix,
    accuracy = accuracy,
    precision = precision,
    sensitivity = sensitivity,
    specificity = specificity,
    recall = recall,
    prevalence = prevalence,
    detection_rate = detection_rate,
    detection_prevalence = detection_prevalence,
    balanced_accuracy = balanced_accuracy,
    pos_pred_value = ppv,
    neg_pred_value = npv
  )

  class(result) <- 'blr_confusion_matrix'
  return(result)

}

#' @rdname blr_confusion_matrix
#' @export
#'
print.blr_confusion_matrix <- function(x, ...) {

  r1 <- x %>%
    use_series(confusion_matrix) %>%
    rownames


  w1 <- r1 %>%
    prepend('Observed') %>%
    nchar %>%
    max

  r2 <- x %>%
    use_series(confusion_matrix) %>%
    `[`(, 1) %>%
    unname


  w2 <- r2 %>%
    prepend('0') %>%
    nchar %>%
    max

  r3 <- x %>%
    use_series(confusion_matrix) %>%
    `[`(, 2) %>%
    unname


  w3 <- r3 %>%
    prepend('1') %>%
    nchar %>%
    max

  w <- sum(w1, w2, w3, 16)
  w16 <- sum(w2, w3, 8)

  cat(f16(), fc('Predicted', w16), '\n')
  cat(rep("-", w), sep = "", '\n')
  cat(fc('Observed', w1), fs3(), fg('0', w2), fs3(), fg('1', w3), '\n')
  cat(rep("-", w), sep = "", '\n')
  for (i in seq_len(2)) {
    cat(fc(r1[i], w1), fs3(), fg(r2[i], w2), fs3(), fg(r3[i], w3), '\n')
  }
  cat(rep("-", w), sep = "", '\n\n')
  cat(fc('Model Performance Measures', 30), '\n')
  cat(rep("-", 30), sep = "", '\n')
  cat('Accuracy               ', format(round(x$accuracy, 4), nsmall = 4),
      '\nPrecision              ', format(round(x$precision, 4), nsmall = 4),
      '\nSensitivity            ', format(round(x$sensitivity, 4), nsmall = 4),
      '\nSpecificity            ', format(round(x$specificity, 4), nsmall = 4),
      '\nRecall                 ', format(round(x$recall, 4), nsmall = 4),
      '\nPrevalence             ', format(round(x$prevalence, 4), nsmall = 4),
      '\nDetection Rate         ', format(round(x$detection_rate, 4), nsmall = 4),
      '\nDetection Prevalence   ', format(round(x$detection_prevalence, 4), nsmall = 4),
      '\nBalanced Accuracy      ', format(round(x$balanced_accuracy, 4), nsmall = 4),
      '\nPos Predicted Value    ', format(round(x$pos_pred_value, 4), nsmall = 4),
      '\nNeg Predicted Value    ', format(round(x$neg_pred_value, 4), nsmall = 4))

}






