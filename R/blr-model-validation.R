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
#' blr_confusion_matrix(model, hsb2, 0.4)
#' @export
#'
blr_confusion_matrix <- function(model, data, cutoff) UseMethod('blr_confusion_matrix')

#' @export
#'
blr_confusion_matrix.default <- function(model, data, cutoff) {

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
