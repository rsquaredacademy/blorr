#' @importFrom caret confusionMatrix
#' @importFrom e1071 classAgreement
#' @title Confusion Matrix
#' @description Confusion matrix
#' @param model an object of class \code{glm}
#' @param data a tibble
#' @param cutoff cutoff for classification
#' @return Confusion Matrix
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_confusion_matrix(model, cutoff = 0.4)
#' @export
#'
blr_confusion_matrix <- function(model, cutoff = 0.5, data = NULL) {

  if (is.null(data)) {
    data <- eval(model$call$data)
  }
  
  response <- model %>%
    use_series(y)
  
  p_data <- predict(model, newdata = data, type = "response")

  confusionMatrix(data = as.numeric(p_data > cutoff), reference = response)

}






