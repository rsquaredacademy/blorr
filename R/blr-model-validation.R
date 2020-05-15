#' Confusion matrix
#'
#' Wrapper for \code{confMatrix} from the caret package.
#'
#' @param model An object of class \code{glm}.
#' @param data A \code{tibble} or a \code{data.frame}.
#' @param cutoff Cutoff for classification.
#'
#' @return Confusion matix.
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'             family = binomial(link = 'logit'))
#'
#' blr_confusion_matrix(model, cutoff = 0.4)
#'
#' @importFrom caret confusionMatrix
#' @importFrom e1071 classAgreement
#' @importFrom magrittr is_greater_than
#'
#' @family model validation techniques
#'
#' @export
#'
blr_confusion_matrix <- function(model, cutoff = 0.5, data = NULL) {

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

  confusionMatrix(data = c_data, reference = response, positive = '1')

}
