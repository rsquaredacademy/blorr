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
#'
#' @family model validation techniques
#'
#' @export
#'
blr_confusion_matrix <- function(model, cutoff = 0.5, data = NULL) {

  namu <-
    model %>%
    formula() %>%
    extract2(2)

  if (is.null(data)) {
    data <- eval(model$call$data)
  	response <-
  	  data %>%
  	  pull(!! namu)
  } else {
		response <-
		  data %>%
		  pull(!! namu)
  }

  p_data   <- predict(model, newdata = data, type = "response")

  confusionMatrix(data = as.numeric(p_data > cutoff), reference = response)

}
