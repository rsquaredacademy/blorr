#' ROC curve data
#'
#' Data for generating ROC curve.
#'
#' @param gains_table An object of clas \code{blr_gains_table}
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'              family = binomial(link = 'logit'))
#' model %>%
#'   blr_gains_table() %>%
#'   blr_prep_roc_data()
#'
#' @export
#'
blr_prep_roc_data <- function(gains_table) {

  gains_table %>%
    use_series(gains_table) %>%
    select(sensitivity, specificity) %>%
    mutate(
      sensitivity_per   = sensitivity / 100,
      `1 - specificity` = 1 - (specificity / 100)
    ) %>%
    add_row(sensitivity_per = 0, `1 - specificity` = 0, .before = 1)

}
