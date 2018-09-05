#' ROC curve data
#'
#' Data for generating ROC curve.
#'
#' @param gains_table An object of clas \code{blr_gains_table}
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'              family = binomial(link = 'logit'))
#' gt <- blr_gains_table(model)
#' blr_prep_roc_data(gt)
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

#' Lorenz curve data
#'
#' Data for generating Lorenz curve.
#'
#' @param model An object of class \code{glm}.
#' @param data A \code{tibble} or \code{data.frame}.
#' @param test_data Logical; \code{TRUE} if data is test data and \code{FALSE} if training data.
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'              family = binomial(link = 'logit'))
#' data <- model$data
#' blr_prep_lorenz_data(model, data, FALSE)
#'
#' @export
#'
blr_prep_lorenz_data <- function(model, data = NULL, test_data = FALSE) {

  decile_count <- lorenz_decile_count(data)

  gains_table_prep(model, data, test_data) %>%
    lorenz_table_modify(decile_count = decile_count) %>%
    gains_table_mutate() %>%
    lorenz_plot_data()

}

lorenz_decile_count <- function(data) {

  data %>%
    nrow() %>%
    divide_by(10) %>%
    round()

}

lorenz_table_modify <- function(data, decile_count) {

  residual <-
    data %>%
    nrow() %>%
    subtract((decile_count * 9))

  data %>%
    select(response = value, prob = value1) %>%
    arrange(desc(prob)) %>%
    add_column(decile = c(rep(1:9, each = decile_count),
                          rep(10, times = residual))) %>%
    group_by(decile) %>%
    summarise(total = n(), `1` = table(response)[[2]])
}

lorenz_plot_data <- function(gains_table) {

  gains_table %>%
    select(`cum_0s_%`, `cum_1s_%`) %>%
    mutate(
      cum_0s_per    = `cum_0s_%` / 100,
      cum_1s_per    = `cum_1s_%` / 100
    ) %>%
    select(cum_0s_per, cum_1s_per) %>%
    add_row(cum_0s_per = 0, cum_1s_per = 0, .before = 1) %>%
    add_row(cum_0s_per = 1, cum_1s_per = 1)
}
