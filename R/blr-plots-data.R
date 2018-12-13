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
    magrittr::use_series(gains_table) %>%
    dplyr::select(sensitivity, specificity) %>%
    dplyr::mutate(
      sensitivity_per   = sensitivity / 100,
      `1 - specificity` = 1 - (specificity / 100)
    ) %>%
    tibble::add_row(sensitivity_per = 0, `1 - specificity` = 0, .before = 1)

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
    magrittr::divide_by(10) %>%
    round()

}

lorenz_table_modify <- function(data, decile_count) {

  residual <-
    data %>%
    nrow() %>%
    magrittr::subtract((decile_count * 9))

  data %>%
    dplyr::select(response = value, prob = value1) %>%
    dplyr::arrange(dplyr::desc(prob)) %>%
    tibble::add_column(decile = c(rep(1:9, each = decile_count),
                          rep(10, times = residual))) %>%
    dplyr::group_by(decile) %>%
    dplyr::summarise(total = n(), `1` = table(response)[[2]])
}

lorenz_plot_data <- function(gains_table) {

  gains_table %>%
    dplyr::select(`cum_0s_%`, `cum_1s_%`) %>%
    dplyr::mutate(
      cum_0s_per    = `cum_0s_%` / 100,
      cum_1s_per    = `cum_1s_%` / 100
    ) %>%
    dplyr::select(cum_0s_per, cum_1s_per) %>%
    tibble::add_row(cum_0s_per = 0, cum_1s_per = 0, .before = 1) %>%
    tibble::add_row(cum_0s_per = 1, cum_1s_per = 1)
}

#' Decile capture rate data
#'
#' Data for generating decile capture rate.
#'
#' @param gains_table An object of clas \code{blr_gains_table}
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'              family = binomial(link = 'logit'))
#' gt <- blr_gains_table(model)
#' blr_prep_dcrate_data(gt)
#'
#' @export
#'
blr_prep_dcrate_data <- function(gains_table) {

  gains_table %>%
    magrittr::use_series(gains_table) %>%
    dplyr::select(decile, total, `1`) %>%
    dplyr::mutate(
      decile_mean = `1` / total
    )
}


#' Lift Chart data
#'
#' Data for generating lift chart.
#'
#' @param gains_table An object of clas \code{blr_gains_table}.
#' @param global_mean Overall conversion rate.
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'              family = binomial(link = 'logit'))
#' gt <- blr_gains_table(model)
#' globalmean <- blr_prep_lchart_gmean(gt)
#' blr_prep_lchart_data(gt, globalmean)
#'
#' @export
#'
blr_prep_lchart_gmean <- function(gains_table) {

  gains_table %>%
    magrittr::use_series(gains_table) %>%
    dplyr::select(total, `1`) %>%
    dplyr::summarise_all(sum) %>%
    dplyr::mutate(
      gmean = `1` / total
    ) %>%
    dplyr::pull(gmean)

}

#' @rdname blr_prep_lchart_gmean
#' @export
#'
blr_prep_lchart_data <- function(gains_table, global_mean) {

  gains_table %>%
    magrittr::use_series(gains_table) %>%
    dplyr::select(decile, total, `1`) %>%
    dplyr::mutate(
      decile_mean = `1` / total,
      d_by_g_mean = decile_mean / global_mean
    )

}

#' KS Chart data
#'
#' Data for generating KS chart.
#'
#' @param gains_table An object of clas \code{blr_gains_table}.
#' @param ks_line Overall conversion rate.
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'              family = binomial(link = 'logit'))
#' gt <- blr_gains_table(model)
#' blr_prep_kschart_data(gt)
#' ks_line <- blr_prep_kschart_line(gt)
#' blr_prep_kschart_stat(ks_line)
#' blr_prep_ksannotate_y(ks_line)
#' blr_prep_ksannotate_x(ks_line)
#'
#' @export
#'
blr_prep_kschart_data <- function(gains_table) {

  gains_table %>%
    magrittr::use_series(gains_table) %>%
    dplyr::select(`cum_total_%`, `cum_1s_%`, `cum_0s_%`) %>%
    dplyr::mutate(
      cum_total_per = `cum_total_%` / 100,
      cum_1s_per    = `cum_1s_%` / 100,
      cum_0s_per    = `cum_0s_%` / 100
    ) %>%
    dplyr::select(cum_total_per, cum_1s_per, cum_0s_per) %>%
    tibble::add_row(cum_total_per = 0, cum_1s_per = 0, cum_0s_per = 0, .before = 1)

}

#' @rdname blr_prep_kschart_data
#' @export
#'
blr_prep_kschart_line <- function(gains_table) {

  gains_table %>%
    magrittr::use_series(gains_table) %>%
    dplyr::select(`cum_total_%`, `cum_1s_%`, `cum_0s_%`, ks) %>%
    dplyr::filter(ks == max(ks)) %>%
    magrittr::divide_by(100)

}

#' @rdname blr_prep_kschart_data
#' @export
#'
blr_prep_ksannotate_y <- function(ks_line) {

  ks_line %>%
    dplyr::mutate(
      ann_loc    = (`cum_1s_%` - `cum_0s_%`) / 2,
      ann_locate = `cum_0s_%` + ann_loc
    ) %>%
    dplyr::pull(ann_locate)

}

#' @rdname blr_prep_kschart_data
#' @export
#'
blr_prep_kschart_stat <- function(ks_line) {

  ks_line %>%
    dplyr::pull(4) %>%
    round(2) %>%
    magrittr::multiply_by(100)

}

#' @rdname blr_prep_kschart_data
#' @export
#'
blr_prep_ksannotate_x <- function(ks_line) {

  ks_line %>%
    dplyr::pull(1) +
    0.1

}

