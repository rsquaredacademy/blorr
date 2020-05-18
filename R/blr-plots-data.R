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

  d <- gains_table$gains_table[c('sensitivity', 'specificity')]
  d$sensitivity_per   <- d$sensitivity / 100
  d$`1 - specificity` <-  1 - (d$specificity / 100)
  d1 <- data.frame(NA, NA, 0, 0)
  colnames(d1) <- c('sensitivity', 'specificity',
                  'sensitivity_per', '1 - specificity')
  rbind(d1 , d)

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

  x <- gains_table_prep(model, data, test_data) 
  y <- gains_table_modify(x, decile_count = decile_count) 
  z <- gains_table_mutate(y) 
  lorenz_plot_data(z)

}

lorenz_decile_count <- function(data) {
  round(nrow(data) / 10)
}

lorenz_plot_data <- function(gains_table) {

  d            <- gains_table[c('cum_0s_%', 'cum_1s_%')]
  d$cum_0s_per <- d$`cum_0s_%` / 100
  d$cum_1s_per <- d$`cum_1s_%` / 100
  d            <- d[c('cum_0s_per', 'cum_1s_per')]
  d1           <- data.frame(cum_0s_per = 0, cum_1s_per = 0)
  d2           <- data.frame(cum_0s_per = 1, cum_1s_per = 1)

  rbind(rbind(d1, d), d2)

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

  d <- gains_table$gains_table[c('decile', 'total', '1')]
  d$decile_mean <- d$`1` / d$total
  return(d)
    
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

  d <- gains_table$gains_table[c('total', '1')]
  d <- data.frame(lapply(d, sum))
  d$X1 / d$total

}

#' @rdname blr_prep_lchart_gmean
#' @export
#'
blr_prep_lchart_data <- function(gains_table, global_mean) {

  d <- gains_table$gains_table[c('decile', 'total', '1')]
  d$decile_mean <- d$`1` / d$total
  d$d_by_g_mean <- d$decile_mean / global_mean

  return(d)

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

  d <- gains_table$gains_table[c('cum_total_%', 'cum_1s_%', 'cum_0s_%')]
  d$cum_total_per <- d$`cum_total_%` / 100
  d$cum_1s_per    <- d$`cum_1s_%` / 100
  d$cum_0s_per    <- d$`cum_0s_%` / 100
  d <- d[c('cum_total_per', 'cum_1s_per', 'cum_0s_per')]
  rbind(data.frame(cum_total_per = 0, cum_1s_per = 0, cum_0s_per = 0), d)

}

#' @rdname blr_prep_kschart_data
#' @export
#'
blr_prep_kschart_line <- function(gains_table) {

  d <- gains_table$gains_table[c('cum_total_%', 'cum_1s_%', 'cum_0s_%', 'ks')]
  d[d$ks == max(d$ks), ] / 100

}

#' @rdname blr_prep_kschart_data
#' @export
#'
blr_prep_ksannotate_y <- function(ks_line) {

  ks_line$ann_loc    <- (ks_line$`cum_1s_%` - ks_line$`cum_0s_%`) / 2
  ks_line$ann_locate <- ks_line$`cum_0s_%` + ks_line$ann_loc
  ks_line$ann_locate

}

#' @rdname blr_prep_kschart_data
#' @export
#'
blr_prep_kschart_stat <- function(ks_line) {
  round(ks_line[[4]], 2) * 100
}

#' @rdname blr_prep_kschart_data
#' @export
#'
blr_prep_ksannotate_x <- function(ks_line) {
  ks_line[[1]] + 0.1
}

