#' Bivariate analysis
#'
#' Information value and likelihood ratio chi square test for initial 
#'   variable/predictor selection. Currently avialable for categorical 
#'   predictors only.
#'
#' @param data A \code{tibble} or a \code{data.frame}.
#' @param response Response variable; column in \code{data}.
#' @param ... Predictor variables; columns in \code{data}.
#'
#' @return A tibble with the following columns:
#' \item{Variable}{Variable name}
#' \item{Information Value}{Information value}
#' \item{LR Chi Square}{Likelihood ratio statisitc}
#' \item{LR DF}{Likelihood ratio degrees of freedom}
#' \item{LR p-value}{Likelihood ratio p value}
#'
#' @examples
#' blr_bivariate_analysis(hsb2, honcomp, female, prog, race, schtyp)
#'
#' @importFrom rlang !! !!!
#'
#' @family bivariate analysis procedures
#'
#' @export
#'
blr_bivariate_analysis <- function(data, response, ...)
  UseMethod("blr_bivariate_analysis")

#' @rdname blr_bivariate_analysis
#' @export
#'
blr_bivariate_analysis.default <- function(data, response, ...) {

  resp <- rlang::enquo(response)
  predictors <- rlang::quos(...)

  blr_check_data(data)

  data_name <- deparse(substitute(data))
  k <- checkmate::check_choice(rlang::quo_name(resp), choices = names(data))
  
  if (k != TRUE) {

    cat("Uh oh...", crayon::bold$red(rlang::quo_name(resp)), "is not a column in", crayon::bold$blue(data_name), ". Please check the column names using: \n\n", crayon::bold$blue("* names()"), "\n", crayon::bold$blue("* colnames()"), "\n\n")

    stop("", call. = FALSE)
  }

  mdata <-
    data %>%
    dplyr::select(!! resp, !!! predictors)

  varnames      <- names(mdata)
  resp_name     <- varnames[1]
  pred_name     <- varnames[-1]
  len_pred_name <- x_length(pred_name)
  result        <- bivar_comp(len_pred_name, mdata, pred_name, resp_name)

  result <- tibble::tibble(variable         = pred_name,
                   iv               = result$iv,
                   likelihood_ratio = result$likelihood_ratio,
                   df               = result$df,
                   pval             = result$pval)

  class(result) <- "blr_bivariate_analysis"
  return(result)

}

#' @export
#'
print.blr_bivariate_analysis <- function(x, ...) {
  print_bivariate_analysis(x)
}

x_length <- function(x) {

  x %>%
    length() %>%
    seq_len(.)
}

bivar_comp <- function(len_pred_name, mdata, pred_name, resp_name) {

  ivs       <- list()
  lr_ratios <- list()
  lr_dfs    <- list()
  lr_pvals  <- list()

  for (i in len_pred_name) {

    ivs[i] <- ivs_comp(mdata, pred_name, resp_name, i)

    model <- stats::glm(
      stats::as.formula(paste(resp_name, "~", pred_name[i])), data = mdata,
      family = stats::binomial(link = "logit")
    )

    model1 <- stats::glm(
      stats::as.formula(paste(resp_name, "~", 1)), data = mdata,
      family = stats::binomial(link = "logit")
    )

    lr           <- blr_test_lr(model, model1)
    lr_ratios[i] <- lr_extract(lr, lr_ratio)
    lr_dfs[i]    <- lr_extract(lr, d_f)
    lr_pvals[i]  <- lr_extract(lr, p_value)

  }

  list(likelihood_ratio = purrr::map_dbl(lr_ratios, 1),
       iv               = purrr::map_dbl(ivs, 1),
       df               = purrr::map_dbl(lr_dfs, 1),
       pval             = purrr::map_dbl(lr_pvals, 1))

}

ivs_comp <- function(mdata, pred_name, resp_name, i) {

  blr_woe_iv(mdata, !! rlang::sym(pred_name[i]), !! rlang::sym(resp_name)) %>%
    magrittr::use_series(woe_iv_table) %>%
    dplyr::pull(iv) %>%
    sum()

}

lr_extract <- function(lr, value) {

  vals <- rlang::enquo(value)

  lr %>%
    magrittr::use_series(test_result) %>%
    dplyr::pull(!! vals)

}
#' Event rate
#'
#' Event rate by segements/levels of a qualitative variable.
#'
#' @param data A \code{tibble} or \code{data.frame}.
#' @param response Response variable; column in \code{data}.
#' @param predictor Predictor variable; column in \code{data}.
#'
#' @return A tibble.
#'
#' @examples
#' blr_segment(hsb2, honcomp, prog)
#'
#' @family bivariate analysis procedures
#'
#' @export
#'
blr_segment <- function(data, response, predictor) UseMethod("blr_segment")

#' @rdname blr_segment
#' @export
#'
blr_segment.default <- function(data, response, predictor) {

  blr_check_data(data)

  resp <- rlang::enquo(response)
  pred <- rlang::enquo(predictor)

  data_name <- deparse(substitute(data))
  k <- checkmate::check_choice(rlang::quo_name(resp), choices = names(data))
  
  if (k != TRUE) {

    cat("Uh oh...", crayon::bold$red(rlang::quo_name(resp)), "is not a column in", crayon::bold$blue(data_name), ". Please check the column names using: \n\n", crayon::bold$blue("* names()"), "\n", crayon::bold$blue("* colnames()"), "\n\n")

    stop("", call. = FALSE)
  }

  k2 <- checkmate::check_choice(rlang::quo_name(pred), choices = names(data))
  
  if (k2 != TRUE) {

    cat("Uh oh...", crayon::bold$red(rlang::quo_name(pred)), "is not a column in", crayon::bold$blue(data_name), ". Please check the column names using: \n\n", crayon::bold$blue("* names()"), "\n", crayon::bold$blue("* colnames()"), "\n\n")

    stop("", call. = FALSE)
  }

  segment_data <-
    data %>%
    dplyr::select(!! pred, !! resp) %>%
    dplyr::group_by(!! pred) %>%
    dplyr::summarise(n = dplyr::n(), `1s` = table(!! resp)[[2]]) %>%
    dplyr::mutate(
      `1s%` = round((`1s` / sum(n)), 2)
    ) %>%
    dplyr::select(-n, -`1s`)

  result <- list(segment_data = segment_data)
  class(result) <- "blr_segment"
  return(result)

}

#' @export
#'
print.blr_segment <- function(x, ...) {
  print_blr_segment(x)
}

#' Two way event rate
#'
#' Event rate across two qualitative variables.
#'
#' @param data A \code{tibble} or \code{data.frame}.
#' @param response Response variable; column in \code{data}.
#' @param variable_1 Column in \code{data}.
#' @param variable_2 Column in \code{data}.
#'
#' @return A tibble.
#'
#' @examples
#' blr_segment_twoway(hsb2, honcomp, prog, female)
#'
#' @family bivariate analysis procedures
#'
#' @export
#'
blr_segment_twoway <- function(data, response, variable_1, variable_2) UseMethod("blr_segment_twoway")

#' @rdname blr_segment_twoway
#' @export
#'
blr_segment_twoway.default <- function(data, response, variable_1, variable_2) {

  blr_check_data(data)

  resp  <- rlang::enquo(response)
  var_1 <- rlang::enquo(variable_1)
  var_2 <- rlang::enquo(variable_2)

  data_name <- deparse(substitute(data))
  k <- checkmate::check_choice(rlang::quo_name(resp), choices = names(data))
  
  if (k != TRUE) {

    cat("Uh oh...", crayon::bold$red(rlang::quo_name(resp)), "is not a column in", crayon::bold$blue(data_name), ". Please check the column names using: \n\n", crayon::bold$blue("* names()"), "\n", crayon::bold$blue("* colnames()"), "\n\n")

    stop("", call. = FALSE)
  }


  k2 <- checkmate::check_choice(rlang::quo_name(var_1), choices = names(data))
  
  if (k2 != TRUE) {

    cat("Uh oh...", crayon::bold$red(rlang::quo_name(var_1)), "is not a column in", crayon::bold$blue(data_name), ". Please check the column names using: \n\n", crayon::bold$blue("* names()"), "\n", crayon::bold$blue("* colnames()"), "\n\n")

    stop("", call. = FALSE)
  }

  k3 <- checkmate::check_choice(rlang::quo_name(var_2), choices = names(data))
  
  if (k3 != TRUE) {

    cat("Uh oh...", crayon::bold$red(rlang::quo_name(var_2)), "is not a column in", crayon::bold$blue(data_name), ". Please check the column names using: \n\n", crayon::bold$blue("* names()"), "\n", crayon::bold$blue("* colnames()"), "\n\n")

    stop("", call. = FALSE)
  }


  n <- nrow(data)

  dat <-
    data %>%
    dplyr::filter((!! resp) == 1) %>%
    dplyr::select(!! var_1, !! var_2)

  var_names <- names(dat)

  twoway <-
    dat %>%
    table() %>%
    magrittr::divide_by(n)

  result <- list(twoway_segment = twoway, varnames = var_names)
  class(result) <- "blr_segment_twoway"

  return(result)

}


#' @export
#'
print.blr_segment_twoway <- function(x, ...) {
  print_blr_twoway_segment(x)
}



#' Response distribution
#'
#' Distribution of response variable by segements/levels of a qualitative variable.
#'
#' @param data A \code{tibble} or a \code{data.frame}.
#' @param response Response variable; column in \code{data}.
#' @param predictor Predictor variable; column in \code{data}.
#' @param x An object of class \code{blr_segment_dist}.
#' @param title Plot title.
#' @param xaxis_title X axis title.
#' @param yaxis_title Y axis title.
#' @param sec_yaxis_title Secondary y axis title.
#' @param bar_color Bar color.
#' @param line_color Line color.
#' @param ... Other inputs.
#'
#' @return A tibble.
#'
#' @examples
#' k <- blr_segment_dist(hsb2, honcomp, prog)
#' k
#' 
#' # plot
#' plot(k)
#'
#'
#' @family bivariate analysis procedures
#'
#' @export
#'
blr_segment_dist <- function(data, response, predictor) UseMethod("blr_segment_dist")


#' @export
#'
blr_segment_dist.default <- function(data, response, predictor) {

  blr_check_data(data)

  resp <- rlang::enquo(response)
  pred <- rlang::enquo(predictor)

  data_name <- deparse(substitute(data))
  k <- checkmate::check_choice(rlang::quo_name(resp), choices = names(data))
  
  if (k != TRUE) {

    cat("Uh oh...", crayon::bold$red(rlang::quo_name(resp)), "is not a column in", crayon::bold$blue(data_name), ". Please check the column names using: \n\n", crayon::bold$blue("* names()"), "\n", crayon::bold$blue("* colnames()"), "\n\n")

    stop("", call. = FALSE)
  }

  k2 <- checkmate::check_choice(rlang::quo_name(pred), choices = names(data))
  
  if (k2 != TRUE) {

    cat("Uh oh...", crayon::bold$red(rlang::quo_name(pred)), "is not a column in", crayon::bold$blue(data_name), ". Please check the column names using: \n\n", crayon::bold$blue("* names()"), "\n", crayon::bold$blue("* colnames()"), "\n\n")

    stop("", call. = FALSE)
  }

  dist_table <- segment_comp(data, pred, resp)

  var_name <-
    dist_table %>%
    names() %>%
    magrittr::extract(1)

  names(dist_table)[1] <- "variable"

  result <- list(dist_table = dist_table, var_name = var_name)
  class(result) <- "blr_segment_dist"

  return(result)

}

segment_comp <- function(data, pred, resp) {

  data %>%
    dplyr::select(!! pred, !! resp) %>%
    dplyr::group_by(!! pred) %>%
    dplyr::summarise(n = dplyr::n(), `1s` = table(!! resp)[[2]]) %>%
    dplyr::mutate(
      `n%` = round((n / sum(n)), 2),
      `1s%` = round((`1s` / sum(n)), 2)
    )

}

#' @export
#'
print.blr_segment_dist <- function(x, ...) {
  print_blr_segment_dist(x)
}

#' @rdname blr_segment_dist
#' @export
#'
plot.blr_segment_dist <- function(x, title = NA, xaxis_title = "Levels",
                                  yaxis_title = "Sample Distribution",
                                  sec_yaxis_title = "1s Distribution",
                                  bar_color = "blue", line_color = "red",
                                  ...) {

  sec_axis_scale <- secondary_axis_scale_comp(x)

  if (is.na(title)) {
    plot_title <- x$var_name
  } else {
    plot_title <- title
  }

  vname <-
    x %>%
    magrittr::use_series(var_name)

  x %>%
    magrittr::use_series(dist_table) %>%
    ggplot2::ggplot(ggplot2::aes(variable)) + 
    ggplot2::geom_col(ggplot2::aes(y = `n%`), fill = bar_color) +
    ggplot2::geom_line(ggplot2::aes(y = `1s%`, group = 1), color = line_color) +
    ggplot2::xlab(xaxis_title) + 
    ggplot2::ggtitle(plot_title) + 
    ggplot2::ylab(yaxis_title) +
    ggplot2::scale_y_continuous(labels = scales::percent,
      sec.axis = ggplot2::sec_axis(~. / sec_axis_scale, name = sec_yaxis_title,
        labels = scales::percent))
}


secondary_axis_scale_comp <- function(x) {

  x %>%
    magrittr::use_series(dist_table) %>%
    dplyr::mutate(sec = `n%` / `1s%`) %>%
    dplyr::pull(sec) %>%
    min()

}
