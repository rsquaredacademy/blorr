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
#' @importFrom rlang enquo !! quos !!!
#' @importFrom purrr map_dbl
#'
#' @family bivariate analysis procedures
#'
#' @export
#'
blr_bivariate_analysis <- function(data, response, ...) UseMethod("blr_bivariate_analysis")

#' @rdname blr_bivariate_analysis
#' @export
#'
blr_bivariate_analysis.default <- function(data, response, ...) {

  resp          <- deparse(substitute(response))
  predictors    <- vapply(substitute(...()), deparse, NA_character_)

  blr_check_data(data)

  mdata         <- data[c(resp, predictors)]
  varnames      <- names(mdata)
  resp_name     <- varnames[1]
  pred_name     <- varnames[-1]
  len_pred_name <- x_length(pred_name)
  result        <- bivar_comp(len_pred_name, mdata, pred_name, resp_name[1])

  result <- tibble(variable         = pred_name,
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
  seq_len(length(x))
}

bivar_comp <- function(len_pred_name, mdata, pred_name, resp_name) {

  ivs       <- list()
  lr_ratios <- list()
  lr_dfs    <- list()
  lr_pvals  <- list()

  for (i in len_pred_name) {

    ivs[i] <- ivs_comp(mdata, pred_name, resp_name[1], i)

    model <- glm(
      as.formula(paste(resp_name, "~", pred_name[i])), data = mdata,
      family = binomial(link = "logit")
    )

    model1 <- glm(
      as.formula(paste(resp_name, "~", 1)), data = mdata,
      family = binomial(link = "logit")
    )

    lr           <- blr_test_lr(model, model1)
    lr_ratios[i] <- lr_extract(lr, lr_ratio)
    lr_dfs[i]    <- lr_extract(lr, d_f)
    lr_pvals[i]  <- lr_extract(lr, p_value)

  }

  list(likelihood_ratio = unlist(lr_ratios),
       iv               = unlist(ivs),
       df               = unlist(lr_dfs),
       pval             = unlist(lr_pvals))

}

ivs_comp <- function(mdata, pred_name, resp_name, i) {
  sum(blr_woe_iv(mdata, pred_name[i], resp_name[1])$woe_iv_table[['iv']])
}

lr_extract <- function(lr, value) {

  vals <- deparse(substitute(value))
  lr$test_result[[vals]]

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

  resp    <- deparse(substitute(response))
  pred    <- deparse(substitute(predictor))
  d       <- data[c(resp, pred)]
  colnames(d) <- c("resp", "pred")
  d       <- data.table(d)
  d       <- d[, .(n = .N, `1s` = table(resp)[[2]]), by = pred]

  setDF(d)

  d$`1s%` <- round((d$`1s` / sum(d$n)), 2)
  d       <- d[c('pred', '1s%')]
  d       <- d[order(d$pred), ]
  result  <- list(segment_data = d)

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

  resp          <- deparse(substitute(response))
  var_1         <- deparse(substitute(variable_1))
  var_2         <- deparse(substitute(variable_2))
  n             <- nrow(data)
  d             <- data[c(resp, var_1, var_2)]
  var_names     <- names(d)
  colnames(d)   <- c("resp", "var_1", "var_2")
  d             <- d[d$resp == 1, c('var_1', 'var_2')]
  twoway        <- table(d) / n
  result        <- list(twoway_segment = twoway, varnames = var_names)
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
#' @importFrom ggplot2 geom_col sec_axis
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

  resp <- deparse(substitute(response))
  pred <- deparse(substitute(predictor))
  data_name <- deparse(substitute(data))
  k <- check_choice(resp, choices = names(data))

  if (k != TRUE) {

    cat("Uh oh...", resp, "is not a column in", data_name, ". Please check the column names using: \n\n", "* names()", "\n", "* colnames()", "\n\n")

    stop("", call. = FALSE)
  }

  k2 <- check_choice(pred, choices = names(data))

  if (k2 != TRUE) {

    cat("Uh oh...", pred, "is not a column in", data_name, ". Please check the column names using: \n\n", "* names()", "\n", "* colnames()", "\n\n")

    stop("", call. = FALSE)
  }

  dist_table <- segment_comp(data, pred, resp)
  var_name <- names(dist_table)[1]
  names(dist_table)[1] <- "variable"
  result <- list(dist_table = dist_table, var_name = var_name)
  class(result) <- "blr_segment_dist"

  return(result)

}

segment_comp <- function(data, pred, resp) {

  d <- data[c(pred, resp)]
  colnames(d) <- c("pred", "resp")
  d <- data.table(d)
  d <- d[, .(n = .N, `1s` = table(resp)[[2]]), by = pred]

  setDF(d)

  d$`n%`  <- round((d$n / sum(d$n)), 2)
  d$`1s%` <- round((d$`1s` / sum(d$n)), 2)
  d <- d[order(d$pred), ]

  return(d)

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
  vname <- x$var_name

  if (is.na(title)) {
    plot_title <- vname
  } else {
    plot_title <- title
  }


  plot_data <- x$dist_table

  p <-
    ggplot(plot_data, aes(variable)) +
    geom_col(aes(y = `n%`), fill = bar_color) +
    geom_line(aes(y = `1s%`, group = 1), color = line_color) +
    xlab(xaxis_title) + ggtitle(plot_title) + ylab(yaxis_title) +
    scale_y_continuous(labels = scales::percent,
      sec.axis = sec_axis(~. / sec_axis_scale, name = sec_yaxis_title,
        labels = scales::percent))
}


secondary_axis_scale_comp <- function(x) {

  d <- x$dist_table
  d$sec <- d$`n%` / d$`1s%`
  min(d$sec)

}
