#' WoE & IV
#'
#' Weight of evidence and information value. Currently avialable for categorical
#'   predictors only.
#'
#' @param data A \code{tibble} or \code{data.frame}.
#' @param predictor Predictor variable; column in \code{data}.
#' @param response Response variable; column in \code{data}.
#' @param digits Number of decimal digits to round off.
#' @param x An object of class \code{blr_segment_dist}.
#' @param title Plot title.
#' @param xaxis_title X axis title.
#' @param yaxis_title Y axis title.
#' @param bar_color Color of the bar.
#' @param line_color Color of the horizontal line.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param ... Other inputs.
#'
#' @return A tibble.
#'
#' @references
#' Siddiqi  N  (2006):  Credit  Risk  Scorecards:  developing  and  implementing  intelligent
#' credit  scoring. New Jersey, Wiley.
#'
#' @examples
#' # woe and iv
#' k <- blr_woe_iv(hsb2, female, honcomp)
#' k
#'
#' # plot woe
#' plot(k)
#'
#' @importFrom ggplot2 geom_hline
#'
#' @family bivariate analysis procedures
#'
#' @export
#'
blr_woe_iv <- function(data, predictor, response, digits = 4, ...) UseMethod("blr_woe_iv")

#' @export
#'
blr_woe_iv.default <- function(data, predictor, response, digits = 4, ...) {

  blr_check_data(data)

  if (is.symbol(substitute(response))) {
    resp <- deparse(substitute(response))
  } else {
    resp <- response
  }

  if (is.symbol(substitute(predictor))) {
    pred <- deparse(substitute(predictor))
  } else {
    pred <- predictor
  }

  dat      <- data[c(pred, resp)]
  lev      <- levels(dat[[pred]])
  f        <- table(dat)
  f1       <- table(dat)
  d1       <- woe_data_prep(f, f1)
  d2       <- woe_data_modify(d1, lev = lev, digits = digits)
  woe_iv   <- woe_data_select(d2)
  var_name <- names(dat)[1]
  result   <- list(woe_iv_table = woe_iv, var_name = var_name)

  class(result) <- "blr_woe_iv"
  return(result)

}

#' @export
#'
print.blr_woe_iv <- function(x, ...) {
  print_blr_woe_iv(x)
}

#' @rdname blr_woe_iv
#' @export
#'
plot.blr_woe_iv <- function(x, title = NA, xaxis_title = "Levels",
                            yaxis_title = "WoE",
                            bar_color = "blue", line_color = "red",
                            print_plot = TRUE, ...) {

  if (is.na(title)) {
    plot_title <- x$var_name
  } else {
    plot_title <- title
  }

  p <-
    ggplot(x$woe_iv_table) +
    geom_col(aes(x = levels, y = woe), fill = bar_color, width = 0.3) +
    geom_hline(yintercept = 0, color = line_color) +
    ggtitle(plot_title) + xlab(xaxis_title) + ylab(yaxis_title)

  if (print_plot) {
    print(p)
  }

  invisible(p)
}


#' Multi variable WOE & IV
#'
#' Prints weight of evidence and information value for multiple variables.
#'   Currently avialable for categorical predictors only.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param response Response variable; column in \code{data}.
#' @param ... Predictor variables; column in \code{data}.
#'
#' @examples
#' blr_woe_iv_stats(hsb2, honcomp, prog, race, female, schtyp)
#'
#' @family bivariate analysis procedures
#'
#' @export
#'
blr_woe_iv_stats <- function(data, response, ...) {

  resp        <- deparse(substitute(response))
  predictors  <- vapply(substitute(...()), deparse, NA_character_)
  dat         <- data[c(resp, predictors)]
  varnames    <- names(dat)
  resp_name   <- varnames[1]
  pred_name   <- varnames[-1]
  l_pred_name <- length(pred_name)

  for (i in seq_len(l_pred_name)) {
    cat(paste("Variable:", pred_name[i]))
    cat("\n\n")
    k <- blr_woe_iv(data, pred_name[i], resp_name[1])
    print(k)
    cat("\n\n")
  }
}

woe_data_prep <- function(f, f1) {

  d <- data.frame(unique(rbind(f, f1)))[c('X0', 'X1')]
  colnames(d) <- c('no', 'yes')
  return(d)

}

woe_data_modify <- function(data, lev, digits) {

  data$total        <- data$no + data$yes
  data$distribution <- round((data$total / sum(data$total) * 100), digits = digits)
  data$approval     <- round(((data$yes / data$total) * 100), digits = digits)
  data$dist_yes     <- round(data$yes / sum(data$yes), digits = digits)
  data$dist_no      <- round(data$no / sum(data$no), digits = digits)
  data$woe          <- round(log(data$dist_no / data$dist_yes), digits = digits)
  data$dist_diff    <- data$dist_no - data$dist_yes
  data$iv           <- round((data$dist_diff * data$woe), digits = digits)
  cbind(data.frame(levels = lev), data)

}

woe_data_select <- function(data) {

  cols_return   <- c('levels', 'no', 'yes', 'dist_no', 'dist_yes', 'woe', 'iv')
  new_col_names <- c('levels', 'count_0s', 'count_1s', 'dist_0s', 'dist_1s', 'woe', 'iv')
  d             <- data[cols_return]
  colnames(d)   <- new_col_names

  return(d)

}
