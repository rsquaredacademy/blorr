#' WoE & IV
#'
#' Weight of evidence and information value.
#'
#' @details
#' Character and numeric predictors are treated as categorical and factor
#' levels without observations are dropped before the table is built. Rows with
#' a missing response or predictor (including an explicit missing factor level)
#' are excluded. If no complete observations remain, WoE/IV is an error rather
#' than an empty table with a zero total. If a level has no events or no
#' non-events, 0.5 is added to both counts for that level (Haldane-Anscombe
#' continuity correction) before the weight of evidence and information value
#' are computed.
#'
#' The response must have exactly two observed, non-missing levels. A response
#' with one observed level is an error: blorr needs both events and non-events.
#' The event is the second level of a factor or character response, \code{TRUE}
#' for a logical response and \code{1} for a numeric response. Numeric responses
#' must be coded 0/1; unused factor levels are ignored. The \code{digits}
#' argument controls the returned weight of evidence and information value
#' components and the printed values. The total information value is computed
#' from the unrounded components and rounded only once for printing.
#'
#' @param data A \code{tibble} or \code{data.frame}.
#' @param predictor Predictor variable; column in \code{data}.
#' @param response Response variable; column in \code{data} with exactly two
#'   observed, non-missing levels. The event is the second level of a factor or
#'   character response, \code{TRUE} for a logical response and \code{1} for a
#'   numeric response. Numeric responses must be coded 0/1. Unused factor
#'   levels are ignored.
#' @param digits Number of decimal digits used for the returned WoE/IV table and
#'   printed values; the total information value is rounded only at print time.
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
#' Haldane, J. B. S. (1956). The estimation and significance of the logarithm of a ratio of frequencies. \emph{Annals of Human Genetics}, 20(4), 309--311.
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

  blr_check_binary_response(factor(data[[resp]]), resp)

  if (is.symbol(substitute(predictor))) {
    pred <- deparse(substitute(predictor))
  } else {
    pred <- predictor
  }

  # Identify the event on the full response before removing incomplete rows.
  # Otherwise a factor response whose non-event class disappears with a
  # missing predictor is re-levelled by droplevels() and its remaining event
  # class becomes the non-event class. Rows with a missing response or
  # predictor (including an explicit NA factor level) are then excluded.
  event <- event_indicator(data[[resp]])
  keep  <- !is.na(event) & !is.na(data[[pred]])

  dat      <- data[keep, c(pred, resp), drop = FALSE]
  event    <- event[keep]

  if (nrow(dat) == 0L) {
    stop("No complete observations remain for predictor `", pred,
         "` and response `", resp, "`: every row has a missing response or ",
         "predictor, so WoE/IV is undefined.", call. = FALSE)
  }

  dat[[pred]] <- droplevels(factor(dat[[pred]]))
  lev      <- levels(dat[[pred]])
  d1       <- woe_data_prep(dat[[pred]], event)
  d2       <- woe_data_modify(d1, lev = lev, digits = digits)
  woe_iv   <- woe_data_select(d2)
  var_name <- names(dat)[1]
  result   <- list(woe_iv_table = woe_iv, var_name = var_name,
                   iv_total = attr(d2, "iv_total"), digits = digits)

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
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param response Response variable; column in \code{data} with exactly two
#'   observed, non-missing levels. The event is the second level of a factor or
#'   character response, \code{TRUE} for a logical response and \code{1} for a
#'   numeric response. Numeric responses must be coded 0/1. Unused factor
#'   levels are ignored.
#' @param ... Predictor variables; column in \code{data}.
#'
#' @details
#' A response with one observed level is an error: blorr needs both events and
#' non-events. Each variable's table is the same table returned by
#' \code{blr_woe_iv()}: rows with a missing response or predictor are excluded,
#' a variable with no complete observations is an error, and the total
#' information value is computed from the unrounded components and rounded only
#' once for printing.
#'
#' @references
#' Haldane, J. B. S. (1956). The estimation and significance of the logarithm of a ratio of frequencies. \emph{Annals of Human Genetics}, 20(4), 309--311.
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
  blr_check_binary_response(factor(data[[resp]]), resp)
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

woe_data_prep <- function(predictor, event) {

  data.frame(
    no  = as.vector(tapply(!event, predictor, sum)),
    yes = as.vector(tapply(event, predictor, sum))
  )

}

woe_data_modify <- function(data, lev, digits) {

  data$total        <- data$no + data$yes
  data$distribution <- (data$total / sum(data$total) * 100)
  data$approval     <- ((data$yes / data$total) * 100)

  # Haldane-Anscombe correction for levels with no events or no non-events
  no  <- data$no
  yes <- data$yes
  zero_cell <- no == 0 | yes == 0
  no[zero_cell]  <- no[zero_cell] + 0.5
  yes[zero_cell] <- yes[zero_cell] + 0.5

  # distributions, logarithms and IV are computed at full precision; values
  # are rounded only when returned so small proportions do not become zero.
  # The total IV is kept unrounded as well and is rounded once by the printer,
  # so it is not the sum of rounded components.
  data$dist_yes     <- yes / sum(yes)
  data$dist_no      <- no / sum(no)
  data$woe          <- log(data$dist_no / data$dist_yes)
  data$dist_diff    <- data$dist_no - data$dist_yes
  data$iv           <- data$dist_diff * data$woe

  iv_total <- sum(data$iv)

  round_cols <- c("distribution", "approval", "dist_yes", "dist_no", "woe", "iv")
  data[round_cols] <- lapply(data[round_cols], round, digits = digits)

  out <- cbind(data.frame(levels = lev), data)
  attr(out, "iv_total") <- iv_total
  out

}

woe_data_select <- function(data) {

  cols_return   <- c('levels', 'no', 'yes', 'dist_no', 'dist_yes', 'woe', 'iv')
  new_col_names <- c('levels', 'count_0s', 'count_1s', 'dist_0s', 'dist_1s', 'woe', 'iv')
  d             <- data[cols_return]
  colnames(d)   <- new_col_names

  return(d)

}
