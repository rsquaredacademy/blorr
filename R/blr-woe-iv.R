#' WoE & IV
#'
#' Weight of evidence and information value.
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
#' @importFrom rlang enquo !!
#' @importFrom dplyr select rename
#' @importFrom tibble add_column
#' @importFrom ggplot2 geom_hline
#'
#' @family bivariate analysis procedures
#'
#' @export
#'
blr_woe_iv <- function(data, predictor, response, digits = 4, ...)
  UseMethod("blr_woe_iv")

#' @export
#'
blr_woe_iv.default <- function(data, predictor, response, digits = 4, ...) {

  blr_check_data(data)

  pred <- enquo(predictor)
  resp <- enquo(response)

  dat <-
    data %>%
    select(!! pred, !! resp)

  lev <-
    dat %>%
    pull(!! pred) %>%
    levels()

  f  <- table(dat)
  f1 <- table(dat)

  woe_iv <-
    woe_data_prep(f, f1) %>%
    woe_data_modify(lev = lev, digits = digits) %>%
    woe_data_select()

  var_name <- names(dat)[1]

  result <- list(woe_iv_table = woe_iv, var_name = var_name)
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
                            bar_color = "blue", line_color = "red", ...) {

  if (is.na(title)) {
    plot_title <- x$var_name
  } else {
    plot_title <- title
  }

  x %>%
    use_series(woe_iv_table) %>%
    ggplot() +
    geom_col(aes(x = levels, y = woe), fill = bar_color, width = 0.3) +
    geom_hline(yintercept = 0, color = line_color) +
    ggtitle(plot_title) + xlab(xaxis_title) + ylab(yaxis_title)
}


#' Multi variable WOE & IV
#'
#' Prints weight of evidence and information value for multiple variables.
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

  resp       <- enquo(response)
  predictors <- quos(...)

  dat <-
    data %>%
    select(!! resp, !!! predictors)

  varnames    <- names(dat)
  resp_name   <- varnames[1]
  pred_name   <- varnames[-1]
  l_pred_name <- length(pred_name)

  for (i in seq_len(l_pred_name)) {
    cat(crayon::bold$red$underline(paste("Variable:", pred_name[i])))
    cat("\n\n")
    k <- blr_woe_iv(data, pred_name[i], resp_name)
    print(k)
    cat("\n\n")
  }
}

woe_data_prep <- function(f, f1) {

  rbind(f, f1) %>%
    unique() %>%
    as_tibble() %>%
    select(no = `0`, yes = `1`)

}

woe_data_modify <- function(data, lev, digits) {

  data %>%
    mutate(
      total        = no + yes,
      distribution = round((total / sum(total) * 100), digits = digits),
      approval     = round(((yes / total) * 100), digits = digits),
      dist_yes     = round(yes / sum(yes), digits = digits),
      dist_no      = round(no / sum(no), digits = digits),
      woe          = round(log(dist_no / dist_yes), digits = digits),
      dist_diff    = dist_no - dist_yes,
      iv           = round((dist_diff * woe), digits = digits)
    ) %>%
    add_column(levels = lev, .before = 1)

}

woe_data_select <- function(data) {

  data %>%
    select(-distribution, -approval) %>%
    select(
      levels, `0s_count` = no, `1s_count` = yes, `0s_dist` = dist_no,
      `1s_dist` = dist_yes, woe = woe, iv = iv
    )

}
