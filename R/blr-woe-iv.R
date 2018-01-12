#' @importFrom rlang enquo !!
#' @importFrom ggplot2 coord_flip
#' @importFrom dplyr select rename
#' @importFrom tibble add_column
#' @title Weight of Evidence & Information Value
#' @description Weight of evidence and information value
#' @param data a tibble
#' @param predictor name of the predictor
#' @param response name of the response variable
#' @param digits number of decimal digits to round off
#' @param x an object of class \code{blr_segment_dist}
#' @param title plot title
#' @param xaxis_title x axis title
#' @param yaxis_title y axis title
#' @param bar_color color of the bar
#' @param ... other inputs
#' @return a tibble
#' @examples
#' blr_woe_iv(hsb2, female, honcomp)
#' @export
#'
blr_woe_iv <- function(data, predictor, response, digits = 2, ...)
  UseMethod('blr_woe_iv')

#' @export
#'
blr_woe_iv.default <- function(data, predictor, response, digits = 2, ...) {

  pred <- enquo(predictor)
  resp <- enquo(response)

  dat <- data %>%
    select(!!pred, !!resp)

  lev <- dat %>%
    pull(!!pred) %>%
    levels()

  f <- table(dat)
  f1 <- table(dat)

  woe_iv <- rbind(f, f1) %>%
    unique() %>%
    as_tibble() %>%
    select(no = `0`, yes = `1`) %>%
    mutate(
      total = no + yes,
      distribution = round((total / sum(total) * 100), digits = digits),
      approval = round(((yes / total) * 100), digits = digits),
      dist_yes = round(yes / sum(yes), digits = digits),
      dist_no = round(no / sum(no), digits = digits),
      woe = round(log(dist_yes / dist_no), digits = digits),
      dist_diff = dist_yes - dist_no,
      iv = round((dist_diff * woe), digits = digits)
    ) %>%
    add_column(levels = lev, .before = 1) %>%
    select(-distribution, -approval) %>%
    select(levels, `0s_count` = no, `1s_count` = yes, `0s_dist` = dist_no,
           `1s_dist` = dist_yes, woe = woe, iv = iv)

  var_name <- dat %>%
    names %>%
    extract(1)

  result <- list(woe_iv_table = woe_iv, var_name = var_name)
  class(result) <- 'blr_woe_iv'
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
plot.blr_woe_iv <- function(x, title = NA, xaxis_title = 'Levels',
                            yaxis_title = 'WoE',
                            bar_color = 'blue', ...) {

  if (is.na(title)) {
    plot_title <- x$var_name
  } else {
    plot_title <- title
  }

  x %>%
    use_series(woe_iv_table) %>%
    ggplot() +
    geom_col(aes(x = levels, y = woe), fill = bar_color) +
    coord_flip() +
    ggtitle(plot_title) + xlab(xaxis_title) + ylab(yaxis_title)

}
