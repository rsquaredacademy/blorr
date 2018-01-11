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

  y1 <- x %>%
    use_series(woe_iv_table) %>%
    map(nchar) %>%
    map_int(max)

  y2 <- x %>%
    use_series(woe_iv_table) %>%
    names %>%
    nchar

  w <- map2_int(y1, y2, max)
  wsum <- sum(w, 24)

  rnames <- x %>%
    use_series(woe_iv_table) %>%
    names

  woe_iv <- x %>%
    use_series(woe_iv_table)

  c1 <- woe_iv %>%
    pull(rnames[1]) %>%
    prepend(rnames[1])

  c2 <- woe_iv %>%
    pull(rnames[2]) %>%
    prepend(rnames[2])

  c3 <- woe_iv %>%
    pull(rnames[3]) %>%
    prepend(rnames[3])

  c4 <- woe_iv %>%
    pull(rnames[4]) %>%
    prepend(rnames[4])

  c5 <- woe_iv %>%
    pull(rnames[5]) %>%
    prepend(rnames[5])

  c6 <- woe_iv %>%
    pull(rnames[6]) %>%
    prepend(rnames[6])

  c7 <- woe_iv %>%
    pull(rnames[7]) %>%
    prepend(rnames[7])

  clen <- length(c1)

  cat(fc('Weight of Evidence', wsum), '\n')
  cat(rep("-", wsum), sep = "", '\n')
  for (i in seq_len(clen)) {
    cat(fc(c1[i], w[1]), fs(), fc(c2[i], w[2]), fs(),
        fc(c3[i], w[3]), fs(), fc(c4[i], w[4]), fs(),
        fc(c5[i], w[5]), fs(), fc(c6[i], w[6]), fs(),
        fc(c7[i], w[7]), '\n')
    if (i == 1) {
      cat(rep("-", wsum), sep = "", '\n')
    }
  }
  cat(rep("-", wsum), sep = "", '\n\n')

  l1 <- c('Variable', x$var_name) %>%
    nchar %>%
    max
  l2 <- 17
  lsum <- sum(l1, l2, 4)

  ivalue <- woe_iv %>%
    pull(iv) %>%
    sum

  cat(fc('Information Value', lsum), '\n')
  cat(rep("-", lsum), sep = "", '\n')
  cat(fc('Variable', l1), fs(), fc('Information Value', l2), '\n')
  cat(rep("-", lsum), sep = "", '\n')
  cat(fc(x$var_name, l1), fs(), fc(ivalue, l2), '\n')
  cat(rep("-", lsum), sep = "", '\n')

}

#' @export
#' @rdname blr_woe_iv
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
