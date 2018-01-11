#' @importFrom rlang enquo !!
#' @title Bivariate Analysis
#' @description Bivariate analysis
#' @param data a tibble
#' @param response response variable
#' @param predictor predictor variable
#' @return a tibble
#' @examples
#' blr_bivariate_analysis(hsb2, honcomp, prog)
#'
#' @export
#'
blr_bivariate_analysis <- function(data, response, predictor)
  UseMethod('blr_bivariate_analysis')

#' @rdname blr_bivariate_analysis
#' @export
#'
blr_bivariate_analysis.default <- function(data, response, predictor) {

  resp <- enquo(response)
  pred <- enquo(predictor)
  mdata <- data %>%
    select(!!resp, !!pred)
  varname <- mdata %>%
    names %>%
    extract(2)
  names(mdata) <- c('response', 'predictor')

  # information value
  iv <- blr_woe_iv(mdata, predictor, response) %>%
    use_series(woe_iv_table) %>%
    pull(iv) %>%
    sum

  # likelihood ratio test
  model <- glm(response ~ predictor, data = mdata,
               family = binomial(link = 'logit'))
  model1 <- glm(response ~ 1, data = mdata,
                family = binomial(link = 'logit'))

  lr <- blr_lr_test(model, model1)
  lr_ratio <- lr %>%
    use_series(test_result) %>%
    pull(lr_ratio)
  lr_df <- lr %>%
    use_series(test_result) %>%
    pull(d_f)
  lr_pval <- lr %>%
    use_series(test_result) %>%
    pull(p_value)

  # result
  result <- tibble(variable = varname,
                   iv = iv,
                   likelihood_ratio = lr_ratio,
                   df = lr_df,
                   pval = lr_pval)

  class(result) <- 'blr_bivariate_analysis'
  return(result)

}

#' @rdname blr_bivariate_analysis
#' @export
#'
print.blr_bivariate_analysis <- function(x, ...) {

  w1 <- max(nchar(c('Variable', x$variable)))
  w2 <- max(nchar(c('Information Value', x$iv)))
  w3 <- max(nchar(c('LR Chi Square', format(round(x$likelihood_ratio, 4),
                                          nsmall = 4))))
  w4 <- max(nchar(c('LR DF', x$df)))
  w5 <- max(nchar(c('LR p-value', format(round(x$pval, 4),
                                       nsmall = 4))))
  w <- sum(w1, w2, w3, w4, w5, 16)

  cat(fc('Bivariate Analysis', w), '\n')
  cat(rep("-", w), sep = "", '\n')
  cat(fc('Variable', w1), fs(), fc('Information Value', w2), fs(),
      fc('LR Chi Square', w3),fs(), fc('LR DF', w4), fs(),
      fc('LR p-value', w5), '\n')
  cat(rep("-", w), sep = "", '\n')
  cat(fc(x$variable, w1), fs(), fc(x$iv, w2), fs(),
      fc(format(round(x$likelihood_ratio, 4), nsmall = 4), w3), fs(),
      fc(x$df, w4), fs(),
      fc(format(round(x$pval, 4), nsmall = 4), w5), '\n')
  cat(rep("-", w), sep = "", '\n')

}

#' @title Response by Segments
#' @description Response by segements
#' @param data a tibble
#' @param response response variable
#' @param predictor predictor variable
#' @return a tibble
#' @examples
#' blr_segment(hsb2, honcomp, prog)
#' @export
#'
blr_segment <- function(data, response, predictor) UseMethod('blr_segment')

#' @rdname blr_segment
#' @export
#'
blr_segment.default <- function(data, response, predictor) {

  resp <- enquo(response)
  pred <- enquo(predictor)

  segment_data <- data %>%
    select(!!pred, !!resp) %>%
    group_by(!!pred) %>%
    summarise(n = n(), `1s` = sum(!!resp)) %>%
    mutate(
      `1s%` = round((`1s` / sum(n)), 2)
    ) %>%
    select(-n, -`1s`)

  result <- list(segment_data = segment_data)
  class(result) <- 'blr_segment'
  return(result)

}

#' @rdname blr_segment
#' @export
#'
print.blr_segment <- function(x, ...) {

  y1 <- x %>%
    use_series(segment_data) %>%
    map(as.character) %>%
    map(nchar) %>%
    map_int(max) %>%
    unname

  y2 <- x %>%
    use_series(segment_data) %>%
    names %>%
    nchar

  w <- map2_int(y1, y2, max)
  wsum <- sum(w, 11)

  rnames <- x %>%
    use_series(segment_data) %>%
    names

  dtable <- x %>%
    use_series(segment_data)

  c1 <- dtable %>%
    pull(rnames[1]) %>%
    prepend(rnames[1])

  c2 <- dtable %>%
    pull(rnames[2]) %>%
    round(2) %>%
    format(nsamll = 2) %>%
    prepend(rnames[2])

  clen <- length(c1)

  cat(fc('Event By Attributes', wsum), '\n')
  cat(rep("-", wsum), sep = "", '\n')
  for (i in seq_len(clen)) {
    cat(fc(c1[i], w[1]), fs4(), fc(c2[i], w[2]), '\n')
    if (i == 1) {
      cat(rep("-", wsum), sep = "", '\n')
    }
  }
  cat(rep("-", wsum), sep = "", '\n\n')

}

#' @title Response Distribution by Multiple Segments
#' @description Response distribution by multiple segements
#' @param data a tibble
#' @param response response variable
#' @param variable_1 column in \code{data}
#' @param variable_2 column in \code{data}
#' @return a tibble
#' @examples
#' blr_twoway_segment(hsb2, honcomp, prog, female)
#' @export
#'
blr_twoway_segment <- function(data, response, variable_1, variable_2) UseMethod('blr_twoway_segment')

#' @rdname blr_twoway_segment
#' @export
#'
blr_twoway_segment.default <- function(data, response, variable_1, variable_2) {

  resp <- enquo(response)
  var_1 <- enquo(variable_1)
  var_2 <- enquo(variable_2)

  n <- data %>%
    nrow

  twoway <- data %>%
    filter(!!resp == 1) %>%
    select(!!var_1, !!var_2) %>%
    table %>%
    divide_by(n)

  result <- list(twoway_segment = twoway)
  class(result) <- 'blr_twoway_segment'
  return(result)
}

#' @rdname blr_twoway_segment
#' @export
#'
print.blr_twoway_segment <- function(x, ...) {



}


#' @importFrom ggplot2 geom_col sec_axis
#' @title Response Distribution by Segments
#' @description Distribution of response by segements
#' @param data a tibble
#' @param response response variable
#' @param predictor predictor variable
#' @param x an object of class \code{blr_segment_dist}
#' @param title plot title
#' @param xaxis_title x axis title
#' @param yaxis_title y axis title
#' @param sec_yaxis_title secondary y axis title
#' @param bar_color color of the bar
#' @param line_color line color
#' @param ... other inputs
#' @return a tibble
#' @examples
#' k <- blr_segment_dist(hsb2, honcomp, prog)
#' plot(k)
#' @export
#'
blr_segment_dist <- function(data, response, predictor) UseMethod('blr_segment_dist')


#' @export
#'
blr_segment_dist.default <- function(data, response, predictor) {

  resp <- enquo(response)
  pred <- enquo(predictor)

  dist_table <- data %>%
    select(!!pred, !!resp) %>%
    group_by(!!pred) %>%
    summarise(n = n(), `1s` = sum(!!resp)) %>%
    mutate(
      `n%` = round((n / sum(n)), 2),
      `1s%` = round((`1s` / sum(n)), 2)
    )

  var_name <- dist_table %>%
    names %>%
    extract(1)

  names(dist_table)[1] <- 'variable'

  result <- list(dist_table = dist_table, var_name = var_name)
  class(result) <- 'blr_segment_dist'
  return(result)

}

#' @export
#'
print.blr_segment_dist <- function(x, ...) {

  y1 <- x %>%
    use_series(dist_table) %>%
    map(as.character) %>%
    map(nchar) %>%
    map_int(max) %>%
    unname

  y2 <- x %>%
    use_series(dist_table) %>%
    names %>%
    nchar

  w <- map2_int(y1, y2, max)
  wsum <- sum(w, 16)

  rnames <- x %>%
    use_series(dist_table) %>%
    names

  dtable <- x %>%
    use_series(dist_table)

  c1 <- dtable %>%
    pull(rnames[1]) %>%
    prepend(x %>%
              use_series(var_name))

  c2 <- dtable %>%
    pull(rnames[2]) %>%
    prepend(rnames[2])

  c3 <- dtable %>%
    pull(rnames[3]) %>%
    prepend(rnames[3])

  c4 <- dtable %>%
    pull(rnames[4]) %>%
    round(2) %>%
    format(nsamll = 2) %>%
    prepend(rnames[4])

  c5 <- dtable %>%
    pull(rnames[5]) %>%
    round(2) %>%
    format(nsamll = 2) %>%
    prepend(rnames[5])

  clen <- length(c1)

  cat(fc('Event Segmentation', wsum), '\n')
  cat(rep("-", wsum), sep = "", '\n')
  for (i in seq_len(clen)) {
    cat(fc(c1[i], w[1]), fs(), fc(c2[i], w[2]), fs(),
        fc(c3[i], w[3]), fs(), fc(c4[i], w[4]), fs(),
        fc(c5[i], w[5]), '\n')
    if (i == 1) {
      cat(rep("-", wsum), sep = "", '\n')
    }
  }
  cat(rep("-", wsum), sep = "", '\n\n')


}

#' @export
#' @rdname blr_segment_dist
#'
plot.blr_segment_dist <- function(x, title = NA, xaxis_title = 'Levels',
                                  yaxis_title = 'Sample Distribution',
                                  sec_yaxis_title = '1s Distribution',
                                  bar_color = 'blue', line_color = 'red',
                                  ...) {

  sec_axis_scale <- x %>%
    use_series(dist_table) %>%
    mutate(
      sec = `n%` / `1s%`
    ) %>%
    pull(sec) %>%
    min

  if (is.na(title)) {
    plot_title <- x$var_name
  } else {
    plot_title <- title
  }

  vname <- x %>%
    use_series(var_name)

  x %>%
    use_series(dist_table) %>%
    ggplot(aes(variable)) +
    geom_col(aes(y = `n%`), fill = bar_color) +
    geom_line(aes(y = `1s%`, group = 1), color = line_color) +
    xlab(xaxis_title) + ggtitle(plot_title) + ylab(yaxis_title) +
    scale_y_continuous(labels = scales::percent,
                       sec.axis = sec_axis(~./sec_axis_scale,
                                           name = sec_yaxis_title,
                                           labels = scales::percent)
                       )

}




