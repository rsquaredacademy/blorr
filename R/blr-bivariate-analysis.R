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
blr_bivariate_analysis <- function(data, response, predictor) {

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

  return(result)

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
blr_segment <- function(data, response, predictor) {

  resp <- enquo(response)
  pred <- enquo(predictor)

  data %>%
    select(!!pred, !!resp) %>%
    group_by(!!pred) %>%
    summarise(n = n(), `1s` = sum(!!resp)) %>%
    mutate(
      `1s%` = round((`1s` / sum(n)), 2)
    ) %>%
    select(-n, -`1s`)
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
blr_twoway_segment <- function(data, response, variable_1, variable_2) {

  resp <- enquo(response)
  var_1 <- enquo(variable_1)
  var_2 <- enquo(variable_2)

  n <- data %>%
    nrow

  data %>%
    filter(!!resp == 1) %>%
    select(!!var_1, !!var_2) %>%
    table %>%
    divide_by(n)

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
  x %>%
    use_series(dist_table) %>%
    print
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




