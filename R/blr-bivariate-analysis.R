#' @importFrom rlang enquo !! quos !!!
#' @importFrom purrr map_dbl
#' @title Bivariate Analysis
#' @description Bivariate analysis
#' @param data a tibble
#' @param response response variable
#' @param ... predictor variables
#' @return a tibble
#' @examples
#' blr_bivariate_analysis(hsb2, honcomp, female, prog, race, schtyp)
#'
#' @export
#'
blr_bivariate_analysis <- function(data, response, ...)
  UseMethod("blr_bivariate_analysis")

#' @rdname blr_bivariate_analysis
#' @export
#'
blr_bivariate_analysis.default <- function(data, response, ...) {

  resp <- enquo(response)
  predictors <- quos(...)

  mdata <-
    data %>%
    select(!! resp, !!! predictors)

  varnames <- names(mdata)
  resp_name <- varnames[1]
  pred_name <- varnames[-1]
  len_pred_name <- x_length(pred_name)
  result <- bivar_comp(len_pred_name, mdata, pred_name, resp_name)

  result <- tibble(
    variable = pred_name, iv = result$iv,
    likelihood_ratio = result$likelihood_ratio, df = result$df,
    pval = result$pval
  )

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
    seq_len()
}

bivar_comp <- function(len_pred_name, mdata, pred_name, resp_name) {

  ivs <- list()
  lr_ratios <- list()
  lr_dfs <- list()
  lr_pvals <- list()

  for (i in len_pred_name) {

    ivs[i] <- ivs_comp(mdata, pred_name, resp_name, i)

    model <- glm(
      as.formula(paste(resp_name, "~", pred_name[i])), data = mdata,
      family = binomial(link = "logit")
    )

    model1 <- glm(
      as.formula(paste(resp_name, "~", 1)), data = mdata,
      family = binomial(link = "logit")
    )

    lr <- blr_lr_test(model, model1)
    lr_ratios[i] <- lr_extract(lr, lr_ratio)
    lr_dfs[i] <- lr_extract(lr, d_f)
    lr_pvals[i] <- lr_extract(lr, p_value)

  }

  list(iv = map_dbl(ivs, 1),
       likelihood_ratio = map_dbl(lr_ratios, 1),
       df = map_dbl(lr_dfs, 1),
       pval = map_dbl(lr_pvals, 1))

}

ivs_comp <- function(mdata, pred_name, resp_name, i) {

  blr_woe_iv(mdata, !! sym(pred_name[i]), !! sym(resp_name)) %>%
    use_series(woe_iv_table) %>%
    pull(iv) %>%
    sum()

}

lr_extract <- function(lr, value) {

  vals <- enquo(value)

  lr %>%
    use_series(test_result) %>%
    pull(!! vals)

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
blr_segment <- function(data, response, predictor) UseMethod("blr_segment")

#' @rdname blr_segment
#' @export
#'
blr_segment.default <- function(data, response, predictor) {

  resp <- enquo(response)
  pred <- enquo(predictor)

  segment_data <-
    data %>%
    select(!! pred, !! resp) %>%
    group_by(!! pred) %>%
    summarise(n = n(), `1s` = table(!! resp)[[2]]) %>%
    mutate(
      `1s%` = round((`1s` / sum(n)), 2)
    ) %>%
    select(-n, -`1s`)

  result <- list(segment_data = segment_data)
  class(result) <- "blr_segment"
  return(result)

}

#' @export
#'
print.blr_segment <- function(x, ...) {
  print_blr_segment(x)
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
blr_twoway_segment <- function(data, response, variable_1, variable_2) UseMethod("blr_twoway_segment")

#' @rdname blr_twoway_segment
#' @export
#'
blr_twoway_segment.default <- function(data, response, variable_1, variable_2) {

  resp <- enquo(response)
  var_1 <- enquo(variable_1)
  var_2 <- enquo(variable_2)

  n <-
    data %>%
    nrow()

  dat <-
    data %>%
    filter((!! resp) == 1) %>%
    select(!! var_1, !! var_2)

  var_names <-
    dat %>%
    names()

  twoway <- dat %>%
    table() %>%
    divide_by(n)

  result <- list(twoway_segment = twoway, varnames = var_names)
  class(result) <- "blr_twoway_segment"

  return(result)

}


#' @export
#'
print.blr_twoway_segment <- function(x, ...) {
  print_blr_twoway_segment(x)
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
blr_segment_dist <- function(data, response, predictor) UseMethod("blr_segment_dist")


#' @export
#'
blr_segment_dist.default <- function(data, response, predictor) {

  resp <- enquo(response)
  pred <- enquo(predictor)

  dist_table <- segment_comp(data, pred, resp)

  var_name <-
    dist_table %>%
    names() %>%
    extract(1)

  names(dist_table)[1] <- "variable"

  result <- list(dist_table = dist_table, var_name = var_name)
  class(result) <- "blr_segment_dist"

  return(result)

}

segment_comp <- function(data, pred, resp) {

  data %>%
    select(!! pred, !! resp) %>%
    group_by(!! pred) %>%
    summarise(n = n(), `1s` = table(!! resp)[[2]]) %>%
    mutate(
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
    use_series(var_name)

  x %>%
    use_series(dist_table) %>%
    ggplot(aes(variable)) +
    geom_col(aes(y = `n%`), fill = bar_color) +
    geom_line(aes(y = `1s%`, group = 1), color = line_color) +
    xlab(xaxis_title) + ggtitle(plot_title) + ylab(yaxis_title) +
    scale_y_continuous(
      labels = scales::percent,
      sec.axis = sec_axis(
        ~. / sec_axis_scale,
        name = sec_yaxis_title,
        labels = scales::percent
      )
    )
}


secondary_axis_scale_comp <- function(x) {

  x %>%
    use_series(dist_table) %>%
    mutate(
      sec = `n%` / `1s%`
    ) %>%
    pull(sec) %>%
    min()

}
