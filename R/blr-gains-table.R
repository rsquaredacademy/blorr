#' @importFrom stats model.frame model.response predict.glm
#' @importFrom dplyr bind_cols arrange group_by summarise n summarise_all desc
#' @importFrom tibble add_column add_row
#' @importFrom ggplot2 ggplot geom_line aes ggtitle xlab ylab
#' scale_x_continuous scale_y_continuous theme element_text
#' @title Gains Table & Lift Curve
#' @description Gains table
#' @param model an object of class \code{glm}
#' @param data a tibble
#' @return a tibble
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
#'              family = binomial(link = 'logit'))
#' blr_gains_table(model, hsb2)
#' @export
#'
blr_gains_table <- function(model, data) UseMethod('blr_gains_table')

#' @export
#'
blr_gains_table.default <- function(model, data) {

  decile_count <- data %>%
    nrow %>%
    divide_by(10) %>%
    ceiling

    gains_table <- model %>%
      model.frame() %>%
      model.response() %>%
      as_tibble() %>%
      bind_cols(predict.glm(model, newdata = data, type = 'response') %>%
                  as_tibble) %>%
      rename(response = value, prob = value1) %>%
      arrange(desc(prob)) %>%
      add_column(decile = rep(1:10, each = decile_count)) %>%
      group_by(decile) %>%
      summarise(total = n(), `1` = sum(response)) %>%
      mutate(
        `0` = total - `1`,
        cum_1s = cumsum(`1`),
        cum_0s = cumsum(`0`),
        cum_total = cumsum(total),
        `cum_total_%` = (cum_total / sum(total)) * 100,
        `cum_1s_%` = (cum_1s / sum(`1`)) * 100,
        `cum_0s_%` = (cum_0s / sum(`0`)) * 100,
        ks = `cum_1s_%` - `cum_0s_%`,
        tp = cum_1s,
        tn = cum_0s[10] - cum_0s,
        fp = cum_0s,
        fn = cum_1s[10] - cum_1s,
        sensitivity = (tp / (tp + fn)) * 100,
        specificity = (tn / (tn + fp)) * 100,
        accuracy = ((tp + tn) / cum_total[10]) * 100
    )

    result <- list(gains_table = gains_table)
    class(result) <- c('blr_gains_table')
    return(result)

}

#' @export
#'
print.blr_gains_table <- function(x, ...) {
  x %>%
    use_series(gains_table) %>%
    select(-cum_1s, -cum_0s, -cum_total, -`cum_total_%`,
           -`cum_1s_%`, -`cum_0s_%`) %>%
    print
}

#' @export
#'
plot.blr_gains_table <- function(x, title = 'Lift Chart', xaxis_title = '% Population',
                                 yaxis_title = '% Cumulative 1s', diag_line_col = 'red',
                                 lift_curve_col = 'blue', plot_title_justify = 0.5, ...) {

  x %>%
    use_series(gains_table) %>%
    select(`cum_total_%`, `cum_1s_%`) %>%
    mutate(
      cum_total_per = `cum_total_%` / 100 ,
      cum_1s_per = `cum_1s_%` / 100 ,
      cum_total_y = cum_total_per
    ) %>%
    select(cum_total_per, cum_1s_per, cum_total_y) %>%
    add_row(cum_total_per = 0, cum_1s_per = 0, cum_total_y = 0, .before = 1) %>%
    ggplot() +
    geom_line(aes(x = cum_total_per, y = cum_1s_per), color = lift_curve_col) +
    geom_line(aes(x = cum_total_per, y = cum_total_y), color = diag_line_col) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    theme(
      plot.title = element_text(hjust = plot_title_justify)
    )


}

