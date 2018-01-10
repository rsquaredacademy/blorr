#' @importFrom stats quantile
#' @importFrom dplyr case_when
#' @title Hosmer Lemeshow Test
#' @description Hosmer lemeshow test
#' @param model an object of class \code{glm}
#' @param data a tibble or a data.frame
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
#'              family = binomial(link = 'logit'))
#'
#' blr_hosmer_lemeshow_test(model)
#' @export
#'
blr_hosmer_lemeshow_test <- function(model, data = NULL)
  UseMethod('blr_hosmer_lemeshow_test')

#' @export
#'
blr_hosmer_lemeshow_test.default <- function(model, data = NULL) {

  if (is.null(data)) {
    data <- eval(model$call$data)
  }

  data$prob <- predict.glm(model, newdata = data, type = 'response')

  data %<>%
    arrange(prob)

  int_limits <- data %>%
    use_series(prob) %>%
    quantile(probs = seq(0, 1, 0.1)) %>%
    unname

  data %<>%
    mutate(
      group = case_when(
        prob <= int_limits[2] ~ 1,
        prob > int_limits[2] & prob <= int_limits[3] ~ 2,
        prob > int_limits[3] & prob <= int_limits[4] ~ 3,
        prob > int_limits[4] & prob <= int_limits[5] ~ 4,
        prob > int_limits[5] & prob <= int_limits[6] ~ 5,
        prob > int_limits[6] & prob <= int_limits[7] ~ 6,
        prob > int_limits[7] & prob <= int_limits[8] ~ 7,
        prob > int_limits[8] & prob <= int_limits[9] ~ 8,
        prob > int_limits[9] & prob <= int_limits[10] ~ 9,
        prob > int_limits[10] ~ 10
      )
    )

  hoslem_table <- data %>%
    group_by(group) %>%
    summarise(n = n(), `1s_observed` = sum(honcomp),
              `0s_observed` = n - `1s_observed`,
              avg_prob = mean(prob),
              `1s_expected` = n * avg_prob,
              `0s_expected` = n - `1s_expected`,
              positive = ((`1s_observed` - `1s_expected`) ^ 2 / `1s_expected`),
              negative = ((`0s_observed` - `0s_expected`) ^ 2 / `0s_expected`))


  chisq_stat <- hoslem_table %>%
    select(positive, negative) %>%
    summarise_all(sum) %>%
    sum

  hoslem_df <- 8

  hoslem_pval <- pchisq(chisq_stat, df = hoslem_df, lower.tail = FALSE)

  result <- list(partition_table = hoslem_table,
                 chisq_stat = chisq_stat, df = hoslem_df,
                 pvalue = hoslem_pval)

  class(result) <- 'blr_hosmer_lemeshow_test'
  return(result)

}

#' @rdname blr_hosmer_lemeshow_test
#' @export
#'
print.blr_hosmer_lemeshow_test <- function(x, ...) {

  w1 <- nchar('group')
  w2 <- x %>%
    use_series(partition_table) %>%
    use_series(n) %>%
    prepend('Total') %>%
    nchar %>%
    max
  w3 <- x %>%
    use_series(partition_table) %>%
    use_series(`1s_observed`) %>%
    prepend('Observed') %>%
    nchar %>%
    max
  w4 <- x %>%
    use_series(partition_table) %>%
    use_series(`1s_expected`) %>%
    round(2) %>%
    format(nsmall = 2) %>%
    prepend('Expected') %>%
    nchar %>%
    max
  w5 <- x %>%
    use_series(partition_table) %>%
    use_series(`0s_observed`) %>%
    prepend('Observed') %>%
    nchar %>%
    max
  w6 <- x %>%
    use_series(partition_table) %>%
    use_series(`0s_expected`) %>%
    round(2) %>%
    format(nsmall = 2) %>%
    prepend('Expected') %>%
    nchar %>%
    max
  w7 <- w3 + w4 + 4
  w8 <- w5 + w6 + 4
  w <- sum(w1, w2, w3, w4, w5, w6, 20)
  j <- x %>%
    use_series(partition_table)

  cat(fc('Partition for the Hosmer & Lemeshow Test', w), '\n')
  cat(rep("-", w), sep = "", '\n')
  cat(fc('     ', w1), fs(), fc('     ', w2), fs(), fc('def = 1', w7), fs(),
      fc('def = 0', w8), '\n')
  cat(fc('Group', w1), fs(), fc('Total', w2), fs(), fc('Observed', w3), fs(),
      fc('Expected', w4), fs(), fc('Observed', w5), fs(), fc('Expected', w6),
      '\n')
  cat(rep("-", w), sep = "", '\n')
  for (i in seq_len(10)) {
    cat(fc(j$group[i], w1), fs(), fc(j$n[i], w2), fs(), fc(j$`1s_observed`[i], w3), fs(),
        fc(format(round(j$`1s_expected`[i], 2), nsmall = 2), w4), fs(), fc(j$`0s_observed`[i], w5), fs(),
        fc(format(round(j$`0s_expected`[i], 2), nsmall = 2), w6), '\n')
  }
  cat(rep("-", w), sep = "", '\n\n')

  w9 <- x %>%
    use_series(chisq_stat) %>%
    round(4) %>%
    format(nsmall = 4) %>%
    prepend('Chi-Square') %>%
    nchar %>%
    max
  w10 <- 2
  w11 <- 10
  w12 <- sum(w9, w10, w11, 8)

  cat(fc('Goodness of Fit Test', w12), '\n')
  cat(rep("-", w12), sep = "", '\n')
  cat(fc('Chi-Square', w9), fs(), fc('DF', w10), fs(), fc('Pr > ChiSq', w11),
      '\n')
  cat(rep("-", w12), sep = "", '\n')
  cat(fc(format(round(x$chisq_stat, 4), nsmall = 4), w9), fs(), fc(x$df, w10),
      fs(), fc(format(round(x$pvalue, 4), nsmall = 4), w11),
      '\n')
  cat(rep("-", w12), sep = "", '\n')

}
