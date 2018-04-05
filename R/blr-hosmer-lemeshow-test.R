#' Hosmer lemeshow test
#'
#' Hosmer lemeshow test.
#'
#' @param model An object of class \code{glm}.
#' @param data a \code{tibble} or \code{data.frame}.
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'              family = binomial(link = 'logit'))
#'
#' blr_test_hosmer_lemeshow(model)
#'
#' @family model validation techniques
#'
#' @importFrom stats quantile
#' @importFrom dplyr case_when
#'
#' @export
#'
blr_test_hosmer_lemeshow <- function(model, data = NULL)
  UseMethod("blr_test_hosmer_lemeshow")

#' @export
#'
blr_test_hosmer_lemeshow.default <- function(model, data = NULL) {

  if (is.null(data)) {
    data <- eval(model$call$data)
    resp <- model$y 
  } else {
    namu <-
      model %>%
      formula() %>%
      extract2(2)

    resp <- 
      data %>% 
      pull(!! namu) %>%
      as.numeric()
  }

  hoslem_data <- hoslem_data_prep(model, data, resp)
  int_limits  <- hoslem_int_limits(hoslem_data)

  hoslem_table <-
    hoslem_data %>%
    hoslem_data_mutate(int_limits = int_limits) %>%
    hoslem_table_data(resp = resp)

  chisq_stat  <- hoslem_chisq_stat(hoslem_table)
  hoslem_df   <- 8
  hoslem_pval <- pchisq(chisq_stat, df = hoslem_df, lower.tail = FALSE)

  result <- list(partition_table = hoslem_table,
                 chisq_stat      = chisq_stat,
                 df              = hoslem_df,
                 pvalue          = hoslem_pval
  )

  class(result) <- "blr_test_hosmer_lemeshow"
  return(result)
}

#' @export
#'
print.blr_test_hosmer_lemeshow <- function(x, ...) {
  print_blr_test_hosmer_lemeshow(x)
}

hoslem_data_prep <- function(model, data, resp) {

  data %>%
    mutate(
      prob = predict.glm(model, newdata = data, type = "response")
    ) %>%
    add_column(resp) %>%
    arrange(prob)

}

hoslem_int_limits <- function(hoslem_data) {

  hoslem_data %>%
    use_series(prob) %>%
    quantile(probs = seq(0, 1, 0.1)) %>%
    unname()
}

hoslem_data_mutate <- function(hoslem_data, int_limits) {

  hoslem_data %>%
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

}

hoslem_table_data <- function(data,resp) {

  data %>%
    group_by(group) %>%
    summarise(
      n             = n(),
      `1s_observed` = sum(resp),
      `0s_observed` = n - `1s_observed`,
      avg_prob      = mean(prob),
      `1s_expected` = n * avg_prob,
      `0s_expected` = n - `1s_expected`,
      positive      = ((`1s_observed` - `1s_expected`) ^ 2 / `1s_expected`),
      negative      = ((`0s_observed` - `0s_expected`) ^ 2 / `0s_expected`)
    )

}

hoslem_chisq_stat <- function(hoslem_table) {

  hoslem_table %>%
    select(positive, negative) %>%
    summarise_all(sum) %>%
    sum()
}
