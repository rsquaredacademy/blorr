#' Hosmer lemeshow test
#'
#' Hosmer lemeshow goodness of fit test.
#'
#' @param model An object of class \code{glm}.
#' @param data a \code{tibble} or \code{data.frame}.
#'
#' @references
#' Hosmer,  D.W.,  Jr.,  &  Lemeshow,  S.  (2000), Applied  logistic  regression(2nd  ed.).
#' New  York:  John Wiley & Sons.
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
#'
#' @export
#'
blr_test_hosmer_lemeshow <- function(model, data = NULL)
  UseMethod("blr_test_hosmer_lemeshow")

#' @export
#'
blr_test_hosmer_lemeshow.default <- function(model, data = NULL) {

  blr_check_model(model)

  if (is.null(data)) {
    resp <- model$y
    data <- model$model
  } else {
    namu <- formula(model)[[2]]
    blr_check_data(data)
    resp_temp <- data[[namu]]
    resp <- as.numeric(levels(resp_temp))[resp_temp]
  }

  hoslem_data <- hoslem_data_prep(model, data, resp)
  int_limits  <- hoslem_int_limits(hoslem_data)

  h1 <- hoslem_data_mutate(hoslem_data, int_limits = int_limits)
  hoslem_table <- hoslem_table_data(h1, resp = resp)

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

  data$prob <- predict.glm(model, newdata = data, type = "response")
  data$resp <- resp
  data[order(data$prob), ]

}

hoslem_int_limits <- function(hoslem_data) {
  unname(quantile(hoslem_data$prob, probs = seq(0, 1, 0.1)))
}

hoslem_data_mutate <- function(hoslem_data, int_limits) {

  d <- hoslem_data

  d$group <- data.table::fcase(
    d$prob <= int_limits[2], 1,
    d$prob > int_limits[2] & d$prob <= int_limits[3], 2,
    d$prob > int_limits[3] & d$prob <= int_limits[4], 3,
    d$prob > int_limits[4] & d$prob <= int_limits[5], 4,
    d$prob > int_limits[5] & d$prob <= int_limits[6], 5,
    d$prob > int_limits[6] & d$prob <= int_limits[7], 6,
    d$prob > int_limits[7] & d$prob <= int_limits[8], 7,
    d$prob > int_limits[8] & d$prob <= int_limits[9], 8,
    d$prob > int_limits[9] & d$prob <= int_limits[10], 9,
    d$prob > int_limits[10], 10
  )

  return(d)


}

hoslem_table_data <- function(data, resp) {

  d <- data.table(data)
  d <- d[, .(n = .N,
             `1s_observed` = sum(resp),
             avg_prob      = mean(prob)),
         by = group]

  d <- setDF(d)
  d$`0s_observed` <- d$n - d$`1s_observed`
  d$`1s_expected` <- d$n * d$avg_prob
  d$`0s_expected` <- d$n - d$`1s_expected`
  d$positive      <- ((d$`1s_observed` - d$`1s_expected`) ^ 2 / d$`1s_expected`)
  d$negative      <- ((d$`0s_observed` - d$`0s_expected`) ^ 2 / d$`0s_expected`)

  return(d)

}

hoslem_chisq_stat <- function(hoslem_table) {

  d <- hoslem_table[c('positive', 'negative')]
  sum(unlist((lapply(d, sum))))

}
