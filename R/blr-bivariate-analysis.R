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
