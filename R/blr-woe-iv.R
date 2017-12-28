#' @importFrom rlang enquo !!
#' @importFrom dplyr select rename
#' @importFrom tibble add_column
#' @title Weight of Evidence & Information Value
#' @description Weight of evidence and information value
#' @param data a tibble
#' @param predictor name of the predictor
#' @param response name of the response variable
#' @param digits number of decimal digits to round off
#' @return a tibble
#' @examples
#' blr_woe_iv(hsb2, female, honcomp)
#' @export
#'
blr_woe_iv <- function(data, predictor, response, digits = 2) {

  pred <- enquo(predictor)
  resp <- enquo(response)

  dat <- data %>%
    select(!!pred, !!resp)

  lev <- dat %>%
    pull(!!pred) %>%
    levels()

  f <- table(dat)
  f1 <- table(dat)

  rbind(f, f1) %>%
    unique() %>%
    as_tibble() %>%
    rename("no" = `0`, "yes" = `1`) %>%
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
    rename(`0` = no, `1` = yes, dist_1 = dist_yes, dist_0 = dist_no)

}



