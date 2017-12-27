#' @importFrom rlang enquo !!
#' @importFrom dplyr select rename
#' @importFrom tibble add_column
#' @title Weight of Evidence & Information Value
#' @description Weight of evidence and information value
#' @param data a tibble
#' @param predictor name of the predictor
#' @param response name of the response variable
#' @return a tibble
#' @export
#'
blr_woe_iv <- function(data, predictor, response) {

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
      distribution = round((total / sum(total) * 100), digits = 2),
      approval = round(((yes / total) * 100), digits = 2),
      dist_yes = round(yes / sum(yes), digits = 2),
      dist_no = round(no / sum(no), digits = 2),
      WOE = round(log(dist_yes / dist_no), digits = 2),
      dist_diff = dist_yes - dist_no,
      IV = round((dist_diff * WOE), digits = 2)
    ) %>%
    add_column(levels = lev, .before = 1)

}



