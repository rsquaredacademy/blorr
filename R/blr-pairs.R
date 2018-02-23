#' @useDynLib blorr
#' @importFrom Rcpp sourceCpp
#' @importFrom dplyr filter
#' @title Concordant & Discordant Pairs
#' @description Association of predicted probabilities and observed responses
#' @param model an object of class \code{glm}
#' @return a tibble
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_pairs(model)
#' @export
#'
blr_pairs <- function(model) {

  resp    <- model$y
  fit     <- fitted(model)
  pairs   <- tibble(response = resp, fit_val = fit)
  n       <- nrow(pairs)
  p_ones  <- pairs_one_zero(pairs, 1, fit_val)
  p_zeros <- pairs_one_zero(pairs, 0, fit_val)

  blr_pairs_cpp(p_ones, p_zeros) %>%
    compute_pairs(n = n)

}

pairs_one_zero <- function(pairs, resp, column) {

  cols <- enquo(column)

  pairs %>%
    filter(response == resp) %>%
    pull(!! cols)
}

pairs_compute <- function(compute_pairs, n) {

  compute_pairs %>%
    mutate(
      concordance = concordant / pairs,
      discordance = discordant / pairs,
      tied        = ties / pairs,
      somers_d    = (concordant - discordant) / (concordant + discordant),
      gamma       = (concordant - discordant) / (pairs),
      tau         = (2 * (concordant - discordant)) / (n * (n - 1)),
      c           = concordance + (0.5 * pairs_tied)
    ) %>%
    as_tibble() %>%
    select(-concordant, -discordant, -ties)

}
