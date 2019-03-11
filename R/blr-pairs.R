#' Concordant & discordant pairs
#'
#' Association of predicted probabilities and observed responses.
#'
#' @param model An object of class \code{glm}.
#'
#' @return A tibble.
#' 
#' @references
#' \url{https://doi.org/10.1080/10485259808832744}
#' 
#' \url{https://doi.org/10.1177/1536867X0600600302}
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_pairs(model)
#' 
#' @useDynLib blorr, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom dplyr filter
#'
#' @family model fit statistics
#'
#' @export
#'
blr_pairs <- function(model) {

  blr_check_model(model)

  resp    <- model$y
  fit     <- fitted(model)
  pairs   <- tibble(response = resp, fit_val = fit)
  n       <- nrow(pairs)
  p_ones  <- pairs_one_zero(pairs, 1, fit_val)
  p_zeros <- pairs_one_zero(pairs, 0, fit_val)

  blr_pairs_cpp(p_ones, p_zeros) %>%
    pairs_compute(n = n)

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
      c           = concordance + (0.5 * tied)
    ) %>%
    as_tibble() %>%
    select(-concordant, -discordant, -ties)

}
