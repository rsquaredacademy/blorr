#' Concordant & discordant pairs
#'
#' Association of predicted probabilities and observed responses.
#'
#' @param model An object of class \code{glm}.
#'
#' @return A tibble.
#' 
#' @references
#' Nelsen  RB  (1998):  Concordance  and  Gini’s  measure  of  association. Journal  of  Nonparametric  
#' Statistics, 9(3):227–238. 
#' 
#' Newson  R  (2006):  Confidence  intervals  for  rank  statistics:  Somers'  D  and  extensions.  
#' The  Stata Journal, 6(3):309–334. 
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
