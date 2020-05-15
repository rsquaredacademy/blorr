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
  pairs   <- data.frame(response = resp, fit_val = fit)
  n       <- nrow(pairs)
  p_ones  <- pairs_one_zero(pairs, 1, fit_val)
  p_zeros <- pairs_one_zero(pairs, 0, fit_val)

  pairs_compute(blr_pairs_cpp(p_ones, p_zeros), n = n)

}

pairs_one_zero <- function(pairs, resp, column) {
  cols <- deparse(substitute(column))
  pairs[pairs$response == resp, ][[cols]]
}

pairs_compute <- function(compute_pairs, n) {

  compute_pairs$concordance <- compute_pairs$concordant / compute_pairs$pairs
  compute_pairs$discordance <-  compute_pairs$discordant / compute_pairs$pairs
  compute_pairs$tied        <-  compute_pairs$ties / compute_pairs$pairs
  compute_pairs$somers_d    <-  (compute_pairs$concordant - compute_pairs$discordant) / (compute_pairs$concordant + compute_pairs$discordant)
  compute_pairs$gamma       <-  (compute_pairs$concordant - compute_pairs$discordant) / (compute_pairs$pairs)
  compute_pairs$tau         <-  (2 * (compute_pairs$concordant - compute_pairs$discordant)) / (n * (n - 1))
  compute_pairs$c           <-  compute_pairs$concordance + (0.5 * compute_pairs$tied)

  cols_return <- c('pairs', 'concordance', 'discordance', 'tied', 'somers_d',
                   'gamma', 'tau', 'c')

  compute_pairs[cols_return]

}
