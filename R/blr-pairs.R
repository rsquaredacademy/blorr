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
  resp <- model %>%
    use_series(y)

  fit <- model %>%
    use_series(fitted.values)

  pairs <- tibble(response = resp, fit_val = fit)
  n <- nrow(pairs)

  p_ones <- pairs %>%
    filter(response == 1) %>%
    pull(fit_val)

  p_zeros <- pairs %>%
    filter(response == 0) %>%
    pull(fit_val)

  compute_pairs <- blr_pairs_cpp(p_ones, p_zeros)

  pairs_count <- compute_pairs[["pairs"]]
  concordant <- compute_pairs[["concordant"]]
  discordant <- compute_pairs[["discordant"]]
  ties <- compute_pairs[["ties"]]

  concordance <- concordant / pairs_count
  discordance <- discordant / pairs_count
  pairs_tied <- ties / pairs_count
  somers_d <- (concordant - discordant) / (concordant + discordant)
  gamma <- (concordant - discordant) / (pairs_count)
  tau <- (2 * (concordant - discordant)) / (n * (n - 1))
  c <- concordance + (0.5 * pairs_tied)

  result <- tibble(
    pairs = pairs_count,
    concordant = concordance,
    discordant = discordance,
    tied = pairs_tied,
    somers_d = somers_d,
    gamma = gamma,
    tau_a = tau,
    c = c
  )

  return(result)
}
