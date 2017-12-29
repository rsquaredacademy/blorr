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

  p_ones <- pairs %>%
    filter(response == 1)
  p_zeros <- pairs %>%
    filter(response == 0)

  somers_1 <- p_zeros %>%
    pull(1)

  n1 <- nrow(p_ones)
  n2 <- nrow(p_zeros)
  n <- n1 + n2

  pairs_count <- 0
  concordant <- 0
  discordant <- 0
  ties <- 0

  for (i in seq_len(n1)) {
    for (j in seq_len(n2)) {

      pairs_count <- pairs_count + 1

      if (p_ones[i, 2] > p_zeros[j, 2]) {
        concordant <- concordant + 1
      } else if (p_ones[i, 2] == p_zeros[j, 2]) {
        ties <- ties + 1
      } else {
        discordant <- discordant + 1
      }

    }
  }

  concordance <- concordant / pairs_count
  discordance <- discordant / pairs_count
  pairs_tied <- ties / pairs_count
  somers_d <- (concordant - discordant) / (concordant + discordant)
  gamma <- (concordant - discordant) / (pairs_count)
  tau <- (2 * (concordant - discordant)) / (n * (n - 1))

  result <- tibble(
    pairs = pairs_count,
    concordant = concordance,
    discordant = discordance,
    tied = ties,
    somers_d = somers_d,
    gamma = gamma,
    tau_a = tau
  )

  return(result)
}
