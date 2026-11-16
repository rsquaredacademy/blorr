noise_data <- function() {
  set.seed(11)
  data.frame(
    y = rbinom(120, 1, 0.5),
    a = rnorm(120),
    b = rnorm(120),
    c = rnorm(120)
  )
}
