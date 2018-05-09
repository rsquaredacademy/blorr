.onAttach <- function(...) {
  if (!interactive() || stats::runif(1) > 0.1) return()

  tips <- c(
    "Learn more about blorr at https://github.com/rsquaredacademy/blorr/.",
    "Use suppressPackageStartupMessages() to eliminate package startup messages.",
    "Need help getting started with logisitc regression models? Visit: https://www.rsquaredacademy.com",
    "Check out our interactive apps for quick data exploration. Visit: https://apps.rsquaredacademy.com/."
  )

  tip <- sample(tips, 1)
  packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
}
