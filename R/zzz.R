.onAttach <- function(...) {

  if (!interactive() || stats::runif(1) > 0.1) return()

  pkgs <- utils::available.packages()
  
  cran_version <- 
    pkgs %>%
    extract("blorr", "Version") %>%
    package_version()

  local_version <- utils::packageVersion("blorr")
  behind_cran <- cran_version > local_version

  tips <- c(
    "Learn more about blorr at https://github.com/rsquaredacademy/blorr/.",
    "Use suppressPackageStartupMessages() to eliminate package startup messages.",
    "Need help getting started with logisitc regression models? Visit: https://www.rsquaredacademy.com",
    "Check out our interactive apps for quick data exploration. Visit: https://apps.rsquaredacademy.com/."
  )

  tip <- sample(tips, 1)
  
  if (interactive()) {
    if (behind_cran) {
      msg <- message("A new version of blorr is available with bug fixes and new features.")
      message(msg, "\nWould you like to install it?")
      if (utils::menu(c("Yes", "No")) == 1) {
        utils::update.packages("blorr")
      } 
    } else {
      packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
    }   
  } 
}
