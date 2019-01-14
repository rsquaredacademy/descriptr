#' @importFrom utils available.packages
#' @importFrom stats runif
.onAttach <- function(...) {

  if (!interactive() || stats::runif(1) > 0.1) return()

  pkgs <- utils::available.packages()
  
  cran_version <- 
    pkgs %>%
    magrittr::extract("descriptr", "Version") %>%
    package_version()

  local_version <- utils::packageVersion("descriptr")
  behind_cran <- cran_version > local_version

  tips <- c(
    "Learn more about descriptr at https://github.com/rsquaredacademy/descriptr/.",
    "Use suppressPackageStartupMessages() to eliminate package startup messages.",
    "Need help getting started with regression models? Visit: https://www.rsquaredacademy.com",
    "Check out our interactive app for quick data exploration. Visit: https://apps.rsquaredacademy.com/."
  )

  tip <- sample(tips, 1)

  if (behind_cran) {
    packageStartupMessage("A new version of descriptr is available with bug fixes and new features.")
  } else {
    packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
  }   

}
