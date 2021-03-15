#' @importFrom utils available.packages
#' @importFrom stats runif
.onAttach <- function(...) {

  if (!interactive() || runif(1) > 0.1) return()

  pkgs          <- available.packages()
  cran_version  <- package_version(pkgs["descriptr", "Version"])
  local_version <- packageVersion("descriptr")
  behind_cran   <- cran_version > local_version

  tips <- c(
    "Learn more about descriptr at https://github.com/rsquaredacademy/descriptr/.",
    "Use suppressPackageStartupMessages() to eliminate package startup messages.",
    "Need help getting started with regression models? Visit: https://www.rsquaredacademy.com",
    "Check out our interactive app for quick data exploration. Visit: https://apps.rsquaredacademy.com/."
  )

  tip <- sample(tips, 1)

  if (interactive()) {
    if (behind_cran) {
      msg <- c("A new version of descriptr is available with bug fixes and new features.")
      packageStartupMessage(msg, "\nWould you like to install it?")
      if (menu(c("Yes", "No")) == 1) {
        update.packages("descriptr")
      }
    } else {
      packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
    }
  }

}
