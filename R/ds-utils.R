formatter_freq <- function(x) {
  return(format(as.character(x), width = 13, justify = "centre"))
}


formatter <- function(x) {
  return(format(as.character(x), width = 13, justify = "right"))
}

percent <- function(x, y) {
  out <- round((x / y) * 100, 2)
  return(out)
}


formata <- function(x, round, width, justify = "centre") {
  return(format(as.character(round(x, round)), width = width, justify = justify))
}

formatas <- function(x, round, width, justify = "centre") {
  return(format(x, width = width, justify = justify))
}

bin_size <- function(data, bins) {
  return((max(data, na.rm = TRUE) - min(data, na.rm = TRUE)) / bins)
}

intervals <- function(data, bins, na.rm = TRUE) {
  binsize <- bin_size(data, bins)
  bin <- bins - 1
  interval <- min(data)
  for (i in seq_len(bin)) {
    out <- interval[i] + binsize
    interval <- c(interval, out)
  }
  interval <- c(interval, max(data))
  return(interval)
}

freq <- function(data, bins, inta) {
  result <- c()
  for (i in seq_len(bins)) {
    k <- i + 1
    freq <- data >= inta[i] & data <= inta[k]
    out <- length(data[freq])
    result <- c(result, out)
  }
  return(result)
}

div_by <- function(x) {
  1 / x
}

standardize <- function(x, avg, stdev, p) {
  ((x - avg) / stdev) ^ p
}


sums <- function(x, q) {
  avg    <- mean(x)
  stdev  <- sd(x)
  result <- sum(sapply(x, standardize, avg, stdev, q))
  return(result)
}

md_helper <- function(x, y) {
  abs(x - y)
}

#' Standard error of mean
#'
#' Returns the standard error of mean.
#'
#' @param x A numeric vector.
#'
#' @examples
#' ds_std_error(mtcars$mpg)
#'
#' @export
#'
ds_std_error <- function(x) {
  sd(x) / (length(x) ^ 0.5)
}

uss <- function(x, y) {
  (x - y) ^ 2
}

stat_uss <- function(x) {
  sum(x ^ 2)
}


formatl <- function(x) {
  return(format(format(x, nsmall = 2), width = 20, justify = "left"))
}

formatol <- function(x, w) {
  format(as.character(x), width = w, justify = "centre")
}


formatr <- function(x, w) {
  format(rounda(x), nsmall = 2, width = w, justify = "right")
}


formatc <- function(x, w) {
  if (is.numeric(x)) {
    ret <- format(as.character(round(x, 2)), width = w, justify = "centre")
  } else {
    ret <- format(as.character(x), width = w, justify = "centre")
  }
  return(ret)
}


formatnc <- function(x, w) {
  format(format(round(x, 2), nsmall = 2), width = w, justify = "centre")
}


fs <- function() {
  x <- rep("  ")
}

formats <- function() {
  x <- rep("    ")
}

format_gap <- function(w) {
  x <- rep("", w)
}

return_pos <- function(data, number) {
  out <- c()
  for (i in seq_len(length(data))) {
    if (data[i] == number) {
      out <- c(out, i)
    }
  }
  return(out)
}

row_pct <- function(mat, tot) {
  rows <- dim(mat)[1]
  l <- length(tot)
  result <- c()
  for (i in seq_len(rows)) {
    diva <- mat[i, ] / tot[i]
    result <- rbind(result, diva)
  }
  rownames(result) <- NULL
  return(result)
}

col_pct <- function(mat, tot) {
  cols <- dim(mat)[2]
  l <- length(tot)
  result <- c()
  for (i in seq_len(cols)) {
    diva <- mat[, i] / tot[i]
    result <- cbind(result, diva)
  }
  colnames(result) <- NULL
  return(result)
}

rounda <- function(x) {
  round(x, 2)
}

l <- function(x) {
  x <- as.character(x)
  k <- grep("\\$", x)
  if (length(k) == 1) {
    temp <- strsplit(x, "\\$")
    out <- temp[[1]][2]
  } else {
    out <- x
  }
  return(out)
}

fround <- function(x) {
  format(round(x, 2), nsmall = 2)
}

seqlp <- function(mean, sd, el) {
  if (el > 4) {
    lmin <- mean - (el * sd)
    lmax <- mean + (el * sd)
  } else {
    lmin <- mean - (4 * sd)
    lmax <- mean + (4 * sd)
  }

  seq(lmin, lmax, sd)
}


xmmp <- function(mean, sd, el) {
  if (el > 4) {
    xmin <- mean - (el * sd)
    xmax <- mean + (el * sd)
  } else {
    xmin <- mean - (4 * sd)
    xmax <- mean + (4 * sd)
  }

  c(xmin, xmax)
}

seql <- function(mean, sd) {
  lmin <- mean - (5 * sd)
  lmax <- mean + (5 * sd)
  seq(lmin, lmax, sd)
}

xmm <- function(mean, sd) {
  xmin <- mean - (5 * sd)
  xmax <- mean + (5 * sd)
  c(xmin, xmax)
}


seqln <- function(mean, sd) {
  lmin <- mean - 3 * sd
  lmax <- mean + 3 * sd
  seq(lmin, lmax, sd)
}


xmn <- function(mean, sd) {
  xmin <- mean - 3 * sd
  xmax <- mean + 3 * sd
  c(xmin, xmax)
}

trimmed_mean <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- na.omit(x)
  }
  mean(x, trim = 0.05)
}

quant1 <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- na.omit(x)
  }
  quantile(x, probs = 0.25)
}

quant3 <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- na.omit(x)
  }
  quantile(x, probs = 0.75)
}

string_to_name <- function(x, index = 1) {
  rlang::sym(x$varnames[index])
}

#' @importFrom utils packageVersion menu install.packages globalVariables
check_suggests <- function(pkg) {

  pkg_flag <- tryCatch(packageVersion(pkg), error = function(e) NA)

  if (is.na(pkg_flag)) {

    msg <- message(paste0('\n', pkg, ' must be installed for this functionality.'))

    if (interactive()) {
      message(msg, "\nWould you like to install it?")
      if (menu(c("Yes", "No")) == 1) {
        install.packages(pkg)
      } else {
        stop(msg, call. = FALSE)
      }
    } else {
      stop(msg, call. = FALSE)
    }
  }

}

check_df <- function(data) {
  data_name <- deparse(substitute(data))
  if (!is.data.frame(data)) {
    stop(paste0(data_name, ' must be a `data.frame` or `tibble`.'), call. = FALSE)
  }
}

check_numeric <- function(data, var, var_name) {

  vary      <- rlang::enquo(var)
  ndata     <- dplyr::pull(data, !! vary)
  var_class <- class(ndata)

  msg <- paste0(var_name, ' is not a continuous variable. The function expects an object of type `numeric` or `integer` but ', var_name, ' is of type `', var_class, '`.')
  if (!is.numeric(ndata)) {
    stop(msg, call. = FALSE)
  }
}

check_factor <- function(data, var, var_name) {

  vary      <- rlang::enquo(var)
  fdata     <- dplyr::pull(data, !! vary)
  var_class <- class(fdata)

  msg <- paste0(var_name, ' is not a categorical variable. The function expects an object of type `factor` but ', var_name, ' is of type `', var_class, '`.')
  if (!is.factor(fdata)) {
    stop(msg, call. = FALSE)
  }
}

ds_rule <- function(text = NULL) {
  con_wid  <- options()$width
  text_len <- nchar(text) + 2
  dash_len <- (con_wid - text_len) / 2
  cat(paste(rep("-", dash_len)), ' ', text, ' ',
      paste(rep("-", dash_len)), sep = "")
}

ds_num_cols <- function(data) {
  is_num <- sapply(data, is.numeric)
  if (!any(is_num)) {
    stop("Data has no continuous variables.", call. = FALSE)
  }
  data[is_num]
}

ds_loc_prep <- function(data, vars = NULL, trim = 0.05, decimals = 2) {

  if (is.null(vars)) {
    varyable <- names(data)
  } else {
    varyable <- vars
  }

  measure <- data.frame(variable  = varyable,
                        n         = sapply(data, length),
                        missing   = sapply(data, function(x) sum(is.na(x))),
                        mean      = round(sapply(data, mean), decimals),
                        trim_mean = round(sapply(data, mean, trim), decimals),
                        median    = round(sapply(data, median), decimals),
                        mode      = round(sapply(data, ds_mode), decimals))

  result <- measure[order(measure$variable), ]
  rownames(result) <- NULL
  return(result)
}
